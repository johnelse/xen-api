(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open OUnit
open Test_common
open Test_highlevel
open Xenops_interface

let test_vm_name = "__test_vm"

type vm_config = {
	oc: (string * string) list;
	platform: (string * string) list;
	pcis: string list;
}

let basic_vm = {oc = []; platform = []; pcis = []}

let string_of_vm_config conf =
	Printf.sprintf "other_config = %s, platform = %s, pcis = %s"
		(Test_printers.(assoc_list string string) conf.oc)
		(Test_printers.(assoc_list string string) conf.platform)
		(Test_printers.(list string) conf.pcis)

let load_pcis ~__context host vm pcis =
	let gPU_group = make_gpu_group ~__context () in
	List.iter
		(fun pci ->
			let open Test_vgpu_common in
			let (_: API.ref_PGPU) =
				make_pgpu ~__context ~address:pci ~host ~gPU_group basic_gpu in
			let (_: API.ref_VGPU) =
				make_vgpu ~__context ~gPU_group ~vm_ref:self
					Xapi_vgpu_type.passthrough_gpu
			in ())
		pcis;

let load_vm_config __context conf =
	let (self: API.ref_VM) = make_vm ~__context
		~name_label:test_vm_name
		~hVM_boot_policy:"BIOS order"
		~other_config:conf.oc
		~platform:conf.platform
		()
	in
	let host = make_host ~__context () in
	load_pcis ~__context host self conf.pcis;
	let vm_record = Db.VM.get_record ~__context ~self in
	Vgpuops.create_vgpus ~__context host (self, vm_record) true;
	let flags = [
		Xapi_globs.cpu_info_vendor_key, "AuthenticAMD";
		Xapi_globs.cpu_info_features_key, "deadbeef-deadbeef";
	] in
	Db.VM.set_last_boot_CPU_flags ~__context ~self ~value:flags;
	self

let run_create_metadata ~__context =
	let localhost_uuid = Helpers.get_localhost_uuid () in
	let host = make_host ~__context ~uuid:localhost_uuid () in
	let (_: API.ref_pool) = make_pool ~__context ~master:host () in
	let vms = Db.VM.get_by_name_label ~__context ~label:test_vm_name in
	let vm = List.nth vms 0 in
	Xapi_xenops.create_metadata ~__context ~upgrade:false ~self:vm

(* Test the behaviour of the "hvm_serial" other_config/platform key. *)
module HVMSerial = Generic.Make(Generic.EncapsulateState(struct
	module Io = struct
		type input_t = vm_config
		type output_t = string option

		let string_of_input_t = string_of_vm_config
		let string_of_output_t = Test_printers.(option string)
	end

	module State = XapiDb

	let load_input __context conf =
		let (_ : API.ref_VM) = load_vm_config __context conf in ()

	let extract_output __context _ =
		let metadata = run_create_metadata ~__context in
		match metadata.Metadata.vm.Vm.ty with
		| Vm.HVM {Vm.serial = serial} -> serial
		| _ -> failwith "expected HVM metadata"

	let tests =
		[
			(* Should default to "pty" if nothing is set. *)
			(
				basic_vm,
				Some "pty"
			);
			(* other_config value should override default if no platform value. *)
			(
				{basic_vm with oc=["hvm_serial", "none"]},
				Some "none"
			);
			(* Should be able to disable serial emulation via the platform key. *)
			(
				{basic_vm with platform=["hvm_serial", "none"]},
				Some "none"
			);
			(* platform value should override other_config value. *)
			(
				{basic_vm with oc=["hvm_serial", "none"]; platform=["hvm_serial", "pty"]},
				Some "pty"
			);
			(* platform value should override other_config value. *)
			(
				{basic_vm with oc=["hvm_serial", "pty"]; platform=["hvm_serial", "none"]},
				Some "none"
			);
			(* Windows debugger redirects the serial port to tcp - this should be
			 * configurable via the other_config key. *)
			(
				{basic_vm with oc=["hvm_serial", "tcp:1.2.3.4:7001"]},
				Some "tcp:1.2.3.4:7001"
			);
			(* Windows debugger should be configurable via the platform key too. *)
			(
				{basic_vm with platform=["hvm_serial", "tcp:1.2.3.4:7001"]},
				Some "tcp:1.2.3.4:7001"
			);
			(* Windows debugger setting via the platform key should override anything
			 * set in other_config. *)
			(
				{basic_vm with oc=["hvm_serial", "none"]; platform=["hvm_serial", "tcp:1.2.3.4:7001"]},
				Some "tcp:1.2.3.4:7001"
			);
		]
end))

let vgpu_manual_setup = Xapi_globs.vgpu_manual_setup_key, "true"
let vgpu_pci_id = Xapi_globs.vgpu_pci_key, "0000:0a:00.0"
let vgpu_config =
	Xapi_globs.vgpu_config_key, "/usr/share/nvidia/vgx/grid_k100.conf"

let vgpu_platform_data = [vgpu_manual_setup; vgpu_pci_id; vgpu_config]

module VideoMode = Generic.Make(Generic.EncapsulateState(struct
	module Io = struct
		type input_t = vm_config
		type output_t = Vm.video_card

		let string_of_input_t = string_of_vm_config
		let string_of_output_t = function
			| Vm.Cirrus -> "Cirrus"
			| Vm.Standard_VGA -> "Standard_VGA"
			| Vm.Vgpu -> "Vgpu"
			| Vm.IGD_passthrough Vm.GVT_d -> "IGD_passthrough"
	end

	module State = XapiDb

	let load_input __context conf =
		let (_ : API.ref_VM) = load_vm_config __context conf in ()

	let extract_output __context _ =
		let metadata = run_create_metadata ~__context in
		match metadata.Metadata.vm.Vm.ty with
		| Vm.HVM {Vm.video = video_mode} -> video_mode
		| _ -> failwith "expected HVM metadata"

	let tests = [
		(* Default video mode should be Cirrus. *)
		basic_vm, Vm.Cirrus;
		(* Unrecognised video mode should default to Cirrus. *)
		{basic_vm with platform=["vga", "foo"]}, Vm.Cirrus;
		(* Video modes set in the platform map should be respected. *)
		{basic_vm with platform=["vga", "cirrus"]}, Vm.Cirrus;
		{basic_vm with platform=["vga", "std"]}, Vm.Standard_VGA;
		(* We should be able to enable vGPU via the manual setup mode. *)
		{basic_vm with platform=vgpu_platform_data}, Vm.Vgpu;
		(* vGPU mode should override whatever's set for the "vga" key. *)
		{basic_vm with platform=["vga", "cirrus"] @ vgpu_platform_data}, Vm.Vgpu;
		{basic_vm with platform=["vga", "std"] @ vgpu_platform_data}, Vm.Vgpu;
		(* If somehow only one of the vGPU keys is set, this shouldn't
		 * trigger vGPU mode. This should only ever happen if a user is
		 * experimenting with vgpu_manual_setup and has got things wrong. *)
		{basic_vm with platform=[vgpu_manual_setup; vgpu_pci_id]}, Vm.Cirrus;
		{
			basic_vm with
			platform=["vga", "cirrus"; vgpu_manual_setup; vgpu_pci_id]
		}, Vm.Cirrus;
		{
			basic_vm with
			platform=["vga", "std"; vgpu_manual_setup; vgpu_pci_id]
		}, Vm.Standard_VGA;
		{basic_vm with platform=[vgpu_manual_setup; vgpu_config]}, Vm.Cirrus;
		{
			basic_vm with
			platform=["vga", "cirrus"; vgpu_manual_setup; vgpu_config]
		}, Vm.Cirrus;
		{
			basic_vm with
			platform=["vga", "std"; vgpu_manual_setup; vgpu_config]
		}, Vm.Standard_VGA;
	]
end))

module VideoRam = Generic.Make(Generic.EncapsulateState(struct
	module Io = struct
		type input_t = vm_config
		type output_t = int

		let string_of_input_t = string_of_vm_config
		let string_of_output_t = Test_printers.int
	end

	module State = XapiDb

	let load_input __context conf =
		let (_ : API.ref_VM) = load_vm_config __context conf in ()

	let extract_output __context _ =
		let metadata = run_create_metadata ~__context in
		match metadata.Metadata.vm.Vm.ty with
		| Vm.HVM {Vm.video_mib = video_mib} -> video_mib
		| _ -> failwith "expected HVM metadata"

	let tests = [
		(* Video ram defaults to 4MiB. *)
		{basic_vm with platform=[]}, 4;
		(* Specifying a different amount of videoram works. *)
		{basic_vm with platform=["videoram", "8"]}, 8;
		(* Default videoram should be 16MiB for vGPU. *)
		{basic_vm with platform=vgpu_platform_data}, 16;
		(* Insufficient videoram values should be overridden for vGPU. *)
		{basic_vm with platform=vgpu_platform_data @ ["videoram", "8"]}, 16;
		(* videoram values larger than the default should be allowed for vGPU. *)
		{basic_vm with platform=vgpu_platform_data @ ["videoram", "32"]}, 32;
		(* Other VGA options shouldn't affect the videoram setting. *)
		{basic_vm with platform=["vga", "cirrus"]}, 4;
		{basic_vm with platform=["vga", "cirrus"; "videoram", "8"]}, 8;
	]
end))

module VgpuExtraArgs = Generic.Make(Generic.EncapsulateState(struct
	open Test_vgpu_common

	module Io = struct
		type input_t = vm_config
		type output_t = string

		let string_of_input_t = string_of_vm_config
		let string_of_output_t = Test_printers.string
	end

	module State = XapiDb

	let load_input __context conf =
		let pgpu_ref = make_pgpu ~__context ~address:"0000:07:00.0" default_k1 in
		let vm_ref = load_vm_config __context conf in
		let (_ : API.ref_VGPU) =
			make_vgpu ~__context ~vm_ref ~scheduled_to_be_resident_on:pgpu_ref k100 in
		()

	let extract_output __context _ =
		let metadata = run_create_metadata ~__context in
		match metadata.Metadata.vgpus with
		| [{Vgpu.implementation = Vgpu.Nvidia nvidia_vgpu}] ->
			nvidia_vgpu.Vgpu.config_file
		| _ -> assert_failure "Incorrect vGPU configuration found"

	let tests = [
		(* No vgpu_extra_args. *)
		{basic_vm with platform = []}, "/usr/share/nvidia/vgx/grid_k100.conf";
		(* One key-value pair in vgpu_extra_args. *)
		{basic_vm with platform = ["vgpu_extra_args", "foo=bar"]},
		"/usr/share/nvidia/vgx/grid_k100.conf,foo=bar";
		(* Two key-value pairs in vgpu_extra_args. *)
		{basic_vm with platform = ["vgpu_extra_args", "foo=bar,baz=123"]},
		"/usr/share/nvidia/vgx/grid_k100.conf,foo=bar,baz=123";
	]
end))

let test =
	"test_xenopsd_metadata" >:::
		[
			"test_hvm_serial" >::: HVMSerial.tests;
			"test_videomode" >::: VideoMode.tests;
			"test_videoram" >::: VideoRam.tests;
			"test_vgpu_extra_args" >::: VgpuExtraArgs.tests;
		]
