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

open Either
open OUnit
open Test_common
open Test_highlevel
open Test_vgpu_common
open Xapi_vgpu_type

module AssertVGPUTypeAllowed = Generic.Make(Generic.EncapsulateState(struct
	module Io = struct
		type input_t = (pgpu_state * vgpu_type)
		type output_t = string option

		let string_of_input_t (pgpu, vgpu_type) =
			Printf.sprintf
				"(pgpu: %s, test vgpu_type: %s)"
				(string_of_pgpu_state pgpu)
				(string_of_vgpu_type vgpu_type)

		let string_of_output_t = function
			| Some code -> Printf.sprintf "Some %s" code
			| None -> "None"
	end

	module State = XapiDb

	let load_input __context (pgpu, _) =
		let (_: API.ref_PGPU) = make_pgpu ~__context pgpu in ()

	let extract_output __context (_, vgpu_type) =
		let pgpu_ref = List.hd (Db.PGPU.get_all ~__context) in
		let vgpu_type_ref = find_or_create ~__context vgpu_type in
		try
			Xapi_pgpu_helpers.assert_VGPU_type_allowed
				~__context ~self:pgpu_ref ~vgpu_type:vgpu_type_ref;
			None
		with Api_errors.Server_error (code, _) -> Some code

	let tests = [
		(* Test that we can run a supported VGPU type on an empty PGPU, or a PGPU
		 * which has some compatible VGPUs already running. *)
		(default_k1, k100), None;
		({default_k1 with resident_VGPU_types = [k100]}, k100), None;
		(default_k2, k200), None;
		({default_k2 with resident_VGPU_types = [k200; k200]}, k200), None;
		(* Test that unsupported VGPU types are blocked. *)
		(default_k1, k200), Some Api_errors.vgpu_type_not_supported;
		(default_k2, k100), Some Api_errors.vgpu_type_not_supported;
		(* Test that disabled VGPU types are blocked. *)
		({default_k1 with enabled_VGPU_types = []}, k100),
		Some Api_errors.vgpu_type_not_enabled;
		({default_k1 with enabled_VGPU_types = []}, passthrough_gpu),
		Some Api_errors.vgpu_type_not_enabled;
		({default_k2 with enabled_VGPU_types = []}, k200),
		Some Api_errors.vgpu_type_not_enabled;
		({default_k2 with enabled_VGPU_types = []}, passthrough_gpu),
		Some Api_errors.vgpu_type_not_enabled;
		(* Test that VGPU types not compatible with those already running
		 * are blocked. *)
		({default_k1 with resident_VGPU_types = [k100]}, k140q),
		Some Api_errors.vgpu_type_not_compatible_with_running_type;
		({default_k1 with resident_VGPU_types = [k100]}, passthrough_gpu),
		Some Api_errors.vgpu_type_not_compatible_with_running_type;
		({default_k1 with resident_VGPU_types = [passthrough_gpu]}, k100),
		Some Api_errors.vgpu_type_not_compatible_with_running_type;
		({default_k2 with resident_VGPU_types = [k260q]}, k200),
		Some Api_errors.vgpu_type_not_compatible_with_running_type;
		({default_k2 with resident_VGPU_types = [k200]}, passthrough_gpu),
		Some Api_errors.vgpu_type_not_compatible_with_running_type;
		({default_k2 with resident_VGPU_types = [passthrough_gpu]}, k200),
		Some Api_errors.vgpu_type_not_compatible_with_running_type;
	]
end))

module GetRemainingCapacity = Generic.Make(Generic.EncapsulateState(struct
	module Io = struct
		type input_t = (pgpu_state * vgpu_type)
		type output_t = int64

		let string_of_input_t (pgpu, vgpu_type) =
			Printf.sprintf
				"(pgpu: %s, test vgpu_type: %s)"
				(string_of_pgpu_state pgpu)
				(string_of_vgpu_type vgpu_type)

		let string_of_output_t = Int64.to_string
	end

	module State = XapiDb

	let load_input __context (pgpu, _) =
		let (_: API.ref_PGPU) = make_pgpu ~__context pgpu in ()

	let extract_output __context (_, vgpu_type) =
		let pgpu_ref = List.hd (Db.PGPU.get_all ~__context) in
		let vgpu_type_ref = find_or_create ~__context vgpu_type in
		Xapi_pgpu_helpers.get_remaining_capacity
			~__context ~self:pgpu_ref ~vgpu_type:vgpu_type_ref

	let tests = [
		(* Test that empty PGPUs have the correct capacity for each virtual
		 * GPU type. *)
		(default_k1, k100), 8L;
		(default_k1, k140q), 4L;
		(default_k1, passthrough_gpu), 1L;
		(default_k2, k200), 8L;
		(default_k2, k240q), 4L;
		(default_k2, k260q), 2L;
		(default_k2, passthrough_gpu), 1L;
		(* Test that we can't mix VGPU types. *)
		({default_k1 with resident_VGPU_types = [passthrough_gpu]}, k100), 0L;
		({default_k1 with resident_VGPU_types = [k100]}, k140q), 0L;
		({default_k2 with resident_VGPU_types = [passthrough_gpu]}, k200), 0L;
		({default_k2 with resident_VGPU_types = [k260q]}, k200), 0L;
		(* Test that remaining capacity values in other situations are correct. *)
		({default_k1 with resident_VGPU_types = [k100; k100]}, k100), 6L;
		({default_k1 with resident_VGPU_types = [k140q; k140q]}, k140q), 2L;
		({default_k1 with resident_VGPU_types = [passthrough_gpu]}, passthrough_gpu), 0L;
		({default_k2 with resident_VGPU_types = [k200]}, k200), 7L;
		({default_k2 with resident_VGPU_types = [k240q; k240q; k240q]}, k240q), 1L;
		({default_k2 with resident_VGPU_types = [k260q]}, k260q), 1L;
		({default_k2 with resident_VGPU_types = [passthrough_gpu]}, passthrough_gpu), 0L;
		(* Test that scheduled vGPUs also affect the capacity calculations. *)
		({default_k1 with scheduled_VGPU_types = [k100]}, k100), 7L;
		({default_k1 with scheduled_VGPU_types = [k100]}, passthrough_gpu), 0L;
		({default_k1 with scheduled_VGPU_types = [passthrough_gpu]}, k100), 0L;
		({default_k1 with scheduled_VGPU_types = [passthrough_gpu]}, passthrough_gpu), 0L;
		({default_k2 with scheduled_VGPU_types = [k200; k200]}, k200), 6L;
		({default_k2 with scheduled_VGPU_types = [k200]}, passthrough_gpu), 0L;
		({default_k2 with scheduled_VGPU_types = [passthrough_gpu]}, k200), 0L;
		({default_k2 with scheduled_VGPU_types = [passthrough_gpu]}, passthrough_gpu), 0L;
		(* Test that the capacity calculations work with combinations of scheduled
		 * and resident VGPUs. *)
		(
			{default_k1 with
				resident_VGPU_types = [k100; k100];
				scheduled_VGPU_types = [k100]},
			k100
		), 5L;
		(
			{default_k2 with
				resident_VGPU_types = [k240q; k240q];
				scheduled_VGPU_types = [k240q]},
			k240q
		), 1L;
	]
end))

let test =
	"test_pgpu_helpers" >:::
		[
			"test_assert_VGPU_type_allowed" >:: AssertVGPUTypeAllowed.test;
			"test_get_remaining_capacity" >:: GetRemainingCapacity.test;
		]
