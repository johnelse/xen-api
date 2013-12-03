(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
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
open Test_vgpu_common

let do_metadata_export ~__context ~vms =
	Xapi_dr.create_import_objects ~__context ~vms

let do_metadata_import ~__context ~objects =
	let metadata_options = {
		Import.dry_run = false;
		Import.live = false;
	} in
	let config = {
		Import.import_type = Import.Metadata_import metadata_options;
		Import.full_restore = true;
		Import.force = false;
	} in
	let rpc = Helpers.make_rpc ~__context in
	ignore (Import.handle_all __context config rpc Ref.null objects)

let test_import_supported_vgpu_type () =
	(* Setup a database with one K1 PGPU, one VM, and a K100 VGPU connected to
	 * that VM. *)
	let __context = Test_common.make_test_database () in
	let pgpu_state = {
		supported_VGPU_types = k1_vgpu_types;
		enabled_VGPU_types = k1_vgpu_types;
		resident_VGPU_types = [];
	} in
	let (_: API.ref_PGPU) = make_pgpu ~__context pgpu_state in
	let k100_ref = Xapi_vgpu_type.find_or_create ~__context k100 in
	let vm_ref = Test_common.make_vm ~__context () in
	let vgpu_ref =
		Test_common.make_vgpu ~__context ~_type:k100_ref ~vM:vm_ref () in
	(* Create the import objects. *)
	let objects = do_metadata_export ~__context ~vms:[vm_ref] in
	(* Destroy the VM and its VGPU. *)
	Db.VGPU.destroy ~__context ~self:vgpu_ref;
	Db.VM.destroy ~__context ~self:vm_ref;
	(* Re-import the objects. *)
	do_metadata_import ~__context ~objects


let test =
	"test_import_vgpu_type" >:::
		[
			"test_import_supported_vgpu_type" >:: test_import_supported_vgpu_type;
		]
