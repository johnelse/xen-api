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

open Db_filter_types
open OUnit
open Test_common
open Test_vgpu_common

let license_vgpu ~__context =
	let pool = Helpers.get_pool ~__context in
	Db.Pool.set_restrictions ~__context
		~self:pool ~value:["restrict_vgpu", "false"]

let test_set_scheduled () =
	let __context = make_test_database () in
	license_vgpu ~__context;
	let master = make_host ~__context () in
	let slave = make_host ~__context () in
	let gPU_group = make_gpu_group ~__context () in
	let master_pgpu = make_pgpu ~__context ~host:master ~gPU_group default_k2 in
	let slave_pgpu = make_pgpu ~__context ~host:slave ~gPU_group default_k2 in
	let vgpu1 = make_vgpu ~__context ~gPU_group k260q in
	let vgpu2 = make_vgpu ~__context ~gPU_group k260q in
	let vgpu3 = make_vgpu ~__context ~gPU_group k260q in
	let vgpu4 = make_vgpu ~__context ~gPU_group k260q in
	let vm1 = Db.VGPU.get_VM ~__context ~self:vgpu1 in
	let vm2 = Db.VGPU.get_VM ~__context ~self:vgpu2 in
	let vm3 = Db.VGPU.get_VM ~__context ~self:vgpu3 in
	let vm4 = Db.VGPU.get_VM ~__context ~self:vgpu4 in
	Vgpuops.create_vgpus ~__context master
		(vm1, Db.VM.get_record ~__context ~self:vm1) true;
	Vgpuops.create_vgpus ~__context master
		(vm2, Db.VM.get_record ~__context ~self:vm2) true;
	Vgpuops.create_vgpus ~__context slave
		(vm3, Db.VM.get_record ~__context ~self:vm3) true;
	Vgpuops.create_vgpus ~__context slave
		(vm4, Db.VM.get_record ~__context ~self:vm4) true;
	let master_vgpus = Db.VGPU.get_records_where ~__context ~expr:(Eq
		(Field "scheduled_to_be_resident_on",
		Literal (Ref.string_of master_pgpu))) in
	let slave_vgpus = Db.VGPU.get_records_where ~__context ~expr:(Eq
		(Field "scheduled_to_be_resident_on",
		Literal (Ref.string_of slave_pgpu))) in
	assert_equal
		~msg:"Master pGPU did not have two vGPUs scheduled"
		2 (List.length master_vgpus);
	assert_equal
		~msg:"Slave pGPU did not have two vGPUs scheduled"
		2 (List.length slave_vgpus)

let test =
	"test_vgpu_reservations" >:::
		[
			"test_set_scheduled" >:: test_set_scheduled;
		]
