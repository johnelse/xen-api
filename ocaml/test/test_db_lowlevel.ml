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

(* If we delete a record after making a Db.get_all_records call, but before the
 * call returns, then Db.get_all_records should return successfully, and the
 * return value should include the deleted record. *)
let test_db_get_all_records_race () =
	let vm_count = 20000 in
	let __context = make_test_database () in
	let make_vms ~__context ~vm_count =
		for i = 1 to vm_count do
			let (_: API.ref_VM) = make_vm ~__context () in ()
		done
	in
	(* First test how long it takes to get_all_records. *)
	make_vms ~__context ~vm_count;
	let start_time = Oclock.gettime Oclock.monotonic in
	let _ = Db.VM.get_all_records ~__context in
	let end_time = Oclock.gettime Oclock.monotonic in
	(* These values are in nanoseconds - convert to a float, convert to seconds
	 * and divide by two. *)
	let delay =
		(Int64.to_float
			(Int64.sub end_time start_time))
		/. 2000000000.
	in
	Printf.printf "delay = %0.2f seconds\n" delay;
	(* Delete the VM at the end of the list. *)
	let vm_ref =
		let vm_refs = Db.VM.get_all ~__context in
		List.nth vm_refs vm_count
	in
	let destroyer ~__context ~delay ~vm_ref =
		Printf.printf "waiting\n%!";
		(*Thread.delay 0.1;*)
		Printf.printf "destroying VM\n%!";
		Db.VM.destroy ~__context ~self:vm_ref;
		Printf.printf "destroyed VM\n%!"
	in
	(* Kick off the thread which will destroy a VM. *)
	let destroyer_thread =
		Thread.create
			(fun (__context, delay, vm_ref) -> destroyer ~__context ~delay ~vm_ref)
			(__context, delay, vm_ref)
	in
	Printf.printf "calling get_all_records\n%!";
	let _ = Db.VM.get_all_records ~__context in
	Printf.printf "called get_all_records\n%!";
	Thread.join destroyer_thread


let test =
	"test_db_lowlevel" >:::
		[
			"test_db_get_all_records_race" >:: test_db_get_all_records_race;
		]
