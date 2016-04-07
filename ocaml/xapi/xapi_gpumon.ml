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

module D = Debug.Make(struct let name="xapi" end)
open D

open Fun
open Xstringext
open Threadext

let systemctl = "/usr/bin/systemctl"
let gpumon = "xcp-rrdd-gpumon"

let is_running () =
	try
		let (_: string * string) = Forkhelpers.execute_command_get_output
			systemctl ["--quiet"; "is-active"; gpumon] in
		true
	with Forkhelpers.Spawn_internal_error _ -> false

let start () =
	debug "Starting %s" gpumon;
	ignore (Forkhelpers.execute_command_get_output systemctl ["start"; gpumon])

let stop () =
	debug "Stopping %s" gpumon;
	ignore (Forkhelpers.execute_command_get_output systemctl ["stop"; gpumon])

module IntSet = Set.Make(struct type t = int let compare = compare end)
let registered_threads = ref IntSet.empty

let register_thread id =
	let state = !registered_threads in
	registered_threads := (IntSet.add id state)

let deregister_thread id =
	let state = !registered_threads in
	registered_threads := (IntSet.remove id state)

let are_threads_registered () =
	let state = !registered_threads in
	not (IntSet.is_empty state)

(* None
 * - means no threads which require gpumon to be stopped are running.
 * Some true
 * - means gpumon must be started when the last thread
 *   leaves with_gpumon_stopped.
 * Some false
 * - means gpumon should not be started when the last thread
 *   leaves with_gpumon_stopped. *)
let restart_gpumon = ref None
let m = Mutex.create ()

(* gpumon must be stopped while any thread is running the function f
 * passed to this function.
 *
 * The first thread to enter this function will stop gpumon if it is running,
 * and set the restart_gpumon flag accordingly.
 *
 * The last thread to leave this function will start gpumon, if
 * restart_gpumon is set to Some true. *)
let with_gpumon_stopped ~f =
	let thread_id = Thread.(id (self ())) in
	(* Stop gpumon if it's running, then register this thread. *)
	Mutex.execute m
		(fun () ->
			begin
				match is_running (), !restart_gpumon with
				| true, _ -> (restart_gpumon := Some true; stop ())
				| false, None -> restart_gpumon := Some false
				| false, _ -> ()
			end;
			register_thread thread_id);
	Pervasiveext.finally
		f
		(* Deregister this thread, and if there are no more threads registered,
		 * start gpumon if it was running in the first place. *)
		(fun () ->
			Mutex.execute m
				(fun () ->
					deregister_thread thread_id;
					match are_threads_registered (), !restart_gpumon with
					| true, _ -> ()
					| false, Some true -> (start (); restart_gpumon := None)
					| false, _ -> restart_gpumon := None))
