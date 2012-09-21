(*
 * Copyright (C) 2006-2012 Citrix Systems Inc.
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

open Test_common

let create_host ~__context ~name_label =
	let ref = Ref.make () in
	let uuid = Uuid.string_of_uuid (Uuid.make_uuid ()) in
	Db.Host.create ~__context ~ref ~uuid
		~name_label ~name_description:"" ~memory_overhead:0L
		~allowed_operations:[] ~current_operations:[]
		~aPI_version_major:Xapi_globs.api_version_major
		~aPI_version_minor:Xapi_globs.api_version_minor
		~aPI_version_vendor:Xapi_globs.api_version_vendor
		~aPI_version_vendor_implementation:Xapi_globs.api_version_vendor_implementation
		~enabled:true ~software_version:Xapi_globs.software_version
		~other_config:[] ~capabilities:[] ~cpu_configuration:[] ~sched_policy:""
		~supported_bootloaders:(List.map fst Xapi_globs.supported_bootloaders)
		~logging:[] ~suspend_image_sr:Ref.null ~crash_dump_sr:Ref.null
		~cpu_info:[] ~hostname:name_label ~address:"127.0.0.1" ~metrics:Ref.null
		~license_params:[] ~boot_free_mem:0L
		~ha_statefiles:[] ~ha_network_peers:[] ~blobs:[] ~tags:[]
		~external_auth_type:""
		~external_auth_service_name:""
		~external_auth_configuration:[]
		~edition:"free" ~license_server:[]
		~bios_strings:[]
		~power_on_mode:""
		~power_on_config:[]
		~local_cache_sr:Ref.null
		~chipset_info:[];
	ref

let create_pool ~__context ~master =
	let ref = Ref.make () in
	let uuid = Uuid.string_of_uuid (Uuid.make_uuid ()) in
	Db.Pool.create ~__context ~ref ~uuid
		~name_label:"pool" ~name_description:"" ~master
		~default_SR:Ref.null ~suspend_image_SR:Ref.null ~crash_dump_SR:Ref.null
		~ha_enabled:false ~ha_configuration:[] ~ha_statefiles:[]
		~ha_host_failures_to_tolerate:0L ~ha_plan_exists_for:0L ~ha_allow_overcommit:false ~ha_overcommitted:false ~blobs:[] ~tags:[] ~gui_config:[]
		~wlb_url:"" ~wlb_username:"" ~wlb_password:Ref.null ~wlb_enabled:false ~wlb_verify_cert:false
		~redo_log_enabled:false ~redo_log_vdi:Ref.null ~vswitch_controller:"" ~restrictions:[]
		~other_config:[];
	ref

let create_sr ~__context ~name_label ~_type =
	let ref = Ref.make () in
	let uuid = Uuid.string_of_uuid (Uuid.make_uuid ()) in
	Db.SR.create ~__context ~ref ~uuid
		~name_label ~name_description:""
		~allowed_operations:[] ~current_operations:[]
		~virtual_allocation:0L
		~physical_utilisation: (-1L)
		~physical_size: (-1L)
		~content_type:"dummy"
		~_type ~shared:true ~other_config:[] ~default_vdi_visibility:true
		~sm_config:[] ~blobs:[] ~tags:[] ~local_cache_enabled:false
		~introduced_by:Ref.null;
	ref

let create_pbd ~__context ~host ~sr =
	let ref = Ref.make () in
	let uuid = Uuid.string_of_uuid (Uuid.make_uuid ()) in
	Db.PBD.create ~__context ~ref ~uuid ~host ~sR:sr
		~device_config:[] ~currently_attached:true ~other_config:[];
	ref

let create_vm ~__context ~power_state ~resident_on ~name_label =
	let ref = Ref.make () in
	let uuid = Uuid.string_of_uuid (Uuid.make_uuid ()) in
	Db.VM.create ~__context ~ref ~uuid
		~power_state ~allowed_operations:[]
		~current_operations:[]
		~blocked_operations:[]
		~name_label ~name_description:""
		~user_version:1L ~is_a_template:false
		~transportable_snapshot_id:""
		~is_a_snapshot:false ~snapshot_time:Date.never ~snapshot_of:Ref.null
		~parent:Ref.null
		~snapshot_info:[] ~snapshot_metadata:""
		~resident_on ~scheduled_to_be_resident_on:Ref.null ~affinity:Ref.null
		~memory_overhead:0L
		~memory_static_max:0L
		~memory_dynamic_max:0L
		~memory_target:0L
		~memory_dynamic_min:0L
		~memory_static_min:0L
		~vCPUs_params:[]
		~vCPUs_at_startup:0L ~vCPUs_max:0L
		~actions_after_shutdown:`destroy ~actions_after_reboot:`restart
		~actions_after_crash:`destroy
		~hVM_boot_policy:"" ~hVM_boot_params:[] ~hVM_shadow_multiplier:1.0
		~suspend_VDI:Ref.null
		~platform:[]
		~pV_kernel:"" ~pV_ramdisk:"" ~pV_args:"" ~pV_bootloader:"" ~pV_bootloader_args:""
		~pV_legacy_args:""
		~pCI_bus:"" ~other_config:[] ~domid:(-1L) ~domarch:""
		~last_boot_CPU_flags:[]
		~is_control_domain:false
		~metrics:Ref.null ~guest_metrics:Ref.null
		~last_booted_record:"" ~xenstore_data:[] ~recommendations:""
		~blobs:[]
		~ha_restart_priority:""
		~ha_always_run:false ~tags:[]
		~bios_strings:[]
		~protection_policy:Ref.null
		~is_snapshot_from_vmpp:false 
		~appliance:Ref.null
		~start_delay:0L
		~shutdown_delay:0L
		~order:0L
		~suspend_SR:Ref.null
		~version:0L;
	ref

let create_vdi ~__context ~sr ~name_label =
	let ref = Ref.make () in
	let uuid = Uuid.string_of_uuid (Uuid.make_uuid ()) in
	Db.VDI.create ~__context ~ref ~uuid
		~name_label ~name_description:""
		~current_operations:[] ~allowed_operations:[]
		~is_a_snapshot:false
		~snapshot_of:Ref.null
		~snapshot_time:Date.never
		~sR:sr ~virtual_size:0L
		~physical_utilisation:0L
		~_type:`user
		~sharable:false ~read_only:false
		~xenstore_data:[] ~sm_config:[]
		~other_config:[] ~storage_lock:false ~location:""
		~managed:true ~missing:false ~parent:Ref.null ~tags:[]
		~on_boot:`persist ~allow_caching:false
		~metadata_of_pool:Ref.null
		~metadata_latest:false;
	ref

let create_multiple ~create_one ~count =
	let things = ref [] in
	for n = 1 to count do
		let thing = create_one n in
		things := thing :: !things
	done;
	!things

let create_vms_on_host ~__context ~host ~count =
	let host_name_label = Db.Host.get_name_label ~__context ~self:host in
	create_multiple
		~create_one:(fun n -> create_vm ~__context ~power_state:`Running ~resident_on:host
			~name_label:(Printf.sprintf "%s_%d" host_name_label n))
		~count

let time_it name f =
	print_endline (Printf.sprintf "Starting task '%s'" name);
	let start_time = Unix.gettimeofday () in
	let result = f () in
	let end_time = Unix.gettimeofday () in
	print_endline (Printf.sprintf "'%s' took %f seconds" name (end_time -. start_time));
	result

let use_optimisations = true

let test_big_database () =
	let db = MockDatabase.make () in
	let __context = MockContext.make ~database:db "Mock context" in
	let hosts, pool, sr, pbds, vms, vdis = time_it "Creating dummy database" (fun () ->
		(* Create 16 hosts. *)
		let hosts = create_multiple
			~create_one:(fun n -> create_host ~__context ~name_label:(Printf.sprintf "host_%d" n))
			~count:16 in
		let pool = create_pool ~__context ~master:(List.hd hosts) in
		(* Create one shared SR with one PBD per host. *)
		let sr = create_sr ~__context ~name_label:"SR" ~_type:"lvmoiscsi" in
		let pbds = List.map (fun host -> create_pbd ~__context ~host ~sr) hosts in
		(* Create 80 VMs per host. *)
		let vms = List.concat
			(List.map
				(fun host -> create_vms_on_host ~__context ~host ~count:80)
				hosts) in
		(* Create 12,000 VDIs. *)
		let vdis = create_multiple
			~create_one:(fun n -> create_vdi ~__context ~sr ~name_label:(Printf.sprintf "vdi_%d" n))
			~count:12000 in
		(hosts, pool, sr, pbds, vms, vdis))
	in
	let vbds = [] in
	let srs = [sr] in
	(* Update allowed operations on all VDIs in the database. *)
	time_it "Updating allowed_operations on all VDIs" (fun () ->
		if use_optimisations then begin
			let sr_records = List.map (fun sr -> (sr, Db.SR.get_record_internal ~__context ~self:sr)) srs in
			let pbd_records = List.map (fun pbd -> (pbd, Db.PBD.get_record ~__context ~self:pbd)) pbds in
			let vbd_records = List.map (fun vbd -> (vbd, Db.VBD.get_record_internal ~__context ~self:vbd)) vbds in
			List.iter (fun vdi -> Xapi_vdi.update_allowed_operations_internal ~__context ~self:vdi ~sr_records ~pbd_records ~vbd_records) vdis;
		end else
			List.iter (fun vdi -> Xapi_vdi.update_allowed_operations ~__context ~self:vdi) vdis);
	ignore (hosts, pool, sr, pbds, vms, vdis)

let _ = test_big_database ()
