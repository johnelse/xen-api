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

module DD=Debug.Debugger(struct let name="xapi" end)
open DD

open Pervasiveext

module Fist = struct
	let wait ~__context ~fist_point ~name =
		if fist_point () then begin
			TaskHelper.add_to_other_config ~__context "fist" name;

			while fist_point () do
				debug "Sleeping while fistpoint %s exists" name;
				Thread.delay 5.0
			done;

			TaskHelper.operate_on_db_task ~__context
				(fun self -> Db.Task.remove_from_other_config ~__context ~self ~key:"fist")
		end

	let wait_for_pause_storage_migrate ~__context =
		wait ~__context
			~fist_point:Xapi_fist.pause_storage_migrate
			~name:"pause_storage_migrate"

	let wait_for_pause_storage_migrate2 ~__context =
		wait ~__context
			~fist_point:Xapi_fist.pause_storage_migrate2
			~name:"pause_storage_migrate2"
end

module Mirror = struct
	(* Xapi's view of a SMAPI mirror operation. *)
	type record = {
		mr_mirrored : bool;
		mr_dp : Storage_interface.dp;
		mr_vdi : Storage_interface.vdi;
		mr_sr : Storage_interface.sr;
		mr_local_xenops_locator : string;
		mr_remote_xenops_locator : string;
		mr_remote_vdi_reference : API.ref_VDI;
	}
end

module Snapshot = struct
	let get_snapshots_vbds ~__context ~vm =
		let rec aux acc nb_snapshots cur =
			let parent = Db.VM.get_parent ~__context ~self:cur in
			debug "get_snapshots %s" (Ref.string_of parent);
			if not (Db.is_valid_ref __context parent) then
				(acc,nb_snapshots)
			else
				aux ((Db.VM.get_VBDs ~__context ~self:parent) @ acc) (nb_snapshots + 1) parent in
		aux [] 0 vm

	let destroy_all ~__context ~vm =
		let rec aux cur =
			let parent = Db.VM.get_parent ~__context ~self:cur in
			Db.VM.destroy ~__context ~self:cur;
			if Db.is_valid_ref __context parent then
				aux parent
		in aux vm
end

module Interpool = struct
	(* Export a local VM's metadata and import into a remote pool. *)
	let metadata_transfer
			~__context ~remote_rpc ~session_id ~remote_address
			~vm ~vdi_map ~vif_map ~dry_run ~live =
		List.iter (fun (vdi,mirror_record) ->
			Db.VDI.remove_from_other_config ~__context ~self:vdi
				~key:Constants.storage_migrate_vdi_map_key;
			Db.VDI.add_to_other_config ~__context ~self:vdi
				~key:Constants.storage_migrate_vdi_map_key
				~value:(Ref.string_of mirror_record.Mirror.mr_remote_vdi_reference)) vdi_map;

		List.iter (fun (vif,network) ->
			Db.VIF.remove_from_other_config ~__context ~self:vif
				~key:Constants.storage_migrate_vif_map_key;
			Db.VIF.add_to_other_config ~__context ~self:vif
				~key:Constants.storage_migrate_vif_map_key
				~value:(Ref.string_of network)) vif_map;

		let vm_export_import = {
			Importexport.vm = vm; dry_run = dry_run; live = live; send_snapshots=true;
		} in
		finally
			(fun () ->
				Importexport.remote_metadata_export_import ~__context
					~rpc:remote_rpc ~session_id ~remote_address (`Only vm_export_import))
			(fun () ->
				(* Make sure we clean up the remote VDI and VIF mapping keys. *)
				List.iter
					(fun (vdi, _) ->
						Db.VDI.remove_from_other_config ~__context ~self:vdi
							~key:Constants.storage_migrate_vdi_map_key)
					vdi_map;
				List.iter
					(fun (vif, _) ->
						Db.VIF.remove_from_other_config ~__context ~self:vif
							~key:Constants.storage_migrate_vif_map_key)
					vif_map)
end

module Intrapool = struct
	(* If VM's suspend_SR is set to the local SR, it won't be visible to
	 * the destination host after an intra-pool storage migrate *)
	let fix_suspend_sr ~__context host vm =
		let sr = Db.VM.get_suspend_SR ~__context ~self:vm in
		if not (Helpers.host_has_pbd_for_sr ~__context ~host ~sr)
		then Db.VM.set_suspend_SR ~__context ~self:vm ~value:Ref.null

	(* Switch a VM's VBDs to point at the mirror destination VDIs
	 * rather than the mirror source VDIs. *)
	let vdi_remap ~__context vm vdi_map =
		let vbds = Db.VM.get_VBDs ~__context ~self:vm in
		List.iter (fun vbd ->
			let vdi = Db.VBD.get_VDI ~__context ~self:vbd in
			if List.mem_assoc vdi vdi_map then
				let mirror_record = List.assoc vdi vdi_map in
				Db.VBD.set_VDI ~__context ~self:vbd
					~value:mirror_record.Mirror.mr_remote_vdi_reference) vbds
end
