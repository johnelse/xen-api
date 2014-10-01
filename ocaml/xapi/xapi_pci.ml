(*
 * Copyright (C) 2006-2011 Citrix Systems Inc.
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
module D=Debug.Debugger(struct let name="xapi" end)
open D

open Listext
open Stringext

type managed_class = Display_controller | Network_controller

let lookup_class_id = function
	| Display_controller -> 03L
	| Network_controller -> 02L

let managed_classes = [Display_controller]

let get_pcis_by_class pcis cls =
	let open Xapi_pci_helpers in
	List.filter (fun pci -> pci.pci_class.id = lookup_class_id cls) pcis

let string_of_pci ~__context ~self =
	let pci = Db.PCI.get_record_internal ~__context ~self in
	String.concat "/" [pci.Db_actions.pCI_vendor_id; pci.Db_actions.pCI_device_id]

(* We use ints within code but schema uses hex strings _without_ leading '0x' *)
let int_of_id string_id =
	let int_of_hex_str = fun s -> Scanf.sscanf s "%Lx" (fun x -> x) in
	int_of_hex_str string_id
let id_of_int hex_id =
	Printf.sprintf "%04Lx" hex_id

let create ~__context ~class_id ~class_name ~vendor_id ~vendor_name ~device_id
		~device_name ~host ~pci_id ~functions ~dependencies ~other_config
		~subsystem_vendor_id ~subsystem_vendor_name
		~subsystem_device_id ~subsystem_device_name =
	let p = Ref.make () in
	let uuid = Uuid.to_string (Uuid.make_uuid ()) in
	Db.PCI.create ~__context ~ref:p ~uuid ~class_id ~class_name ~vendor_id ~vendor_name ~device_id
		~device_name ~host ~pci_id ~functions ~dependencies:[] ~other_config:[]
		~subsystem_vendor_id ~subsystem_vendor_name
		~subsystem_device_id ~subsystem_device_name;
	debug "PCI %s, %s, %s created" pci_id vendor_name device_name;
	p

let update_pcis ~__context ~host =
	let existing = List.filter_map
		(fun pref ->
			let prec = Db.PCI.get_record_internal ~__context ~self:pref in
			if prec.Db_actions.pCI_host = host then
				Some (pref, prec)
			else
				None)
		(Db.PCI.get_all ~__context)
	in

	let open Xapi_pci_helpers in
	let pci_db = Pci_db.open_default () in
	let strings_of_pci_property = function
		| None -> "", ""
		| Some property -> id_of_int property.id, property.name
	in
	let rec update_or_create cur = function
		| [] -> cur
		| pci :: remaining_pcis ->
			let obj =
				try
					let (rf, rc) = List.find (fun (rf, rc) ->
						rc.Db_actions.pCI_pci_id = pci.pci_id &&
						rc.Db_actions.pCI_vendor_id = id_of_int pci.vendor.id &&
						rc.Db_actions.pCI_device_id = id_of_int pci.device.id)
						existing in
					(* sync the vendor name. *)
					if rc.Db_actions.pCI_vendor_name <> pci.vendor.name
					then Db.PCI.set_vendor_name ~__context ~self:rf ~value:pci.vendor.name;
					(* sync the device name. *)
					if rc.Db_actions.pCI_device_name <> pci.device.name
					then Db.PCI.set_device_name ~__context ~self:rf ~value:pci.device.name;
					(* sync the subsystem vendor fields. *)
					let subsystem_vendor_id, subsystem_vendor_name =
						strings_of_pci_property pci.subsystem_vendor in
					if rc.Db_actions.pCI_subsystem_vendor_id <> subsystem_vendor_id
					then Db.PCI.set_subsystem_vendor_id ~__context ~self:rf ~value:subsystem_vendor_id;
					if rc.Db_actions.pCI_subsystem_vendor_name <> subsystem_vendor_name
					then Db.PCI.set_subsystem_vendor_name ~__context ~self:rf ~value:subsystem_vendor_name;
					(* sync the subsystem device fields. *)
					let subsystem_device_id, subsystem_device_name =
						strings_of_pci_property pci.subsystem_device in
					if rc.Db_actions.pCI_subsystem_device_id <> subsystem_device_id
					then Db.PCI.set_subsystem_device_id ~__context ~self:rf ~value:subsystem_device_id;
					if rc.Db_actions.pCI_subsystem_device_name <> subsystem_device_name
					then Db.PCI.set_subsystem_device_name ~__context ~self:rf ~value:subsystem_device_name;
					(* sync the attached VMs. *)
					let attached_VMs = List.filter (Db.is_valid_ref __context) rc.Db_actions.pCI_attached_VMs in
					if attached_VMs <> rc.Db_actions.pCI_attached_VMs then
						Db.PCI.set_attached_VMs ~__context ~self:rf ~value:attached_VMs;
					rf, rc
				with Not_found ->
					let subsystem_vendor_id, subsystem_vendor_name =
						strings_of_pci_property pci.subsystem_vendor in
					let subsystem_device_id, subsystem_device_name =
						strings_of_pci_property pci.subsystem_device in
					let self = create ~__context
						~class_id:(id_of_int pci.pci_class.id)
						~class_name:pci.pci_class.name
						~vendor_id:(id_of_int pci.vendor.id)
						~vendor_name:pci.vendor.name
						~device_id:(id_of_int pci.device.id)
						~device_name:pci.device.name ~host ~pci_id:pci.pci_id
						~functions:1L ~dependencies:[] ~other_config:[]
						~subsystem_vendor_id ~subsystem_vendor_name
						~subsystem_device_id ~subsystem_device_name in
					self, Db.PCI.get_record_internal ~__context ~self
			in
			update_or_create ((obj, pci) :: cur) remaining_pcis
	in
	let host_pcis = Xapi_pci_helpers.get_host_pcis pci_db in
	let class_pcis = List.flatten (List.map (fun cls -> get_pcis_by_class host_pcis cls) managed_classes) in
	let deps = List.flatten (List.map (fun pci -> pci.related) class_pcis) in
	let deps = List.map (fun dep -> List.find (fun pci -> pci.pci_id = dep) host_pcis) deps in
	let managed_pcis = List.setify (class_pcis @ deps) in
	let current = update_or_create [] managed_pcis in

	let update_dependencies current =
		let rec update = function
		| [] -> ()
		| ((pref, prec), pci) :: remaining ->
			let dependencies = List.map
				(fun pci_id ->
					let (r, _), _ = List.find (fun ((_, rc), _) -> rc.Db_actions.pCI_pci_id = pci_id) current
					in r)
				pci.related
			in
			Db.PCI.set_dependencies ~__context ~self:pref ~value:dependencies;
			update remaining
		in
		update current
	in
	update_dependencies current;

	let current = List.map (fun ((pref, prec), _) -> pref, prec) current in
	let obsolete = List.set_difference existing current in
	List.iter (fun (self, _) -> Db.PCI.destroy ~__context ~self) obsolete

let get_pciback_devices () =
  try
    let dir = "/sys/bus/pci/drivers/pciback" in
    let children = Array.to_list (Sys.readdir dir) in
    List.filter (fun item -> String.startswith "0000" item) children
  with _ -> []

let get_system_display_device () =
	try
		let line =
			Unixext.with_file "/dev/vga_arbiter" [Unix.O_RDONLY] 0o000 (fun fd ->
				let data = Unixext.try_read_string ~limit:1024 fd in
				List.hd (String.split ~limit:2 '\n' data)
			)
		in
		(* Example contents of line:
		 * count:7,PCI:0000:10:00.0,decodes=io+mem,owns=io+mem,locks=none(0:0) *)
		let items = String.split ',' line in
		List.fold_left
			(fun acc item ->
				if String.startswith "PCI" item
				then Some (Scanf.sscanf item "PCI:%s" (fun id -> id))
				else acc)
			None items
	with _ -> None
