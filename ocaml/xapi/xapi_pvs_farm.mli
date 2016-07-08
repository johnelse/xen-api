(*
 * Copyright (C) 2016 Citrix Systems Inc.
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

val introduce : __context:Context.t -> name:string -> API.ref_PVS_farm

val forget : __context:Context.t -> self:API.ref_PVS_farm -> unit

val set_name : __context:Context.t ->
	self:API.ref_PVS_farm -> value:string -> unit

val add_cache_storage : __context:Context.t ->
	self:API.ref_PVS_farm -> value:API.ref_SR -> unit

val remove_cache_storage : __context:Context.t ->
	self:API.ref_PVS_farm -> value:API.ref_SR -> unit

val proxy_port_name : string -> string

(** Request xcp-networkd to update a farm's PVS-proxy daemon configuration,
 *  for all locally running proxies, taking into account starting and stopping proxies *)
val update_farm_on_localhost : __context:Context.t ->
	self:API.ref_PVS_farm ->
	vdi:API.ref_VDI ->
	?starting_proxies:(API.ref_VIF * API.ref_PVS_proxy) list ->
	?stopping_proxies:(API.ref_VIF * API.ref_PVS_proxy) list ->
	unit ->
	unit
