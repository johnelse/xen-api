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
