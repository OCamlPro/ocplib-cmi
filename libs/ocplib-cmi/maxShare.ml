(***********************************************************************)
(*                                                                     *)
(*                             ocplib-cmi                              *)
(*                                                                     *)
(*      Copyright 2012-2018 OCamlPro SAS                               *)
(*      Distributed under the GNU Lesser General Public License        *)
(*      version 2.1, with the special exception on linking described   *)
(*      in the file LICENSE.                                           *)
(*                                                                     *)
(***********************************************************************)

let h = Hashtbl.create 11333

let rec iter x =
  if Obj.is_block x then
    try
      Hashtbl.find h x
    with Not_found ->
      let x =
        let tag = Obj.tag x in
        if tag < Obj.no_scan_tag then
          let size = Obj.size x in
          let y = Obj.new_block tag size in
          Hashtbl.add h x y; (* try to avoid infinite loop *)
          for i = 0 to size - 1 do
            Obj.set_field y i (iter (Obj.field x i))
          done;
          y
        else
          x
      in
      Hashtbl.add h x x;
      x
  else x

let get x = Obj.magic (iter (Obj.repr x))
let reset () = Hashtbl.clear h
