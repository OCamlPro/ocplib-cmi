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

let rmloc = ref false

open Asttypes
open Types

let share x = MaxShare.get x
let string : string -> string = share
let path : Path.t -> Path.t = share
let ident : Ident.t -> Ident.t = share
let location loc =
  if !rmloc then Location.none else share loc

let remembered = ref []
let type_of_id = Hashtbl.create 1111
let hcons = Hashtbl.create 1111


let ids = ref (-2)
let reset_ids () = ids := -2
let newty ty =
  let id = !ids in
  decr ids;
  { ty with id }

let counter = ref 0

let map_option f x = match x with None -> None | Some x -> Some (f x)

#if OCAML_VERSION >= "4.04.0"

#include "cmiCompress404.ml"

#elif OCAML_VERSION >= "4.03.0"

#include "cmiCompress403.ml"

#elif OCAML_VERSION = "4.02.1"

#include "cmiCompress402.ml"

#elif OCAML_VERSION = "4.01.0"

#include "cmiCompress401.ml"

#else
#error "Version of ocaml not supported"
#endif


  (* This function runs of the list of all closed objects found during
     the first pass.  Objects that are not used anymore have been set
     to Tnil, so just discard them.  For other objects, try to list
     methods in increasing order of popularity, so that we are more
     likely to share the end of the list with other objects. *)

  let share_remembered_objects () =
    (* Printf.printf "sharing...\n%!"; *)
    let h = Hashtbl.create 111 in
    let h2 = Hashtbl.create 111 in
    List.iter (fun (ty, meths, _refo) ->
        if ty.desc <> Tnil then
          List.iter (fun (name, kind, ty) ->
              let key = Marshal.to_string (name, kind, ty) [] in
              try
                incr (Hashtbl.find h key)
              with Not_found ->
                Hashtbl.add h key (ref 1)
            ) meths
      ) !remembered;
    List.iter (fun (objty, meths, refo) ->
        if objty.desc <> Tnil then
          let meths = List.map (fun (name, kind, ty) ->
                          let key = Marshal.to_string (name, kind, ty) [] in
                          (! (Hashtbl.find h key), key, name,  kind,  ty)
                        ) meths in
          let meths = List.sort compare meths in
          let rec unflatten meths =
            let desc, list =
              match meths with
                [] -> Tnil, []
              | (n, key, name, kind, ty) :: meths ->
                 (*                Printf.printf "  %d\t%s\n" n name; *)
                 let (meths, list) = unflatten meths in
                 Tfield(name, kind, ty, meths), (n, key) :: list
            in
            try
              Hashtbl.find h2 list
            with Not_found ->
              let ty = newty { desc = desc;
                               id = 0;
                               level = objty.level;
                             } in
              Hashtbl.add h2 list (ty, list);
              ty, list
          in
          let (ty, _list) = unflatten meths in
          (*        Printf.printf "OBJECT\n"; *)
          objty.desc <- Tobject(ty, refo);
      ) !remembered

  let signature sg =
    Hashtbl.clear hcons;
    Hashtbl.clear type_of_id;
    counter := 10;
    reset_ids();
    remembered := [];
    let sg = signature sg in
    share_remembered_objects ();
    Hashtbl.clear hcons;
    Hashtbl.clear type_of_id;
    remembered := [];
    sg
