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

#if OCAML_VERSION >= "4.01.0"

open Cmi_format

type cmi_file = Cmi_format.cmi_infos

let signature cmi = cmi.cmi_sign
let with_signature cmi cmi_sign = { cmi with cmi_sign }
let read_file filename =
  let cmi = Cmi_format.read_cmi filename in
  match cmi.cmi_crcs with
#if OCAML_VERSION >= "4.02.0"
  | (name, Some crc) :: cmi_crcs when name = cmi.cmi_name ->
#else
  | (name, crc) :: cmi_crcs when name = cmi.cmi_name ->
#endif
     crc, { cmi with cmi_crcs }
  | _ -> assert false


#if OCAML_VERSION >= "4.06.0"

let write_file filename cmi =
  let _crc =
    Misc.output_to_file_via_temporary (* see MPR#7472, MPR#4991 *)
      ~mode: [Open_binary] filename
      (fun temp_filename oc -> Cmi_format.output_cmi temp_filename oc cmi) in
  ()

#else

let write_file filename cmi =
  let oc = open_out_bin filename in
  let _crc = Cmi_format.output_cmi filename oc cmi in
  close_out oc

#endif

#else

type cmi_file =
  string * Types.signature * (string * Digest.t) list * Env.pers_flags list

let signature sg = sg
let with_signature _ sg = sg

let read_file filename =
  let ic = open_in filename in
  try
    let magic = Bytes.create 12 in
    really_input ic magic 0 12;
    if magic = Config.cmi_magic_number then
      let (modname, sg) = (input_value ic : string * Types.signature) in
      let crcs = (input_value ic : (string * Digest.t) list) in
      let flags = input_value ic in
      close_in ic;
      (modname, sg, crcs, flags)
    else failwith ("bad ocaml version : " ^ magic)
  with e -> close_in ic; raise e


let write_file filename sg =
(*  Printf.fprintf stderr "Saving %s\n%!" filename; *)
  let oc = open_out filename in
  let f = Format.formatter_of_out_channel oc in
  Printtyp.reset_names ();
  try
    Printtyp.signature f sg;
  Format.fprintf f "@.";
  close_out oc
  with e ->
    Format.fprintf f "@.";
    close_out oc;
    raise e

#endif
