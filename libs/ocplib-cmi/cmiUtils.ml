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

#if OCAML_VERSION = "4.06.1"

open Cmi_format

type cmi_file = Cmi_format.cmi_infos

let signature cmi = cmi.cmi_sign
let with_signature cmi cmi_sign = { cmi with cmi_sign }
let read_file filename =
  let cmi = Cmi_format.read_cmi filename in
  match cmi.cmi_crcs with
  | (name, Some crc) :: cmi_crcs when name = cmi.cmi_name ->
     crc, { cmi with cmi_crcs }
  | _ -> assert false

let write_file filename cmi =
  let _crc =
    Misc.output_to_file_via_temporary (* see MPR#7472, MPR#4991 *)
      ~mode: [Open_binary] filename
      (fun temp_filename oc -> Cmi_format.output_cmi temp_filename oc cmi) in
  ()

#else

let load filename =
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


let save filename sg =
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
