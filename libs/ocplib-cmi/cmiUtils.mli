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

type cmi_file

val signature : cmi_file -> Types.signature
val with_signature : cmi_file -> Types.signature -> cmi_file

val read_file : string -> Digest.t * cmi_file
val write_file : string -> cmi_file -> unit
