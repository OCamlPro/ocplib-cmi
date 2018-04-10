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

(* [signature sg] returns a signature equivalent to [sg] where sharing
has been increased.  *)

val signature : Types.signature -> Types.signature

(* Remove locations *)
val rmloc : bool ref
