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

(* This module will only work on immutable, non-cyclic data in the heap. *)

(* [get v] returns a value structurally equivalent to [v], sharing as much data structure
  as possible with values returned by previous calls to [get]. *)
val get : 'a -> 'a

(* [reset ()] clear the history of values. Needed to free space. *)
val reset : unit -> unit
