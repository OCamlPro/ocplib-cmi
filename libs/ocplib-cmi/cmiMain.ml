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

let getsize filename =
(* (MinUnix.lstat filename).MinUnix.st_size *)
  let ic = open_in_bin filename in
  let size = in_channel_length ic in
  close_in ic;
  size

let compress_file filename =
  let size0 = getsize filename in
  let _crc, file = CmiUtils.read_file filename in
  let sg = CmiUtils.signature file in
  let sg = CmiCompress.signature sg in
  let file = CmiUtils.with_signature file sg in
  let backup_file = filename ^ ".old" in
  if not (Sys.file_exists backup_file) then Sys.rename filename backup_file;
  CmiUtils.write_file filename file;
  let size1 = getsize filename in
  Printf.eprintf "%s: %d -> %d\n%!" filename size0 size1

let compress_dir dirname =
  let files = Sys.readdir dirname in
  Array.iter (fun file ->
      if Filename.check_suffix file ".cmi" then
        compress_file (Filename.concat dirname file)
    ) files

let restore_dir dirname =
  let files = Sys.readdir dirname in
  Array.iter (fun file ->
      let file = Filename.concat dirname file in
      if Filename.check_suffix file ".cmi.old" then
        let cmi_file = Filename.chop_suffix file ".old" in
        Sys.rename file cmi_file;
        Printf.eprintf "%s restored\n%!" cmi_file
    ) files

let arg_list =
  Arg.align [
      "--rmloc", Arg.Set CmiCompress.rmloc, " Remove locations";
      "--compress-dir", Arg.String compress_dir, "DIR Compress cmis in directory";
      "--restore-dir", Arg.String restore_dir, "DIR Restore cmis in directory";
    ]

let arg_usage = "Compress cmi files"

let () =
  Arg.parse arg_list compress_file arg_usage;
  exit 0
