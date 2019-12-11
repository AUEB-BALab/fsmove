(*
 * Copyright (c) 2018-2019 Thodoris Sotiropoulos
 *
 * This program is free software: you can redistribute it and/or modify  
 * it under the terms of the GNU General Public License as published by  
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but 
 * WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License 
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *)


open Pervasives


exception Timeout


(* Default modulepath. *)
let modulepath = "/etc/puppet/code/modules"

(* Default timeout in seconds. *)
let default_timeout = 360


let timeout_status_code = 253


let child_failed_status_code = 255


let make_executor_err msg =
  raise (Errors.Error (Errors.ExecutorError, Some msg))


let string_of_unix_err err call params =
  Printf.sprintf "%s: %s (%s)" (Unix.error_message err) call params


let read_buff output =
  (* We read the buffer from the output file descriptor;
   20 bytes at a time. *)
  let len = 20 in
  let buff = Bytes.create len in
  let rec _read_buf acc buff =
    match Unix.read output buff 0 len with
    | 0 -> acc
    | n ->
      let acc' = acc ^ (String.sub buff 0 n) in
      _read_buf acc' buff
  in
  _read_buf "" buff


let timeout f time default_value =
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout));
  ignore(Unix.alarm time);
  try
    f ()
  with
  | Timeout -> default_value
  | exc     -> raise exc


let run_strace_and_puppet trace_file manifest modulepath input =
  let prog = "/usr/bin/strace" in
  let args = [|
    "strace";
    "-s";
    "300";
    "-o";
    trace_file;
    "-f";
    "puppet";
    "apply";
    manifest;
    "--modulepath";
    modulepath;
    "--debug";
    "--evaltrace"; |]
  in
  try
    ignore (Unix.execv prog args);
    exit 254; (* We should never reach here. *)
  with Unix.Unix_error (err, call, params) ->
    (* Maybe strace is not installed in the system.
      So, we pass the exception to err to the pipe
      so that it can be read by the parent process. *)
    let msg = string_of_unix_err err call params in
    begin
      ignore (Unix.write input msg 0 (String.length msg));
      Unix.close input;
      exit child_failed_status_code;
    end


let run_manifest ?(modulepath=modulepath) ?(proc_timeout=default_timeout) manifest trace_file =
  if not (Sys.file_exists manifest)
  then make_executor_err ("The manifest " ^ manifest ^ " does not exist")
  else
    let output, input = Unix.pipe() in
    (* We create a child process that is responsible for invoking
     strace and run the apply the provided puppet manifest. *)
    match Unix.fork () with
    | 0   -> run_strace_and_puppet trace_file manifest modulepath input
    | pid ->
      Unix.close input;
      let _, status =
        (0, Unix.WSIGNALED timeout_status_code)
        |> timeout Unix.wait proc_timeout
      in
      match status with
      | Unix.WEXITED 0  ->
        Printf.fprintf stdout
          "The application of the manifest %s terminated successfully" manifest
      | Unix.WEXITED 255  ->
        (** The invocation of strace seems to be failed.
          So we read the error message from the output pipe. *)
        let msg = read_buff output in
        make_executor_err msg
      | Unix.WEXITED i  ->
        let msg = "The application of the manifest " ^ manifest
          ^ " returned with status code " ^ (string_of_int i) in
        make_executor_err msg
      | Unix.WSIGNALED 253  ->
          (* This means that the timeout has elapsed, so it's time to
            kill the child process. *)
          Printf.fprintf stdout "Killing process %d\n" pid;
          Unix.kill pid Sys.sigkill;
      | Unix.WSTOPPED i
      | Unix.WSIGNALED i -> (
        let msg = "The application of the manifest " ^ manifest
          ^ " stopped with " ^ (string_of_int i) ^ " status code" in
        make_executor_err msg)
    | exception Unix.Unix_error (err, call, params) ->
      params |> string_of_unix_err err call |> make_executor_err
