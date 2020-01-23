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


type mode =
  | Online
  | Offline


type options =
  {mode: mode;
   manifest: string option;
   modulepath: string option;
   package_notify: bool;
   graph_file: string option;
   graph_format: Dependency_graph.graph_format;
   print_stats: bool;
   trace_file: string option;
   dump_puppet_out: string option;
  }


type read_point =
  | File of string
  | FileDesc of Unix.file_descr


type option_status =
  | Ok
  | Err of string


let child_failed_status_code = 255


let make_executor_err msg =
  raise (Errors.Error (Errors.ExecutorError, Some msg))


let string_of_unix_err err call params =
  Printf.sprintf "%s: %s (%s)" (Unix.error_message err) call params


let validate_options = function
  | { mode = Online; manifest = None; _} ->
    Err "Mode 'online' requires the option '-manifest.'"
  | { mode = Online; modulepath = None; _ } ->
    Err "Mode 'online' requires the option '-modulepath'"
  | { mode = Offline; manifest = Some _; _ } ->
    Err "Option `-manifest` is only compatiable with the mode 'online'"
  | { mode = Offline; modulepath = Some _; _ } ->
    Err "Option `-modulepath` is only compatiable with the mode 'online'"
  | { mode = Offline; dump_puppet_out = Some _; _ } ->
    Err "Option `-dump-puppet-out` is only compatiable with the mode 'online'"
  | { mode = Offline; trace_file = None; _ } ->
    Err "Mode 'offline' requires the option '-trace-file.'"
  | _ -> Ok


let syscalls = [
  "access";
  "chdir";
  "chmod";
  "chown";
  "clone";
  "close";
  "dup";
  "dup2";
  "dup3";
  "execve";
  "fchdir";
  "fchmodat";
  "fchownat";
  "fcntl";
  "fork";
  "getxattr";
  "getcwd";
  "lchown";
  "lgetxattr";
  "lremovexattr";
  "lsetxattr";
  "lstat";
  "link";
  "linkat";
  "mkdir";
  "mkdirat";
  "mknod";
  "open";
  "openat";
  "readlink";
  "readlinkat";
  "removexattr";
  "rename";
  "renameat";
  "rmdir";
  "statfs";
  "symlink";
  "symlinkat";
  "unlink";
  "unlinkat";
  "utime";
  "utimensat";
  "utimes";
  "vfork";
  "write";
  "writev";
]


let run_strace_and_puppet manifest modulepath input puppet_out =
  let modulepath =
    match modulepath with
    | None            -> "/etc/puppet/code/modules"
    | Some modulepath -> modulepath
  in
  let prog = "/usr/bin/strace" in
  let fd_out = input |> Fd_send_recv.int_of_fd |> string_of_int in
  let args = [|
    "strace";
    "-s";
    "300";
    "-o";
    ("/dev/fd/" ^ fd_out);
    "-e";
    (String.concat "," syscalls);
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
    print_endline ("Start executing manifest " ^ manifest ^ " ...");
    let out =
      match puppet_out with
      | None            -> "/dev/null"
      | Some puppet_out -> puppet_out
    in
    let fd = Unix.openfile out [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o640 in
    let _ = Unix.dup2 fd Unix.stdout in
    let _ = Unix.dup2 Unix.stdout Unix.stderr in
    let _ = Unix.close fd in
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


let analyze_trace_internal read_p catalog debug_trace_file options =
  let module PuppetParser = Sys_parser.Make(Puppet_parser) in
  let module PuppetAnalyzer = Analyzer.Make(Puppet) in
  let open Puppet in
  let open PuppetAnalyzer in
  let open PuppetParser in
  let t0 = Unix.gettimeofday () in
  let resource_graph, _ =
    match read_p with
    | File p     ->
      p
      |> parse_trace_file debug_trace_file
      |> analyze_traces
    | FileDesc p ->
      p
      |> parse_trace_fd debug_trace_file
      |> analyze_traces
  in
  let _ = detect_bugs
    ~graph_format:options.graph_format
    ~package_notify:options.package_notify
    resource_graph catalog options.graph_file
  in
  let diff = Unix.gettimeofday () -. t0 in
  if options.print_stats then begin
    print_endline ("Analysis time: " ^ (string_of_float diff))
  end;
  ()


let run_manifest manifest catalog options =
  if not (Sys.file_exists manifest)
  then make_executor_err ("The manifest " ^ manifest ^ " does not exist")
  else
    let output, input = Unix.pipe () in
    (* We create a child process that is responsible for invoking
     strace and run the apply the provided puppet manifest. *)
    match Unix.fork () with
    | 0   ->
      Unix.close output;
      run_strace_and_puppet manifest options.modulepath input options.dump_puppet_out
    | pid -> (
      Unix.close input;
      analyze_trace_internal
        (FileDesc output) catalog options.trace_file options;
      try
        Unix.kill pid Sys.sigkill;
        Unix.close output;
      with Unix.Unix_error _ -> ())
    | exception Unix.Unix_error (err, call, params) ->
      params |> string_of_unix_err err call |> make_executor_err


let analyze_trace trace_file catalog options =
  analyze_trace_internal (File trace_file) catalog None options
