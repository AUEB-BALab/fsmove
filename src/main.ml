(*
 * Copyright (c) 2018-2020 Thodoris Sotiropoulos
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


open Fsmove


let handle_error err msg =
  match msg with
  | None ->
    begin
      Printf.eprintf "Error: %s" (Errors.string_of_error err);
      exit 1;
    end
  | Some msg ->
    begin
      Printf.eprintf "Error: %s: %s" (Errors.string_of_error err) msg;
      exit 1;
    end


let format_of_string = function
  | "dot" -> Dependency_graph.Dot
  | "csv" -> Dependency_graph.Csv
  | _     ->
    begin
      Printf.eprintf "Format must be either 'dot' or 'csv'";
      exit 1;
    end


let mode_of_string = function
  | "online"  -> Puppet_ex.Online
  | "offline" -> Puppet_ex.Offline
  | _         ->
    begin
      Printf.eprintf "Mode must be either 'online' or 'offline'";
      exit 1;
    end


let puppet_ex catalog options =
  match Puppet_ex.validate_options options with
  | Err err ->
    Printf.eprintf "Error: %s. Run command with -help" err;
    exit 1
  | Ok ->
    try
      match options with
      | { mode = Online; manifest = Some manifest; } ->
        Puppet_ex.run_manifest manifest catalog options
      | { mode = Offline; trace_file = Some trace_file; } ->
        Puppet_ex.analyze_trace trace_file catalog options
      | _ ->
        raise (Errors.Error (Errors.InternalError, (Some "Unreachable case")))
    with Errors.Error (err, msg) -> handle_error err msg


let () =
  let open Core.Command.Let_syntax in
  Core.Command.basic
    ~summary:"Applies a Puppet Manifest and collects its system call trace."
    [%map_open
    let catalog =
      flag "catalog" (required string)
      ~doc:"Path to compiled catalog of manifest."
    and mode =
      flag "mode" (required (Arg_type.create mode_of_string))
      ~doc: "Analysis mode; either online or offline"
    and manifest =
      flag "manifest" (optional string)
      ~doc: "Path to the manifest that we need to apply. (Avaiable only when mode is 'online')"
    and modulepath =
      flag "modulepath" (optional string)
      ~doc: "Path to the directory of the Puppet modules. (Available only when mode is 'online')"
    and trace_file =
      flag "trace-file" (optional string)
      ~doc:"Path to trace file produced by the 'strace' tool. (Available only when mode is 'offline')"
    and dump_puppet_out =
      flag "dump-puppet-out" (optional string)
      ~doc: "File to store output from puppet execution (for debugging only)"
    and graph_format =
      flag "graph-format" (optional_with_default Dependency_graph.Dot (Arg_type.create format_of_string))
      ~doc: "Format for storing the dependency graph of the provided Puppet manifests."
    and graph_file =
      flag "graph-file" (optional string)
      ~doc: "File to store the dependency graph inferred by the compiled catalog."
    and print_stats =
      flag "print-stats" (no_arg)
      ~doc: "Print stats about execution and analysis"
    and package_notify =
      flag "package-notify" (no_arg)
      ~doc: "Consider missing notifiers from packages to services"
    in
    fun () ->
      {Puppet_ex.trace_file = trace_file;
       Puppet_ex.manifest = manifest;
       Puppet_ex.dump_puppet_out = dump_puppet_out;
       Puppet_ex.mode = mode;
       Puppet_ex.graph_file = graph_file;
       Puppet_ex.graph_format = graph_format;
       Puppet_ex.print_stats = print_stats;
       Puppet_ex.package_notify = package_notify;
       Puppet_ex.modulepath = modulepath;} |> puppet_ex catalog
    ] |> Core.Command.run
