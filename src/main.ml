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


let format_of_string str =
  match str with
  | "dot" -> Dependency_graph.Dot
  | "csv" -> Dependency_graph.Csv
  | _     ->
    begin
      Printf.eprintf "Format must be either 'dot' or 'csv'";
      exit 1;
    end


let puppet trace_file catalog graph_file graph_format print_time package_notify =
  let module PuppetParser = Sys_parser.Make(Puppet_parser) in
  let module PuppetAnalyzer = Analyzer.Make(Puppet) in
  let open Puppet in
  let open PuppetAnalyzer in
  let open PuppetParser in
  let t0 = Unix.gettimeofday () in
  try
    let resource_graph, _ = trace_file
      |> parse_trace_file
      |> analyze_traces
    in
    detect_bugs ~graph_format ~package_notify resource_graph catalog graph_file;
    let diff = Unix.gettimeofday () -. t0 in
    if print_time then begin
      print_string
        ("Analysis time: " ^ (string_of_float diff) ^ "\n")
    end;
  with
  | Errors.Error (err, msg) -> handle_error err msg


let puppet_ex trace_file manifest modulepath timeout =
  try
    Puppet_ex.run_manifest manifest trace_file ~modulepath ~proc_timeout: timeout
  with Errors.Error (err, msg) -> handle_error err msg


let analyze_cmd =
  let open Core.Command.Let_syntax in
  Core.Command.basic
    ~summary:"Detects Missing Ordering Relationships (MOR) and Missing Notifiers (MN) in Puppet Programs"
    [%map_open
    let trace_file =
      flag "traces" (required string)
      ~doc:"Path to trace file produced by the 'strace' tool."
    and catalog =
      flag "catalog" (required string)
      ~doc: "Path to the compiled catalog."
    and graph_format =
      flag "graph-format" (optional_with_default Dependency_graph.Dot (Arg_type.create format_of_string))
      ~doc: "Format for storing the dependency graph of the provided Puppet manifests."
    and graph_file =
      flag "graph-file" (optional string)
      ~doc: "File to store the dependency graph inferred by the compiled catalog."
    and print_time =
      flag "print-time" (no_arg)
      ~doc: "Print analysis times"
    and package_notify =
      flag "package-notify" (no_arg)
      ~doc: "Consider missing notifiers from packages to services"
    in
    fun () ->
      puppet trace_file catalog graph_file graph_format print_time package_notify
    ]


let execute_cmd =
  let open Core.Command.Let_syntax in
  Core.Command.basic
    ~summary:"Applies a Puppet Manifest and collects its system call trace."
    [%map_open
    let trace_file =
      flag "traces" (required string)
      ~doc:"Path to trace file produced by the 'strace' tool."
    and manifest =
      flag "manifest" (required string)
      ~doc: "Path to the manifest that we need to apply."
    and modulepath =
      flag "modulepath" (optional_with_default "/etc/puppet/code/modules" string)
      ~doc: "Path to the directory of the Puppet modules."
    and timeout =
      flag "timeout" (optional_with_default 300 int)
      ~doc: "Timeout for the application of the Puppet manifests."
    in
    fun () ->
      puppet_ex trace_file manifest modulepath timeout
    ]


let () =
  Core.Command.group
    ~summary:"Detecting missing dependencies and notifiers in Puppet programs"
    [
      "analyze", analyze_cmd;
      "execute", execute_cmd;
    ] |> Core.Command.run
