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


type mode =
  | Online
  | Offline
(** The mode of analysis. It's either online
    (i.e., it is performed while Puppet executing manifest)
    or offline. *)


type options =
  {mode: mode; (** Mode of analysis. *)
   manifest: string option; (** Manifest to execute. *)
   modulepath: string option; (** Path of Puppet modules. *)
   package_notify: bool; (** Consider missing notifiers related to packages. *)
   graph_file: string option; (** Output dependency graph to the specified file. *)
   graph_format: Dependency_graph.graph_format; (** Format of generated dependency graph. *)
   print_stats: bool; (** Print statistics about analysis. *)
   trace_file: string option; (** Path to system call trace file. *)
   dump_puppet_out: string option; (** Dump Puppet output to this file. *)
  }
(** Run and analyze Puppet manifest with these options. *)


type option_status =
  | Ok
  | Err of string
(** Status of user-specified options. *)


val validate_options : options -> option_status
(** Validates user-specified options. *)


val run_manifest :
  string
  -> string
  -> options
  -> unit
(** This function applies the provided Puppet manifest,
  collects its system call trace.

  The analysis of traces is online and is done while Puppet executes
  manifest. *)


val analyze_trace :
  string
  -> string
  -> options
  -> unit
(** Performs an offline analysis of system call trace.
   This function expects a file where the system call trace
   stemming from Puppet execution is stored along with the compiled catalog. *)
