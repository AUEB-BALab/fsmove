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


type graph
(** Type for Puppet's dependency graph. *)


type graph_format =
  | Dot
  | Csv
(** A type that represents different formats for storing dependency graphs. *)


type abstraction_desc =
  {
    kind:  string; (** The type of the Puppet abstraction. *)
    title: string; (** The title of the Puppet abstraction. *)
    file:  string option; (** The file where the Puppet abstraction is declared. *)
    line:  string option; (** The line where the Puppet abstraction is declared. *)
  }
(** A record that stores the details of a Puppet abstraction. *)


type abstraction_schema
(** Type that stores the details about all the declared abstractions in
 a certain Puppet catalog. *)


type graph_scan
(** Type that represents the output of the DFS algorithm.
 
 In particular, this type contains all nodes that are visited by the
 given source node. *)


type ignore_resources_t
(** This type specifies the resources that need to ignored
 in the dependency graph. *)


val build_graph :
  ?graph_format: graph_format
  -> ?graph_file: string option
  -> ?package_notify: bool
  -> string
  -> graph * abstraction_schema * ignore_resources_t
(** Processes a Puppet's catalog and builds the dependency graph. *)


val is_ignored : ignore_resources_t -> string -> bool
(** Checks whether a particular resource is ignored or not. *)


val get_abstraction_details : abstraction_schema -> string -> abstraction_desc
(** This function gets the details of a certain Puppet abstraction
 specified by a string id with regards to the provided abstraction schema. *)


val abstraction_id : abstraction_desc -> string
(** Gets an abstraction description and returns its id.
 
 The id is given by:
   <abstraction_kind>`[`<abstraction_title>`]`
 *)


val string_of_abstraction : abstraction_desc -> string
(** This function converts the provided description of
  an abstraction to a string.*)


val dfs : graph -> string -> string -> bool -> graph_scan option
(** This function implements a DFS algorithm.
 
 Given a source node on a graph, this function computes
 all nodes that are visited by the source node.

 This function returns the all the paths from the source node to
 the target. *)


val happens_before : graph -> string -> string -> graph_scan option -> bool
(** Check the first Puppet abstraction `happens-before` the second one
   with regards to the given dependency graph. *)


val notifies : graph -> string -> string -> graph_scan option -> bool
(** Check whether the first Puppet abstraction `notifies` the second one
   with regards to the given dependency graph. *)


val exists : graph -> string -> bool
(** Check whether the given abstraction exists in the provided
  dependency graph. *)


val to_dot : graph -> string -> unit
(** Output the given dependency graph to a .dot file. *)


val to_csv : graph -> string -> unit
(** Output the given dependency graph to a csv file. *)
