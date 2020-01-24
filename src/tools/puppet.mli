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


val adapt_effect : string -> Domains.syscall_effect -> (string * Domains.abstraction_effect)
(** Adapts the effect of a given resource with regards to the semantics
    of puppet. *)


val detect_bugs :
  ?graph_format: Dependency_graph.graph_format
  -> ?package_notify: bool
  -> (Domains.abstraction_effect * Syntax.syscall_desc) list Util.Strings.t
  -> string
  -> string option
  -> unit
(** Detects Missing Ordering Relationships (MOR) and Missing Notifiers (MR)
    in Puppet programs. *)
