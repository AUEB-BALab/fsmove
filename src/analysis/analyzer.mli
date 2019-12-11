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


module type ToolType =
  sig
    val adapt_effect : string -> Domains.syscall_effect -> (string * Domains.abstraction_effect)
    (** Adapts the effect on a system resource based on the given
      resource name. *)
  end


module type S =
  sig
    type resource_graph = (Domains.abstraction_effect * Syntax.syscall_desc) list Util.Strings.t
    (** This type captures the relations between
      the system resources (e.g. files, etc.) and
      the units defined by the configuration tool.
     
      For example, it captures what kind of system resources,
      every tool's unit consumes or produces. *)


    val analyze_traces : Syntax.trace Syntax.stream ->
        (resource_graph * string list)
    (** Analyzes every trace and produces a resource graph. *)
  end


module Make (T : ToolType) : S
(** A functor for building the implementation of an analyzer
    that detects missing dependencies using the dependency graph
    of the tool. *)
