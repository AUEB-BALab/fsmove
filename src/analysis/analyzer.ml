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


open Util


module type ToolType =
  sig
  val adapt_effect : string -> Domains.syscall_effect -> (string * Domains.abstraction_effect)
  end


module type S =
  sig
  type resource_graph = (Domains.abstraction_effect * Syntax.syscall_desc) list Util.Strings.t

  val analyze_traces : Syntax.trace Syntax.stream ->
    (resource_graph * string list)
  end

module Make(T: ToolType) = struct

  type resource_graph = (Domains.abstraction_effect * Syntax.syscall_desc) list Util.Strings.t


  let update_graph graph resource effects =
    List.fold_left (fun acc (effect, sdesc) ->
      let key, effect' = T.adapt_effect resource effect in
      match Strings.find_opt key acc with
      | None -> Strings.add key [effect', sdesc] acc
      | Some effects ->
        Strings.add key ((effect', sdesc) :: effects) acc
    ) graph (Domains.unique_effects effects)


  let rec _analyze_traces traces state g l =
    match Syntax.peek_trace traces with
    | Some (pid, (Syntax.End v, sdesc)) ->
      let state = Interpreter.interpret (pid, (Syntax.End v, sdesc)) state in
      _analyze_traces
        (Syntax.next_trace traces)
        (Domains.reset_effect_store state)
        (update_graph g v (Domains.get_effects state))
        (v :: l)
    | Some (pid, trace) ->
      _analyze_traces
        (Syntax.next_trace traces)
        (Interpreter.interpret (pid, trace) state)
        g
        l
    | None -> g, l


  let analyze_traces traces =
    _analyze_traces
      traces
      (Domains.init_state())
      Strings.empty
      []
end
