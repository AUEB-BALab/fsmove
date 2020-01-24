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


open Yojson

open Errors
open Util


let make_puppet_error msg =
  raise (Error (ToolError, msg))


let make_internal_error msg =
  raise (Error (InternalError, msg))


type relationship =
  | Contain
  | Before
  | Notify
  | Include


type graph_format =
  | Dot
  | Csv


module EdgeSet = Set.Make(
  struct
    type t = (string * relationship)
    let compare = Core.compare
  end
)


module LabelSet = Set.Make(
  struct
    type t = relationship
    let compare = Core.compare
  end
)


type graph_scan = string list list


type abstraction_desc =
  {kind:  string;
   title: string;
   file:  string option;
   line:  string option;
  }


type graph = EdgeSet.t Strings.t


type abstraction_schema = abstraction_desc Strings.t


type ignore_resources_t = StringSet.t


let is_ignored ignored_resources resource =
  StringSet.exists (fun elem -> resource = elem) ignored_resources


let get_abstraction_details schema key =
  match Strings.find_opt key schema with
  | Some desc -> desc
  | None      ->
    (* If we are not able to find the description of
     * the given resource in the catalog, return the
     * default one. *)
    {kind  = "-";
     title = "-";
     file  = None;
     line  = None
    }


let abstraction_id { kind = kind; title = title; _ } =
  kind ^ "[" ^ title ^ "]"


let string_of_abstraction abstraction =
  match abstraction with
  | { file = Some file; line = Some line; _ } ->
    (abstraction_id abstraction) ^ ": " ^ file ^ ": " ^ line
  | _ -> abstraction_id abstraction


let add_node graph node =
  match Strings.find_opt node graph with
  | None -> Strings.add node EdgeSet.empty graph
  | _    -> graph


let add_abstraction schema key desc =
  Strings.add key desc schema


let add_edge graph source target relationship =
  let graph = add_node graph target in (* Adds target node if it does not exist. *)
  match Strings.find_opt source graph, relationship with
  | Some edges, Notify -> (
    match EdgeSet.find_opt (target, Before) edges with
    | None ->
      Strings.add source (EdgeSet.add (target, relationship) edges) graph
    | _ ->
      (* If the set contains a `before` relationship, we override it with
         a `notify` relationship. *)
      let edges = EdgeSet.remove (target, Before) edges in
      Strings.add source (EdgeSet.add (target, Notify) edges) graph
  )
  | Some edges, _ ->
    Strings.add source (EdgeSet.add (target, relationship) edges) graph
  | None, _ ->
    Strings.add source (EdgeSet.singleton (target, relationship)) graph


let get_resource_name resource =
  String.concat "" [
    resource |> Basic.Util.member "type"  |> Basic.Util.to_string;
    "[";
    resource |> Basic.Util.member "title" |> Basic.Util.to_string;
    "]";
  ] 


let extract_line resource =
  match resource |> Basic.Util.member "line" with
  | `Null     -> None
  | `Int line -> Some (string_of_int line)
  | _         -> None


let extract_file resource =
  match resource |> Basic.Util.member "file" with
  | `Null        -> None
  | `String file -> Some file
  | _            -> None


let get_abstraction resource =
  {
    kind  = resource |> Basic.Util.member "type"  |> Basic.Util.to_string;
    title = resource |> Basic.Util.member "title" |> Basic.Util.to_string;
    file  = extract_file resource;
    line  = extract_line resource;
  }


let process_params source parameters key graph =
  match parameters with
  | `Null -> graph
  | _     ->
    let _process_param graph param_value =
      match key, Basic.Util.to_string_option param_value with
      | "notify", Some target    -> add_edge graph source target Notify
      | "before", Some target    -> add_edge graph source target Before
      | "require", Some target   -> add_edge graph target source Before
      | "subscribe", Some target -> add_edge graph target source Notify
      | _, _                     -> make_internal_error (Some "Unreachable case.")
    in
    match parameters |> Basic.Util.member key with
    | `List parameters ->
        List.fold_left (fun acc param -> _process_param acc param) graph parameters
    | `Null -> graph
    | param_value -> _process_param graph param_value


let extract_paths resource key parameters =
  match Basic.Util.member key parameters with
  | `Null        -> (
      match Puppet_util.extract_file_title resource with
      | None      -> None
      | Some path -> Some [(path, Core.Filename.dirname path)])
  | `String path -> Some [(path, Core.Filename.dirname path)]
  | `List paths  ->
    Some (List.fold_left (fun acc x ->
      match Basic.Util.to_string_option x with
      | None      -> make_internal_error (Some "Unreachable case.\n")
      | Some path -> (path, Core.Filename.dirname path) :: acc
    ) [] paths)
  | _ -> make_internal_error (Some "Unreachable case.")


let process_package_resource source package_notify ignored_resources =
  if not package_notify && Util.check_prefix "Package" source
  then source ++ ignored_resources
  else ignored_resources


let process_file_resource source parameters file_info ignored_resources graph =
  match parameters with
  | `Null -> file_info, ignored_resources, graph
  | _     ->
    let _process_path (file_info, ignored_resources, graph) (path, dirname) =
      match Strings.find_opt dirname file_info with
      | None          -> Strings.add path source file_info, ignored_resources, graph
      | Some resource ->
        file_info,
        resource ++ ignored_resources,
        add_edge graph resource source Include
    in
    match
      if Util.check_prefix "File" source
      then extract_paths source "path" parameters
      else
        if Util.check_prefix "Exec" source
        then extract_paths source "creates" parameters
        else None
    with
    | None                 -> file_info, ignored_resources, graph
    | Some paths           ->
      List.fold_left _process_path (file_info, ignored_resources, graph) paths


(* This function creates edges between nodes that are
 included in a parent abstraction and the nodes
 which the parent abstraction is connected with.
 
 For example, in the following scenario:
  Class[install] before Class[config]
  Class[install] contains Package[foo]

 we have:
   Package[foo] before Class[config]
 *)
let propagate_edges source target graph =
  match Strings.find_opt source graph with
  | None       -> make_internal_error (Some "Unreachable case.")
  | Some edges ->
    if EdgeSet.is_empty edges
    (* We do not need to connect target with the nodes with which
      the source node is connected. *)
    then graph
    else
      EdgeSet.fold (fun (node, rel) acc ->
        match rel with
        | Contain -> acc (* We do not take into account `Contain` edges. *)
        | _       -> add_edge acc target node rel
      ) edges graph


let process_edges graph edges =
  match edges with
  | `List edges ->
    List.fold_left (fun acc edge ->
      let source = edge |> Basic.Util.member "source" |> Basic.Util.to_string in
      let target = edge |> Basic.Util.member "target" |> Basic.Util.to_string in
      Contain
        |> add_edge acc source target
        |> propagate_edges source target
    ) graph edges
  | _ -> make_puppet_error (Some "`edges` key must be an array.")


let process_resources package_notify out resources =
  match resources with
  | `List resources ->
    List.fold_left (fun (graph, schema, file_info, ignored_resources) resource ->
      let source, abstraction = (
        get_resource_name resource,
        get_abstraction resource)
      in
      let parameters = Basic.Util.member "parameters" resource in
      let processor = process_params source parameters in
      let file_info, ignored_resources, graph = graph
        |> processor "require"
        |> processor "before"
        |> processor "notify"
        |> processor "subscribe"
        |> process_file_resource source parameters file_info ignored_resources
      in
      let ignored_resources =
        process_package_resource source package_notify ignored_resources
      in
      graph,
      add_abstraction schema source abstraction,
      file_info,
      ignored_resources
    ) out resources
  | _ -> make_puppet_error (Some "`resources` key must be an array.")


let string_of_label label =
  match label with
  | Contain -> "contain"
  | Before  -> "before"
  | Notify  -> "notify"
  | Include -> "include"


let save_to_file file str =
  begin
    let out = open_out file in
    output_string out str;
    close_out out;
  end


let to_dot graph file =
  let add_brace str =
    str ^ "}"
  in
  "digraph {"
    |> Strings.fold (fun source edges acc ->
        EdgeSet.fold (fun (target, label) acc' ->
          String.concat "" [
            acc';
            to_quotes source;
            " -> ";
            to_quotes target;
            "[label=";
            string_of_label label;
            "];\n"
          ]
        ) edges (acc ^ ((to_quotes source) ^ ";\n"))
      ) graph
    |> add_brace
    |> save_to_file file


let to_csv graph file =
  ""
    |> Strings.fold (fun source edges acc ->
        EdgeSet.fold (fun (target, label) acc' ->
          String.concat "" [
            acc';
            source;
            ",";
            target;
            ",";
            string_of_label label;
            "\n";
          ]
        ) edges acc) graph
    |> save_to_file file


let build_graph ?(graph_format=Dot) ?(graph_file=None)
                ?(package_notify=false) catalog_file =
  let buf = open_in catalog_file in
  let json = Basic.from_channel buf in
  close_in buf;
  let graph, schema, _, ignored_resources =
    json
    |> Basic.Util.member "resources"
    |> process_resources package_notify (Strings.empty, Strings.empty,
                                         Strings.empty, StringSet.empty)
  in
  let graph = process_edges graph (Basic.Util.member "edges" json) in
  match graph_file, graph_format with
  | None, _        -> graph, schema, ignored_resources
  | Some file, Dot ->
    to_dot graph file;
    graph, schema, ignored_resources
  | Some file, Csv ->
    to_csv graph file;
    graph, schema, ignored_resources


(* A generic function that implements a DFS algorith.

 The output of this function is the list of paths from

 If the parameter `enum_paths` is None, the algorithm becomes `lightweight`
 and simply returns a single path.

 The function is tail-recursive.
*)
let dfs_generic graph source target enum_paths =
  let rec _dfs paths visited stack =
    match stack with
    | [] -> paths
    | (node, prev) :: stack ->
      let path = node :: prev in
      if node = target
      then
        (* If `enum_paths` is true, we need to find all
           paths that reach target. *)
        if enum_paths
        then _dfs (path :: paths) visited stack
        else path :: paths
      else
        let edges = Strings.find node graph in
        match StringSet.find_opt node visited with
        | None ->
          edges
            |> EdgeSet.elements
            |> List.fold_left (fun acc (node, _) -> (node, path) :: acc) stack
            |> _dfs paths (node ++ visited)
        | Some _ ->
          if enum_paths
          then
            (* If `enum_paths` is true, we need to revisit nodes
               in order to compute new paths. *)
            edges
              |> EdgeSet.elements
              |> List.filter (fun (_, x) -> not (x = Include))
              |> List.fold_left (fun acc (node, _) ->
                if Util.has_elem acc (node, path)
                then acc
                else (node, path) :: acc
              ) stack
              |> _dfs paths visited
          else _dfs paths visited stack
  in
  _dfs [] StringSet.empty [(source, [])]


let dfs graph source target enum_paths =
  try
    let paths = dfs_generic graph source target enum_paths in
    Some paths
  with Not_found -> None


let exists graph abstraction =
  match Strings.find_opt abstraction graph with
  | None   -> false
  | Some _ -> true


let compute_dfs_out graph dfs_out source target enum_paths =
  match dfs_out with
  | None         -> dfs graph source target enum_paths
  | dfs_out      -> dfs_out


let happens_before graph source target dfs_out =
  match compute_dfs_out graph dfs_out source target false with
  | None    -> true
  (* There is not any path from the source to the target*)
  | Some [] -> false
  | _       -> true


let is_contain_or_notify x =
  x = Contain || x = Notify


(* A helper function that converts a list to a list of pairs as follows:

  [1, 2, 3, 4] -> [(1, 2), (2, 3), (3, 4)]. *)
let to_pairs path =
  let rec _to_pairs acc path =
    match path with
    | []
    | [_]           -> acc
    | x :: (y :: t) ->
      let acc' = (y, x) :: acc in
      _to_pairs acc' (y :: t)
  in
  _to_pairs [] path


(* A function that checks that all the edges of the provided path
 is either `Notify` or `Contain`. *)
let is_notification_path graph path =
  let edges = to_pairs path in
  List.for_all (fun (x, y) ->
    match Strings.find_opt x graph with
    | None       -> make_internal_error (Some ("Unreachable case."))
    | Some edges ->
      EdgeSet.exists (fun (node, label) ->
        is_contain_or_notify label && node = y
      ) edges
  ) edges


let notifies graph source target dfs_out =
  match compute_dfs_out graph dfs_out source target true with
  | None       -> true
  (* Since there is not any path from the source to the target,
    it is guaranteed that there is not notification relationship
    between the provided abstractions. *)
  | Some []    -> false
  | Some paths ->
    (* There must be exist at least one path where all
     its edges have `Notify` or `Contain` labels. *)
    List.exists (fun path -> is_notification_path graph path) paths
