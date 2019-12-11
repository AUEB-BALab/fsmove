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


open Domains
open Util
open Syntax


module AbPair = Map.Make(
  struct
    type t = (Dependency_graph.abstraction_desc * Dependency_graph.abstraction_desc)
    let compare = Core.compare
  end
)


type bug_kind =
  | MOR (* The bug indicates a missing ordering relationship. *)
  | MN  (* The bug indicates a missing notifier. *)


type location = (Domains.abstraction_effect * Syntax.syscall_desc)


type bug =
  {conflict: (location * location);
   resource: string;
   kind:  bug_kind;
  }


let cache_size = 5000
(* Two caches that store the result of the `Dependency_graph.dfs`
  and `Dependency_graph.notifies` functions. *)
let dfs_cache, notify_cache = (
  Hashtbl.create cache_size,
  Hashtbl.create cache_size
)


let is_service resource =
  Util.check_prefix "Service" resource


let is_exec resource =
  Util.check_prefix "Exec" resource


let adapt_effect resource effect =
  match (effect, Puppet_util.extract_file_title resource) with
  | Domains.Read v, Some pathname
  | Domains.Touch v, Some pathname ->
    if Util.string_contains v pathname 
    then v, Domains.Produced resource
    else v, Domains.Consumed resource
  | Domains.Read    v, _
  | Domains.Touch   v, _ -> v, Domains.Consumed resource
  | Domains.Write   v, _ -> v, Domains.Modified resource
  | Domains.Create  v, _ -> v, Domains.Produced resource
  | Domains.Remove  v, _ -> v, Domains.Expunged resource


let non_consumed x =
  match x with
  | Produced _, _ | Expunged _, _ -> true
  | _                             -> false


let is_consumed x =
  match x with
  | Consumed _, _ | Modified _, _ -> true
  | _                             -> false


let get_2combinations lst =
  let rec _get_2combinations l accum =
    match l with
    | [] -> accum
    | h :: t ->
      let accum' = accum @ (List.rev_map (fun x -> (h, x)) t) in
      _get_2combinations t accum'
  in _get_2combinations lst []


let get_cartesian lst lst' = 
  List.concat (List.rev_map (fun x -> List.rev_map (fun y -> (x, y)) lst') lst)


let strip_con x =
  match x with
  | Consumed v, _
  | Modified v, _
  | Produced v, _
  | Expunged v, _ -> v


(**
 * Filters the case when a system resource is
 * consumed and produced by the same tool's unit.
 *)
let get_cartesian_and_filter lst lst' =
  List.filter (fun (x, y) ->
    (strip_con x) <> (strip_con y)
  ) (get_cartesian lst lst')


let to_string_elem x =
  let msg, sdesc = (
    match x with
    | Consumed x, d -> "Consumed by " ^ x, d
    | Modified x, d -> "Modified by " ^ x, d
    | Produced x, d -> "Produced by " ^ x, d
    | Expunged x, d -> "Expunged by " ^ x, d
  ) in
  String.concat " " [
    msg;
    "(";
    sdesc.syscall;
    "at line";
    (string_of_int sdesc.line);
    ")";
  ]


let to_string_pairs pairs =
  match List.fold_left (fun (i, str) {conflict = (x, y); resource = resource; _; } ->
    (i + 1,
    String.concat "" [
      str;
      "      - ";
      resource;
      ": ";
      to_string_elem x;
      " and ";
      to_string_elem y;
      "\n";
    ])) (1, "") pairs
  with
  | (_, s) -> s


(* A function that is used to split a list of Puppet bugs
 into two different lists based on their kind
 (e.g., one list that contains only MOR bugs) *)
let partition_bugs bugs =
  AbPair.fold (fun k bugs (mor_bugs, mn_bugs) ->
    match
      List.fold_left (fun (mors, mns) bug ->
        match bug.kind with
        | MOR -> bug :: mors, mns
        | MN  -> mors, bug :: mns 
      ) ([], []) bugs
    with
    | [], []    -> mor_bugs, mn_bugs
    | mors, []  -> AbPair.add k mors mor_bugs, mn_bugs
    | [], mns   -> mor_bugs, AbPair.add k mns mn_bugs
    | mors, mns -> 
      AbPair.add k mors mor_bugs,
      AbPair.add k mns mn_bugs
  ) bugs (AbPair.empty, AbPair.empty)


let report_bug_details bugs =
  Printf.printf "# Faults: %d\n" (AbPair.cardinal bugs);
  print_string "Pairs:\n";
  let _ = AbPair.fold (fun (x, y) z i ->
    let report = String.concat "" [
      "  * ";
      Dependency_graph.string_of_abstraction x;
      "\n";
      "  * ";
      Dependency_graph.string_of_abstraction y;
      " => \n";
      "      Conflict on ";
      z |> List.length |> string_of_int;
      " resources:\n";
      to_string_pairs z;
      "\n";
    ] in
    print_string report;
    i + 1
  ) bugs 1
  in ()


let report_bugs bugs =
  let mor_title = "Missing Ordering Relationships:\n===============================\n" in
  let mn_title = "Missing Notifiers:\n==================\n" in
  let _report_bugs title bugs =
    if AbPair.cardinal bugs <> 0
    then
      begin
        print_string title;
        report_bug_details bugs;
      end
    else ()
  in
  let mors, mns = partition_bugs bugs in
  begin
    _report_bugs mor_title mors;
    _report_bugs mn_title mns;
  end


let ignore_resource resource =
  String.equal "/dev/null" resource ||
  String.equal "/etc/shadow" resource ||
  Util.check_prefix "/var/lib/puppet" resource ||
  Util.check_prefix "/proc" resource ||
  String.equal "/etc/shadow" resource ||
  String.equal "/etc/gshadow" resource ||
  String.equal "/etc/default/locale" resource ||
  String.equal "/etc/locale.gen" resource ||
  String.equal "/etc/logrotate.d" resource


let filter_conflict x y =
  Util.check_prefix "Package" x && Util.check_prefix "Package" y ||
  (Util.check_prefix "User" x || Util.check_prefix "User" y) ||
  (Util.check_prefix "File" x && Util.check_prefix "File" y) ||
  (String.equal "File[/usr/sbin]" x || String.equal "File[/usr/sbin]" y) ||
  (String.equal "File[/usr/bin]" x || String.equal "File[/usr/bin]" y) ||
  (String.equal "File[/bin]" x || String.equal "File[/bin]" y) ||
  (String.equal "File[/sbin]" x || String.equal "File[/sbin]" y) ||
  (String.equal "File[/var]" x || String.equal "File[/var]" y) ||
  (String.equal "File[/etc]" x || String.equal "File[/etc]" y) ||
  (String.equal "File[/usr]" x || String.equal "File[/usr]" y) ||
  (Util.check_prefix "Apt_key" x && String.equal "File[/etc/apt/apt.conf.d/15update-stamp]" y) ||
  (String.equal "File[/etc/apt/apt.conf.d/15update-stamp]" x && Util.check_prefix "Apt_key" y) ||
  (String.equal "Package[apt-transport-https]" x || String.equal "Package[apt-transport-https]" y)


let adapt_file file graph =
  match Puppet_util.extract_file_title file with
  | None          -> file
  | Some filename ->
      let res = "File[" ^ (Core.Filename.dirname filename) ^ "]" in
      if Dependency_graph.exists graph res
      (* We use the parent directory (if it is declared in the catalog)
        to check the happens-before relation between the Package abstraction. *)
      then res
      else file


let adapt_file_package_pair x y graph =
  if Util.check_prefix "Package" x && Util.check_prefix "File" y
  then adapt_file y graph, x
  else
    if Util.check_prefix "File" x && Util.check_prefix "Package" y
    then adapt_file x graph, y
    else x, y


let construct_err resource conflict bug_kind =
  {conflict = conflict;
   resource = resource;
   kind     = bug_kind;
  }


let get_abstraction_details x y schema =
  let x, y = strip_con x, strip_con y in
  Dependency_graph.get_abstraction_details schema x,
  Dependency_graph.get_abstraction_details schema y


let add_bug resource (x, y) bug_type schema bugs =
  let err = construct_err resource (x, y) bug_type in
  match get_abstraction_details x y schema with
  (* Ignore unspecified Puppet resources. *)
  | { Dependency_graph.kind = "-"; _ }, _
  | _, { Dependency_graph.kind = "-"; _ } -> bugs
  | x, y ->
    match (
      AbPair.find_opt (x, y) bugs,
      AbPair.find_opt (y, x) bugs
    ) with
    | None, None     -> AbPair.add (x, y) [err] bugs
    | Some l, None   -> AbPair.add (x, y) (err :: l) bugs
    | None, Some l   -> AbPair.add (y, x) (err :: l) bugs
    | Some _, Some _ -> bugs


let notification_bug ignored_resources conflict =
  match conflict with
  | (Produced x, _), (Consumed y, _) 
  | (Consumed y, _), (Produced x, _) ->
      let ignored = Dependency_graph.is_ignored ignored_resources in
      if is_service y && not (is_exec x) && not (ignored x)
      then Some (x, y)
      else None
  | _ -> None


let add_mor_bug ignored_resources resource conflict schema bugs =
  match notification_bug ignored_resources conflict with
  | None -> add_bug resource conflict MOR schema bugs
  | _    -> bugs


let add_notification_bug ignored_resources func resource conflict schema bugs =
  match notification_bug ignored_resources conflict with
  | None        -> bugs
  | Some (x, y) ->
    match func with
    | None              -> add_bug resource conflict MN schema bugs
    | Some not_notifies ->
      if not_notifies x y
      then add_bug resource conflict MN schema bugs
      else bugs


let visit_nodes graph x y enum_paths cache =
  match Hashtbl.find_opt cache (x, y, enum_paths) with
  | None     ->
      let out = Dependency_graph.dfs graph x y enum_paths in
      Hashtbl.add cache (x, y, enum_paths) out;
      out
  | Some out -> out


let process_conflicts (dep_graph, schema, ignored_resources) resource conflicts bugs =
  List.fold_left (fun bugs conflict ->
    match conflict with
    | (Produced x, _), (Consumed y, _)
    | (Produced x, _), (Produced y, _)
    | (Produced x, _), (Expunged y, _)
    | (Produced x, _), (Modified y, _)
    | (Consumed x, _), (Produced y, _)
    | (Consumed x, _), (Expunged y, _)
    | (Expunged x, _), (Produced y, _)
    | (Expunged x, _), (Modified y, _)
    | (Expunged x, _), (Consumed y, _) ->
      let x', y' = adapt_file_package_pair x y dep_graph in
      if filter_conflict x' y'
      then bugs
      else (
        match (
          visit_nodes dep_graph x' y' false dfs_cache,
          visit_nodes dep_graph y' x' false dfs_cache)
        with
        | None, _ | _, None -> bugs
        | vx, vy            ->
          if not (Dependency_graph.happens_before dep_graph x' y' vx ||
              Dependency_graph.happens_before dep_graph y' x' vy)
          then
            bugs
              |> add_mor_bug ignored_resources resource conflict schema
              |> add_notification_bug ignored_resources None resource conflict schema
          else
            (* A function used to check if x notifies y. *)
            let notify_check x y =
              match Hashtbl.find_opt notify_cache (x, y) with
              | None ->
                  let out = Dependency_graph.notifies dep_graph x y None in
                  Hashtbl.add notify_cache (x, y) out;
                  not out
              | Some out -> not out
            in
            add_notification_bug ignored_resources (Some notify_check)
              resource conflict schema bugs
      )
    | _ -> bugs
  ) bugs conflicts


let process_resource graph_abstr resource effects bugs =
  if ignore_resource resource
  then bugs
  else
    match (
      List.filter non_consumed effects,
      List.filter is_consumed effects
    ) with
    | [], _ -> bugs (* No conflict detected for the given resource. *)
    | non_consumed, consumed ->
      match (get_2combinations non_consumed) @ (
        get_cartesian_and_filter non_consumed consumed)
      with
      | []        -> bugs
      | conflicts -> process_conflicts graph_abstr resource conflicts bugs


let detect_bugs ?(graph_format=Dependency_graph.Dot) ?(package_notify=false)
                state catalog_file graph_file =
  let resource_processor = catalog_file
    |> Dependency_graph.build_graph ~graph_format ~graph_file ~package_notify
    |> process_resource
  in
  let bugs = Strings.fold (fun resource effects bugs ->
    resource_processor resource effects bugs) state AbPair.empty
  in
  report_bugs bugs
