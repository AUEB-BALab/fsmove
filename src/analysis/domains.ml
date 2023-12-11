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


open Util


exception DomainError of string


module INodeT = Map.Make(
  struct
    type t = (int * string)
    let compare = Core.Poly.compare
  end
)


module RelSet = Set.Make(
  struct
    type t = Syntax.eff
    let compare = Core.Poly.compare
  end
)


type inode_occ =
  | Linked of int
  | Unlinked of int


type syscall_effect =
  | Create of string
  | Read of string
  | Remove of string
  | Touch of string
  | Write of string


type abstraction_effect =
  | Consumed of string
  | Modified of string
  | Produced of string
  | Expunged of string


type effect = (syscall_effect * Syntax.syscall_desc)


type addr_t = int


type inode = int


type process = string


type fd = string


type filename = string


type inode_store = (inode INodeT.t * StringSet.t Ints.t)


type proc_store = (int * int) Strings.t


type proc_fd_store = inode option Strings.t


type fd_store = proc_fd_store Ints.t


type cwd_store = inode Ints.t


type symlink_store = filename Ints.t


type effect_cache = RelSet.t Strings.t


type effect_store = (effect list * effect_cache)


type opened_inodes = inode_occ Ints.t


type execution_block = string option


type parent_process = string option


type state = 
  {
   k:  proc_store;
   r:  fd_store;
   c:  effect_store;
   s:  symlink_store;
   d:  cwd_store;
   i:  inode_store;
   g:  int Stream.t;
   p:  opened_inodes;
   b:  execution_block;
   z:  parent_process;
  }


let cache_size = 1000


let gen_inode state =
  Stream.next state.g


let gen_addr state =
  Stream.next state.g


(* Functions that perform queries to the structures of the state. *)
let find_from_cwdtable pid proc_table cwd_table =
  match Strings.find_opt pid proc_table with
  | None           -> None
  | Some (addr, _) -> Ints.find_opt addr cwd_table


let find_from_symtable inode sym_table =
  Ints.find_opt inode sym_table


let find_proc_fdtable pid proc_table fd_table =
  match Strings.find_opt pid proc_table with
  | None           -> None
  | Some (_, addr) -> Ints.find_opt addr fd_table


let find_from_fdtable pid fd proc_table fd_table =
  match Strings.find_opt pid proc_table with
  | None           -> None
  | Some (_, addr) -> Strings.find fd (Ints.find addr fd_table)


let find_from_inodetable inode_p base (inode_table, _) =
  INodeT.find_opt (inode_p, base) inode_table


let find_from_proctable pid proc_table =
  Strings.find_opt pid proc_table


(* Functions that perform additions to the structures of the state. *)
let add_to_cwdtable pid inode proc_table cwd_table =
  match Strings.find_opt pid proc_table with
  | None           -> cwd_table
  | Some (addr, _) -> Ints.add addr inode cwd_table


let add_to_fdtable pid fd inode proc_table fd_table =
  match Strings.find_opt pid proc_table with
  | None           -> fd_table
  | Some (_, addr) ->
    match Ints.find_opt addr fd_table with
    | None   ->  Ints.add addr (Strings.singleton fd inode) fd_table
    | Some f ->  Ints.add addr (Strings.add fd inode f) fd_table


let add_to_symtable inode path sym_table =
  Ints.add inode path sym_table


let add_to_rev_inodetable inode path rev_inode_table =
  match Ints.find_opt inode rev_inode_table with
  | None       -> Ints.add inode (~+ path) rev_inode_table
  | Some paths -> Ints.add inode (path ++ paths) rev_inode_table


let strip_trailing_slash path =
  let len = String.length path in
  if len > 0 && path.[len - 1] = '/'
  then String.sub path 0 (len - 1)
  else path


let add_to_inodetable inode_p base inode path (inode_table, rev_inode_table) =
  (* Strip any traling slashes at the end of file names.*)
  let path = strip_trailing_slash path in
  if inode_p = inode
  then
    raise (DomainError ("The parent inode is the same with the inode of the child"))
  else
    match Ints.find_opt inode_p rev_inode_table with
    | None   -> raise (DomainError "The parent inode does not exist")
    | Some p ->
      match ~@ p with
      | [p] ->
        (* The path corresponding to the parent inode `inode_p`
          must be the same with parent directory of the given
          argument `path`.

          Also the basename which stem from the variable `path`
          must be the same with the value of the parameter `base`. *)
        let dir, b = Core.Filename.dirname path, Core.Filename.basename path in
        if String.equal p dir && String.equal base b
        then
          INodeT.add (inode_p, base) inode inode_table,
          add_to_rev_inodetable inode path rev_inode_table
        else
          raise (DomainError ("The value of `path` is incompatible with the given `inode_p` and `base`"))
      | _  ->
          raise (DomainError "Multiple paths are linked with the parent inode")


(* Functions that perform deletions to the structures of the state. *)
let remove_from_fdtable pid fd proc_table fd_table =
  match Strings.find_opt pid proc_table with
  | None           -> fd_table
  | Some (_, addr) -> Ints.add addr (
    Strings.remove fd (Ints.find addr fd_table)) fd_table


(* Removes a path that points to a particular inode.

  That path must be placed inside the directory given as an argument. *)
let remove_from_rev_it inode path rev_inode_table =
  match Ints.find_opt inode rev_inode_table with
  | None -> rev_inode_table
  | Some paths ->
    let paths = path +- paths in
    if StringSet.is_empty paths
    then 
      (* The inode is not pointed by any file, so we remove it. *)
      Ints.remove inode rev_inode_table
    else Ints.add inode paths rev_inode_table


let remove_from_inodetable inode_p basename inode path (inode_table, rev_inode_table) =
  INodeT.remove (inode_p, basename) inode_table,
  remove_from_rev_it inode path rev_inode_table

  
let init_proc_fdtable addr fd_table =
  Ints.add addr Strings.empty fd_table


let init_proc_cwdtable addr cwd_table =
  Ints.add addr 1 cwd_table


let add_to_proctable pid cwd_addr fd_addr proc_table =
  Strings.add pid (cwd_addr, fd_addr) proc_table


let copy_cwdtable pid addr proc_table cwd_table =
  match Strings.find_opt pid proc_table with
  | None               -> cwd_table
  | Some (old_addr, _) ->
    Ints.add addr (Ints.find old_addr cwd_table) cwd_table


let copy_fdtable pid addr proc_table fd_table =
  match Strings.find_opt pid proc_table with
  | None               -> fd_table
  | Some (_, old_addr) -> Ints.add addr (Ints.find old_addr fd_table) fd_table


let copy_fd pid f1 f2 proc_table fd_table =
  add_to_fdtable pid f2 (find_from_fdtable pid f1 proc_table fd_table)
    proc_table fd_table


(* Function to convert a path to an inode. *)
let rec to_inode path state =
  match path with
  | "/" -> 2, state
  | path ->
    let d = Core.Filename.dirname path in
    let b = Core.Filename.basename path in
    let inode_p, state' = to_inode d state in
    match find_from_inodetable inode_p b state'.i with
    | None ->
      let inode = gen_inode state' in
      inode, { state' with
               i  = add_to_inodetable inode_p b inode path state'.i; }
    | Some inode -> inode, state'


(* This functions gets all the paths that point to the given inode. *)
let to_paths { i = (_, ri); _} inode =
  match Ints.find_opt inode ri with
  | None ->
    raise (DomainError ("Inode " ^ (string_of_int inode) ^ " not found"))
  | Some paths -> ~@ paths


(** This function converts the given inode to a path.
 The assumption of this function is that the given inode is pointed
 by a single path (i.e., a directory). *)
let to_path_singleton inode state =
  match to_paths state inode with
  | [path] -> path (* An inode is always pointed by a single directory. *)
  | _ -> raise (DomainError (
    "Inode pointed by multiple directories found."))


let to_paths_set { i = (_, ri); _ } inode =
  Ints.find_opt inode ri


(* Operations on inode table. *)
let remove_inode state inode_p basename inode =
  match Ints.find_opt inode state.p with
  | None ->
    (* The inode is not opened anywhere
       so we add it to the list of the removed inodes. *)
    let path = Core.Filename.concat (to_path_singleton inode_p state) basename in
    { state with
      i = remove_from_inodetable inode_p basename inode path state.i; }
  | Some (Linked v) ->
    (* The inode is opened by a handler, so we mark it
       as unlinked in order to be removed on close. *)
    { state with
      p = Ints.add inode (Unlinked v) state.p }
  | Some (Unlinked _) -> state


let unlink_resource inode basename state =
  match find_from_inodetable inode basename state.i with
  | None        -> state (* The inode entry was not found. *)
  | Some inode' -> remove_inode state inode basename inode'


(* Operations on the structure that holds inodes which are
   currently opened by the open handler. *)
let open_inode inode opened_inodes =
  match Ints.find_opt inode opened_inodes with
  | None -> Ints.add inode (Linked 1) opened_inodes
  | Some (Linked v) | Some (Unlinked v) ->
    Ints.add inode (Linked (v + 1)) opened_inodes


let close_inode inode state =
  match Ints.find_opt inode state.p with
  (* The given inode is not used anywhere, so we do nothing. *)
  | None | Some (Linked 0) | Some (Unlinked 0) -> state
  (* The given inode is only used by the current handler.
     We just remove it. *)
  | Some (Linked 1) -> { state with p = Ints.remove inode state.p}
  (* The given inode is also used by other handlers..
     We remove the usage counter. *)
  | Some (Linked v) ->
    { state with p = Ints.add inode (Linked (v - 1)) state.p }
  (* The given inode is orphaned; and it's used only by
     the current handler.
     Therefore, it's time to remove it from the inode table. *)
  | Some (Unlinked 1) -> (
    match to_paths_set state inode with
    | Some paths -> (
      match ~@ paths with
      | [path] ->
        let dir, base = (
          Core.Filename.dirname path,
          Core.Filename.basename path) in
        let inode_p, state = to_inode dir state in
        let state = { state with p = Ints.remove inode state.p } in
        remove_inode state inode_p base inode
      | _ -> { state with p = Ints.remove inode state.p }
    )
    | _ -> { state with p = Ints.remove inode state.p }
  )
  (* The given inode is orphaned so, but it's used by other handlers.
     We decrement the usage counter.*)
  | Some (Unlinked v) ->
    { state with p = Ints.add inode (Unlinked (v - 1)) state.p }


(* Functions that operate on the effect store. *)
let add_effect_to_cache cache x effect =
  match Strings.find_opt x cache with
  | None     -> Strings.add x (RelSet.add effect RelSet.empty) cache 
  | Some set -> Strings.add x (RelSet.add effect set) cache


let add_effect (lst, cache) (elem, sdesc) =
  match elem with
  | Create x ->
    (elem, sdesc) :: lst, add_effect_to_cache cache x Syntax.Produce
  | Read x | Touch x | Write x ->
    (elem, sdesc) :: lst, add_effect_to_cache cache x Syntax.Consume 
  | Remove x ->
    (elem, sdesc) :: lst, add_effect_to_cache cache x Syntax.Expunge 


let add_rename_effect (lst, cache) (pathname, sdesc) =
  match Strings.find_opt pathname cache with
  | None         -> (Create pathname, sdesc) :: lst, cache
  | Some effects ->
    match RelSet.find_opt Syntax.Produce effects with
    (* If we did not produce previously the file,
       it means that it already existed.

       So we add a write effect. *)
    | None -> (Write pathname, sdesc) :: lst, cache
    | _    -> lst, cache


let init_inode_table () =
  (* We initialize the inode table.
     The initial inode table contains the root directory '/'.
     By convention, the inode of the root directory is 2.

     Also, we initialize the inode table with one more virtual resource
     named '/UNKNOWN' to map all those resources for which we don't
     know their path names, e.g., unlink(0x7ffc8e1eb020). *)
  let inode_table = INodeT.add (-1, "/") 2 INodeT.empty in
  INodeT.add (2, "UNKNOWN") 1 inode_table


let init_proc_store () = Strings.empty


let init_fd_store () = Ints.empty


let init_cwd_store () = Ints.empty


let init_symlink_store () = Ints.empty


let init_effect_store () = ([], Strings.empty)


let init_int_stream () =
  (* Initialize a stream of integers to be used as inodes.
     We start from 3 because 2 is alreasy reseved by the root
     directory. *)
  Util.int_stream 3


let init_opened_inodes () =
  Ints.empty


let reset_effect_store state =
  { state with c = init_effect_store () }


let init_reversed_inode_store () =
  let r_inode_store = Ints.add 2 (~+ "/") Ints.empty in
  Ints.add 1 (~+ "/UNKNOWN") r_inode_store


let init_state () =
  {
    k = init_proc_store ();
    r = init_fd_store ();
    c = init_effect_store ();
    s = init_symlink_store ();
    d = init_cwd_store ();
    i = (init_inode_table (), init_reversed_inode_store());
    g = init_int_stream ();
    p = init_opened_inodes ();
    b = None;
    z = None;
  }


let get_effects state =
  match state.c with
  | x, _ -> x


let strip_con x =
  match x with
  | Create v | Read v | Touch v | Remove v | Write v -> v


let unique_effects effects =
  (* Adds a cache that remembers paths that
     have been processed previously. *)
  let cache = Hashtbl.create cache_size in
  List.fold_left (fun acc (x, d) ->
    match x, Hashtbl.find_opt cache (strip_con x) with
    | _, None ->
      Hashtbl.add cache (strip_con x) (x, d);
      (x, d) :: acc
    | Create _, Some _
    | Read _,  Some _
    | Touch _, Some _
    | (Write _, Some (Write _, _))
    | (Write _, Some (Create _, _)) -> acc
    | Write u, Some _ ->
      Hashtbl.replace cache u (Write u, d);
      (Write u, d) :: acc
    | Remove u, Some _ ->
      (* If we expunge the resource, all the previous
         effects on that resource have not meaning.
         So we remove them.

         Also, we remove all the resources which start with
         the name of the removed resource.

         This captures the case when we remove a directory.

         rmdir("/foo/bar")

         Apparently, in this case, we also need to remove all the
         resources included in that directory.
         *)
      Hashtbl.remove cache u;
      List.filter (fun (x, _) ->
        match x with
        | Create v | Read v | Touch v | Write v | Remove v ->
          not (Core.String.equal u v)) acc
  ) [] (List.rev effects)


(* Helper functions used during interpretation. *)
let get_cwd pid state =
  match find_from_cwdtable pid state.k state.d with
  (* Perphaps, it's the case when early take place
     and we don't have the information about the current working
     directory of the process. *)
  | None     -> "/CWD"
  | Some icwd -> to_path_singleton icwd state


let get_parent_dir pid state d =
  match d with
  (* It's not an *at call, so we get the current working directory. *)
  | Syntax.AT_FDCWD                               -> Some (get_cwd pid state)
  | Syntax.Fd "0" | Syntax.Fd "1" | Syntax.Fd "2" -> None
  | Syntax.Fd dirfd                               ->
    match find_from_fdtable pid dirfd state.k state.r with
    | Some inode          -> Some (to_path_singleton inode state)
    | None                -> None
    | exception Not_found -> None


(**
 * Extract and generate the absolute path name from the arguments
 * of system call.
 *
 * If the given path name is absolute, then this function returns it verbatim.
 * Otherwise, it extracts the dirfd argument:
   - If it is AT_FDCWD constant, then we interpret the given path name relative
     to the current working directory.
   - Otherwise, we inspect the directory corresponding to the given dirfd.
 *)
let get_pathname pid state d p =
  match p with
  | Syntax.Unknown _     -> Some p
  | Syntax.Path pathname ->
    if is_absolute pathname then Some p
    else
      match (
        pathname,
        get_parent_dir pid state d
      ) with
      | _, None -> None
      (* Get the current directory, e.g. /foo/. -> /foo *)
      | ".", Some cwd -> Some (Syntax.Path cwd)
      (* Get the parent directory, e.g. /foo/bar/.. -> /foo/bar *)
      | "..", Some cwd -> Some (Syntax.Path (Core.Filename.dirname cwd))
      (* Join paths, e.g. /foo/bar and /bar/x -> /foo/bar/bar/x *)
      | _, Some cwd -> Some (Syntax.Path (Core.Filename.concat cwd pathname))


let path_linked path state =
  let dir, base = Core.Filename.dirname path, Core.Filename.basename path in
  let inode_p, state = to_inode dir state in
  match find_from_inodetable inode_p base state.i with
  | Some _ -> true
  | None   -> false
