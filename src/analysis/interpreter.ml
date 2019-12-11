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


open Errors
open Domains
open Syntax


let dereference_link state pathname dereferer =
  if dereferer
  then
    let inode, state' = Domains.to_inode pathname state in
    match find_from_symtable inode state.s with
    | None -> pathname, state'
    | Some target_pathname -> target_pathname, state'
  else pathname, state


let handle_path_effect pathname effect state sdesc derefer =
  match effect with
  | Syntax.Consume ->
    let pathname, state' = dereference_link state pathname derefer in
    { state' with c = Domains.add_effect state'.c (Touch pathname, sdesc) }
  | Syntax.Produce ->
    let pathname, state' = dereference_link state pathname derefer in
    { state' with c = Domains.add_effect state'.c (Create pathname, sdesc) }
  | Syntax.Expunge ->
    let pathname, state' = dereference_link state pathname derefer in
    let c' = add_effect state'.c (Remove pathname, sdesc) in
    let inode, state'' = Domains.to_inode (Core.Filename.dirname pathname) state' in
    let state = Domains.unlink_resource inode
       (Core.Filename.basename pathname) state'' in
    { state with c = c' }


(**
 * A generic function which is used to interpret a family of system calls
 * that operate on path names (either absolute or relative).
 *)
let interpret_hpath derefer pid state sdesc d p eff =
  match Domains.get_pathname pid state d p with
  | None | Some (Syntax.Unknown _) -> state
  | Some (Syntax.Path pathname)    ->
    handle_path_effect pathname eff state sdesc derefer


let interpret_chdir pid state p =
  match Domains.get_pathname pid state Syntax.AT_FDCWD p with
  | None                                 -> state
  | Some (Path cwd) | Some (Unknown cwd) ->
    let inode, state' = Domains.to_inode cwd state in
    { state' with d = Domains.add_to_cwdtable pid inode state'.k state'.d }


let process_clone_none pid new_pid state =
  let addr = Domains.gen_addr state in
  let k' = Domains.add_to_proctable new_pid addr addr state.k in
  (* Get the working directory of the parent process
     and use the same for the child process. *)
  { state with k = k';
               d = Domains.copy_cwdtable pid addr k' state.d;
               r = Domains.copy_fdtable pid addr k' state.r;}


let process_clone_cwd pid new_pid state =
  let cwd_addr = Domains.gen_addr state in
  match Domains.find_from_proctable pid state.k with
  | None              -> state
  | Some (_, fd_addr) ->
    let k' = Domains.add_to_proctable new_pid cwd_addr fd_addr state.k in
    { state with k = k';
                 d = Domains.copy_cwdtable pid cwd_addr k' state.d; }


let process_clone_fd pid new_pid state =
  let fd_addr = Domains.gen_addr state in
  match Domains.find_from_proctable pid state.k with
  | None              -> state
  | Some (cwd_addr, _) ->
    let k' = Domains.add_to_proctable new_pid cwd_addr fd_addr state.k in
    { state with k = k';
                 r = Domains.copy_fdtable pid fd_addr k' state.r; }


let process_clone_fdcwd pid new_pid state =
  match Domains.find_from_proctable pid state.k with
  | None              -> state
  | Some (cwd_addr, fd_addr) ->
    let k' = Domains.add_to_proctable new_pid cwd_addr fd_addr state.k in
    { state with k = k' }


let interpret_newproc pid state c f =
  try
    match c with
    | Syntax.NONE  -> process_clone_none pid f state
    | Syntax.CWD   -> process_clone_fd pid f state
    | Syntax.FS    -> process_clone_cwd pid f state
    | Syntax.FSCWD -> process_clone_fdcwd pid f state
  with Not_found ->
    let addr = Domains.gen_addr state in
    { state with k = Domains.add_to_proctable f addr addr state.k }


let interpret_delfd pid state f =
  try
    match Domains.find_from_fdtable pid f state.k state.r with
    | None ->
      { state with r = Domains.remove_from_fdtable pid f state.k state.r }
    | Some inode ->
      let state = Domains.close_inode inode state in
      { state with r = Domains.remove_from_fdtable pid f state.k state.r }
  with Not_found -> state


let interpret_dupfd pid state oldfd newfd =
  match oldfd with
  | "0" | "1" | "2" ->
    (* We try to duplicate a fd
       that we ignore, e.g. stdin, stdout, etc.

       Those fds do not correspond to any pathname.*)
    { state with r = Domains.add_to_fdtable pid newfd None state.k state.r }
  | oldfd ->
    if oldfd = newfd
    then state
    else
      (* We duplicate fd. The return value of fcntl corresponds to
         the new file descriptor that points to the same path of
         the fd given as argument. *)
      try
        { state with r = Domains.copy_fd pid oldfd newfd state.k state.r }
      with Not_found -> state


let interpret_fchdir pid state f =
  (* We inspect the pathname corresponding to the given fd,
   * and we update the working directory of the current process
   * accordingly. *)
  try
    match Domains.find_from_fdtable pid f state.k state.r with
    | Some inode ->
      { state with d = Domains.add_to_cwdtable pid inode state.k state.d }
    | None -> state
  with Not_found -> state


let interpret_link pid state sdesc d1 p1 d2 p2 =
  match (
    Domains.get_pathname pid state d1 p1,
    Domains.get_pathname pid state d2 p2)
  with
  | None, _
  | _, None
  | Some (Unknown _), _
  | _, Some (Unknown _) -> state
  | Some (Path old_pathname), Some (Path new_pathname) ->
    let dir, base = (
      Core.Filename.dirname new_pathname,
      Core.Filename.basename new_pathname
    ) in
    let inode, state = Domains.to_inode old_pathname state in
    let c' = Domains.add_effect state.c (Touch dir, sdesc) in
    let inode_p, state = Domains.to_inode dir state in
    { state with
      i  = Domains.add_to_inodetable inode_p base inode new_pathname state.i;
      c  = c' }


let interpret_newfd pid state sdesc d p f =
  match Domains.get_pathname pid state d p with
  | None | Some (Unknown _) -> state
  | Some (Path pathname)    ->
    let dir = Core.Filename.dirname pathname in
    let c' = Domains.add_effect state.c (Touch dir, sdesc) in
    let inode, state' = Domains.to_inode pathname state in
    { state' with
      r = Domains.add_to_fdtable pid f (Some inode) state'.k state'.r;
      c = c';
      p = Domains.open_inode inode state'.p;
    }


let interpret_rename pid state sdesc d1 p1 d2 p2 =
  match (
    Domains.get_pathname pid state d1 p1,
    Domains.get_pathname pid state d2 p2)
  with
  | None, _
  | _, None
  | Some (Unknown _), _
  | _, Some (Unknown _) -> state
  | Some (Path old_pathname), Some (Path new_pathname) ->
    let dir, base = (
      Core.Filename.dirname new_pathname,
      Core.Filename.basename new_pathname
    ) in
    let inode, state = Domains.to_inode old_pathname state in
    let inode_p, state = Domains.to_inode dir state in
    let same = (
      match Domains.find_from_inodetable inode_p base state.i with
      | None   -> false
      | Some i -> i = inode
    ) in
    if same
    then state (* The old pathname and the new pathname are hard links,
            so we do nothing. *)
    else
      (* We also touch the parent directory of the target. *)
      let c' = Domains.add_effect state.c (Touch dir, sdesc) in
      let state = {
        state with
        c  = Domains.add_rename_effect c' (new_pathname, sdesc);
        i  = Domains.add_to_inodetable inode_p base inode new_pathname state.i;
      } in
      state


let interpret_symlink pid state sdesc p1 d p2 =
  (* symlink (old, new) system call create a relation between
   * then old pathname and the new path name.
   *
   * Specifically, upon success, we create a map entry (new, old) where
   * the new path name points to the old one.
   *)
  match (
    Domains.get_pathname pid state Syntax.AT_FDCWD p1,
    Domains.get_pathname pid state d p2)
  with
  | None, _
  | _, None
  | Some (Unknown _), _
  | _, Some (Unknown _) -> state
  | Some (Path pathname), Some (Path new_pathname) ->
    (* We touch the parent directory of the target. *)
    let c' = Domains.add_effect state.c
      (Touch (Core.Filename.dirname new_pathname), sdesc) in
    let inode, state' = Domains.to_inode new_pathname state in
    { state with
      c = c';
      s = Domains.add_to_symtable inode pathname state'.s;
    }


let interpret_begin pid state _ b =
  { state with
    b = Some b;
    z = Some pid; }


let interpret_end _ state _ _ =
  { state with b = None }


let interpret (pid, (construct, sdesc)) state =
  match sdesc with
  (* We do not handle system calls that failed. *)
  | {err = Some _; _ } -> state
  | _ ->
    let state =
      match sdesc.line with
      | 1 ->
          let addr = gen_addr state in
          { state with k = Domains.add_to_proctable pid addr addr state.k;
                       d = Domains.init_proc_cwdtable addr state.d;
                       r = Domains.init_proc_fdtable addr state.r;
          }
      | _ -> state
    in
    try
      match construct with
      | Chdir p                 -> interpret_chdir pid state p
      | Newproc (c, f)          -> interpret_newproc pid state c f
      | Delfd f                 -> interpret_delfd pid state f
      | Dupfd (f1, f2)          -> interpret_dupfd pid state f1 f2
      | Fchdir f                -> interpret_fchdir pid state f
      | Hpath (d, p, m)         -> interpret_hpath true pid state sdesc d p m
      | Hpathsym (d, p, m)      -> interpret_hpath false pid state sdesc d p m
      | Link (d1, p1, d2, p2)   -> interpret_link pid state sdesc d1 p1 d2 p2
      | Newfd (d, p, f)         -> interpret_newfd pid state sdesc d p f
      | Rename (d1, p1, d2, p2) -> interpret_rename pid state sdesc d1 p1 d2 p2
      | Symlink (p1, d, p2)     -> interpret_symlink pid state sdesc p1 d p2
      | Begin b                 -> interpret_begin pid state sdesc b
      | End b                   -> interpret_end pid state sdesc b
      | Nop                     -> state (* Nop  does not affect the state. *)
    with
    | DomainError msg ->
      let msg = String.concat "" [
        msg;
        "on ";
        "model: ";
        Syntax.string_of_trace (construct, sdesc);
      ] in
      let err = (InterpretationError sdesc) in
      raise (Error (err, Some msg))
    | v ->
      let msg = Printexc.to_string v in
      raise (Error (InternalError, Some msg))
