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


open OUnit2

open Fsmove
open Domains
open Syntax


let assert_not_equal =
  assert_equal ~cmp: (fun x y -> x <> y)


type state_diff =
  | Proc_table
  | Fd_table
  | Eff_table
  | Sym_table
  | Cwd_table
  | Inode_table
  | Opinode_table
  | Block
  | Process


let diff_states state state' =
  let _compare x y elem acc =
    if x = y
    then acc
    else elem :: acc
  in
  []
  |> _compare state.k state'.k Proc_table
  |> _compare state.r state'.r Fd_table
  |> _compare state.c state'.c Eff_table
  |> _compare state.s state'.s Sym_table
  |> _compare state.d state'.d Cwd_table
  |> _compare state.i state'.i Inode_table
  |> _compare state.p state'.p Opinode_table
  |> _compare state.b state'.b Block
  |> _compare state.z state'.z Process


let get_syscall_effects state =
  state
  |> get_effects
  |> List.map (fun (x, _) -> x)


let assert_syscall_effect assert_fun state =
  state
  |> get_syscall_effects
  |> List.exists assert_fun
  |> assert_equal true


let assert_syscall_effect_all assert_fun state =
  state
  |> get_syscall_effects
  |> List.for_all assert_fun
  |> assert_equal true


let assert_effects assert_fun effects =
  effects
  |> List.map (fun (x, _) -> x)
  |> List.exists assert_fun
  |> assert_equal true


let assert_not_create_path path state =
  assert_syscall_effect_all (fun x -> Create path <> x) state


let assert_not_remove_path path state =
  assert_syscall_effect_all (fun x -> Remove path <> x) state


let assert_create_path path state =
  assert_syscall_effect (fun x -> Create path = x) state


let assert_touch_path path state =
  assert_syscall_effect (fun x -> Touch path = x) state


let assert_read_path path state =
  assert_syscall_effect (fun x -> Read path = x) state


let assert_write_path path state =
  assert_syscall_effect (fun x -> Write path = x) state


let assert_remove_path path state =
  assert_syscall_effect (fun x -> Remove path = x) state


let assert_no_path_effect path state =
  assert_syscall_effect_all (fun x ->
    match x with
    | Read p | Write p | Touch p | Create p | Remove p -> p <> path
  ) state


let construct_sysdesc syscall args ret err line =
  {
    syscall = syscall;
    args    = args;
    ret     = ret;
    err     = err;
    line    = line;
  }


let default_sysdesc () =
  construct_sysdesc "syscall" "args" "ret" None 2


let construct_nop_trace pid =
  (pid, (Nop, default_sysdesc ()))


let construct_begin_trace pid b =
  (pid, (Begin b, default_sysdesc ()))


let construct_end_trace pid b =
  (pid, (End b, default_sysdesc ()))


let construct_chdir_trace pid p =
  (pid, (Chdir p, default_sysdesc ()))


let construct_newproc_trace pid c f =
  let construct = Newproc (c, f) in
  (pid, (construct, default_sysdesc ()))


let construct_delfd_trace pid fd =
  (pid, (Delfd fd, default_sysdesc ()))


let construct_dupfd_trace pid fd1 fd2 =
  let construct = Dupfd (fd1, fd2) in
  (pid, (construct, default_sysdesc ()))


let construct_fchdir_trace pid fd =
  (pid, (Fchdir fd, default_sysdesc ()))


let construct_hpath_trace pid d p e =
  let construct = Hpath (d, p, e) in
  (pid, (construct, default_sysdesc ()))


let construct_hpathsym_trace pid d p e =
  let construct = Hpathsym (d, p, e) in
  (pid, (construct, default_sysdesc ()))


let construct_link_trace pid d1 p1 d2 p2 =
  let construct = Link (d1, p1, d2, p2) in
  (pid, (construct, default_sysdesc ()))


let construct_newfd_trace pid d p f =
  let construct = Newfd (d, p, f) in
  (pid, (construct, default_sysdesc ()))


let construct_rename_trace pid d1 p1 d2 p2 =
  let construct = Rename (d1, p1, d2, p2) in
  (pid, (construct, default_sysdesc ()))


let construct_symlink_trace pid p1 d p2 =
  let construct = Symlink (p1, d, p2) in
  (pid, (construct, default_sysdesc ()))


let initialize_state () =
  let state = init_state () in
  let addr  = gen_addr state in
  let k = add_to_proctable "pid" addr addr state.k in
  { state with k = k;
               d = init_proc_cwdtable addr state.d;
               r = init_proc_fdtable addr state.r;
  }
