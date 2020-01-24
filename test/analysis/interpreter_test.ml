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
open Interpreter
open Syntax
open Domains

open Test_utilities


let test_nop _ =
  let trace = construct_nop_trace "pid" in
  let state = initialize_state () in
  let state' = interpret trace state in
  assert_equal [] (diff_states state state')


let test_begin _ =
  let trace  = construct_begin_trace "pid" "foo" in
  let state  = initialize_state () in
  let state' = interpret trace state in
  begin
    assert_equal state.b None;
    assert_equal state.z None;
    assert_equal state'.b (Some "foo");
    assert_equal state'.z (Some "pid");
    assert_equal [Process; Block;] (diff_states state state');
  end


let test_end _ =
  let trace = construct_end_trace "pid" "foo" in
  let state = { (initialize_state ()) with b = Some "foo"; } in
  let state' = interpret trace state in
  begin
    assert_equal state.b (Some "foo");
    assert_equal state'.b None;
    assert_equal state'.z state.z;
    assert_equal [Block;] (diff_states state state');
  end


let test_chdir _ =
  let trace = construct_chdir_trace "pid" (Path "/foo") in
  let state = initialize_state () in
  let state' = interpret trace state in
  begin
    assert_equal state'.b state.b;
    assert_equal state'.z state.z;
    assert_equal None (find_from_cwdtable "pidf" state.k state.d); 
    assert_equal [Inode_table; Cwd_table;] (diff_states state state');
    match find_from_cwdtable "pid" state'.k state'.d with
    | Some inode -> assert_equal "/foo" (to_path_singleton inode state')
    | None       -> assert_failure "Inode not found"
  end


let test_newproc_not_found _ =
  let trace = construct_newproc_trace "pid3" FS "pid2" in
  let state = initialize_state () in
  let state' = interpret trace state in
  assert_equal [] (diff_states state state')


let test_newproc_none _ =
  let trace = construct_newproc_trace "pid" NONE "pid2" in
  let state = initialize_state () in
  let inode = gen_inode state in
  let state = {
    state with
    r = add_to_fdtable "pid" "3" (Some inode) state.k state.r;
    d = add_to_cwdtable "pid" inode state.k state.d;
  } in 
  let state' = interpret trace state in
  begin
    assert_equal [Cwd_table; Fd_table; Proc_table;] (diff_states state state');
    match (
      find_from_cwdtable "pid" state'.k state'.d,
      find_proc_fdtable "pid" state'.k state'.r,
      find_from_cwdtable "pid2" state'.k state'.d,
      find_proc_fdtable  "pid2" state'.k state'.r
    ) with
    | Some i1, Some ft1, Some i2, Some ft2 ->
      assert_equal i1 i2;
      assert_equal ft1 ft2;
    | _ -> assert_failure "Process clone failed"
  end


let test_newproc_cwd _ =
  let trace = construct_newproc_trace "pid" CWD "pid2" in
  let state = initialize_state () in
  let inode = gen_inode state in
  let state = {
    state with
    r = add_to_fdtable "pid" "3" (Some inode) state.k state.r;
    d = add_to_cwdtable "pid" inode state.k state.d;
  } in 
  let state' = interpret trace state in
  begin
    assert_equal [Fd_table; Proc_table;] (diff_states state state');
    match (
      find_from_cwdtable "pid" state'.k state'.d,
      find_proc_fdtable "pid" state'.k state'.r,
      find_from_cwdtable "pid2" state'.k state'.d,
      find_proc_fdtable  "pid2" state'.k state'.r,
      find_from_proctable "pid" state'.k,
      find_from_proctable "pid2" state'.k
    ) with
    | Some i1, Some ft1, Some i2, Some ft2, Some (a1, a2), Some (a1', a2') ->
      assert_equal i1 i2;
      assert_equal ft1 ft2;
      assert_equal a1 a1';
      assert_not_equal a2 a2';
    | _ -> assert_failure "Process clone failed"
  end


let test_newproc_fd _ =
  let trace = construct_newproc_trace "pid" FS "pid2" in
  let state = initialize_state () in
  let inode = gen_inode state in
  let state = {
    state with
    r = add_to_fdtable "pid" "3" (Some inode) state.k state.r;
    d = add_to_cwdtable "pid" inode state.k state.d;
  } in 
  let state' = interpret trace state in
  begin
    assert_equal [Cwd_table; Proc_table;] (diff_states state state');
    match (
      find_from_cwdtable "pid" state'.k state'.d,
      find_proc_fdtable "pid" state'.k state'.r,
      find_from_cwdtable "pid2" state'.k state'.d,
      find_proc_fdtable  "pid2" state'.k state'.r,
      find_from_proctable "pid" state'.k,
      find_from_proctable "pid2" state'.k
    ) with
    | Some i1, Some ft1, Some i2, Some ft2, Some (a1, a2), Some (a1', a2') ->
      assert_equal i1 i2;
      assert_equal ft1 ft2;
      assert_not_equal a1 a1';
      assert_equal a2 a2';
    | _ -> assert_failure "Process clone failed"
  end


let test_newproc_fdcwd _ =
  let trace = construct_newproc_trace "pid" FSCWD "pid2" in
  let state = initialize_state () in
  let inode = gen_inode state in
  let state = {
    state with
    r = add_to_fdtable "pid" "3" (Some inode) state.k state.r;
    d = add_to_cwdtable "pid" inode state.k state.d;
  } in 
  let state' = interpret trace state in
  begin
    assert_equal [Proc_table;] (diff_states state state');
    match (
      find_from_cwdtable "pid" state'.k state'.d,
      find_proc_fdtable "pid" state'.k state'.r,
      find_from_cwdtable "pid2" state'.k state'.d,
      find_proc_fdtable  "pid2" state'.k state'.r,
      find_from_proctable "pid" state'.k,
      find_from_proctable "pid2" state'.k
    ) with
    | Some i1, Some ft1, Some i2, Some ft2, Some (a1, a2), Some (a1', a2') ->
      assert_equal i1 i2;
      assert_equal ft1 ft2;
      assert_equal a1 a1';
      assert_equal a2 a2';
    | _ -> assert_failure "Process clone failed"
  end


let test_delfd_fd_not_open _ =
  let trace = construct_delfd_trace "pid" "1" in
  let state = initialize_state () in
  let state' = interpret trace state in
  assert_equal [] (diff_states state state')


let test_delfd _ =
  let trace = construct_delfd_trace "pid" "1" in
  let state = initialize_state () in
  let state = { state with r = add_to_fdtable "pid" "1"
    (Some (gen_inode state)) state.k state.r }
  in
  let state' = interpret trace state in
  begin
    assert_equal [Fd_table] (diff_states state state');
    assert_raises Not_found (fun _ -> find_from_fdtable "pid" "1" state'.k state'.r)
  end


let test_dupfd_123 _ =
  let _assert_fd s1 s2 fd =
    begin
      assert_raises Not_found (fun _ -> find_from_fdtable "pid" fd s1.k s1.r);
      assert_equal [Fd_table] (diff_states s1 s2);
      assert_equal None (find_from_fdtable "pid" fd s2.k s2.r);
    end
  in
  let t1, t2, t3 = (
    construct_dupfd_trace "pid" "0" "3",
    construct_dupfd_trace "pid" "1" "3",
    construct_dupfd_trace "pid" "2" "3"
  ) in
  let state = initialize_state () in
  let s1, s2, s3 = (
    interpret t1 state,
    interpret t2 state,
    interpret t3 state
  ) in
  begin
    _assert_fd state s1 "3";
    _assert_fd state s2 "3";
    _assert_fd state s3 "3";
  end


let test_dupfd_not_found _ =
  let trace = construct_dupfd_trace "pid" "3" "4" in
  let state = initialize_state () in
  let state' = interpret trace state in
  assert_equal [] (diff_states state state')


let test_dupfd _ =
  let trace = construct_dupfd_trace "pid" "3" "4" in
  let state = initialize_state () in
  let inode = gen_inode state in
  let state = { state with
                r = add_to_fdtable "pid" "3" (Some inode) state.k state.r }
  in
  let state' = interpret trace state in
  begin
    assert_equal [Fd_table] (diff_states state state');
    match find_from_fdtable "pid" "4" state'.k state'.r with
    | Some i -> assert_equal inode i;
    | None   -> assert_failure "Inode is none";
  end


let test_fchdir_not_found _ =
  let trace = construct_fchdir_trace "pid" "3" in
  let state = init_state () in
  let state' = interpret trace state in
  assert_equal [] (diff_states state state')


let test_fchdir _ =
  let trace = construct_fchdir_trace "pid" "3" in
  let state = initialize_state () in
  let inode = gen_inode state in
  let state = { state with
                r = add_to_fdtable "pid" "3" (Some inode) state.k state.r }
  in
  let state' = interpret trace state in
  begin
    assert_equal [Cwd_table] (diff_states state state');
    match (
      find_from_cwdtable "pid" state.k state.d,
      find_from_cwdtable "pid" state'.k state'.d
    ) with
    | Some old_i, Some i ->
        assert_equal inode i;
        assert_equal false (old_i = i)
    | _                  -> assert_failure "fchdir has errors"
  end


let test_hpath _ =
  let t1, t2, t3 = (
    construct_hpath_trace "pid" AT_FDCWD (Path "/foo") Consume,
    construct_hpath_trace "pid" AT_FDCWD (Path "/foo") Produce,
    construct_hpath_trace "pid" AT_FDCWD (Path "/foo") Expunge
  ) in
  let state = initialize_state () in
  let inode, state = to_inode "/foo" state in
  let _, state = to_inode "/bar" state in
  let state = { state with s = add_to_symtable inode "/bar" state.s } in
  let s1, s2, s3 = (
    interpret t1 state,
    interpret t2 state,
    interpret t3 state
  ) in
  begin
    assert_equal [Eff_table] (diff_states state s1);
    assert_touch_path "/bar" s1;
    assert_equal true (path_linked "/foo" s1);
    assert_equal [Eff_table] (diff_states state s2);
    assert_create_path "/bar" s2;
    assert_equal true (path_linked "/foo" s2);
    assert_equal [Inode_table; Eff_table] (diff_states state s3);
    assert_equal false (path_linked "/bar" s3);
    assert_no_path_effect "/foo" s1;
    assert_no_path_effect "/foo" s2;
    assert_no_path_effect "/foo" s3;
  end


let test_hpathsym _ =
  let t1, t2, t3 = (
    construct_hpathsym_trace "pid" AT_FDCWD (Path "/foo") Consume,
    construct_hpathsym_trace "pid" AT_FDCWD (Path "/foo") Produce,
    construct_hpathsym_trace "pid" AT_FDCWD (Path "/foo") Expunge
  ) in
  let state = initialize_state () in
  let inode, state = to_inode "/foo" state in
  let _, state = to_inode "/bar" state in
  let state = { state with s = add_to_symtable inode "/bar" state.s } in
  let s1, s2, s3 = (
    interpret t1 state,
    interpret t2 state,
    interpret t3 state
  ) in
  begin
    assert_equal [Eff_table] (diff_states state s1);
    assert_touch_path "/foo" s1;
    assert_equal true (path_linked "/foo" s1);
    assert_equal [Eff_table] (diff_states state s2);
    assert_create_path "/foo" s2;
    assert_equal true (path_linked "/foo" s2);
    assert_equal [Inode_table; Eff_table] (diff_states state s3);
    assert_equal false (path_linked "/foo" s3);
    assert_no_path_effect "/bar" s1;
    assert_no_path_effect "/bar" s2;
    assert_no_path_effect "/bar" s3;
  end


let test_link _ =
  let trace = construct_link_trace "pid" AT_FDCWD (Path "/foo") AT_FDCWD (Path "/bar") in
  let state = initialize_state () in
  let state' = interpret trace state in
  begin
    assert_equal [Inode_table; Eff_table] (diff_states state state');
    assert_not_create_path "/bar" state';
    assert_touch_path "/" state';
    match to_inode "/foo" state', to_inode "/bar" state' with
    | (i1, _), (i2, _) -> assert_equal i1 i2
  end


let test_link_uknown _ =
  let trace = construct_link_trace "pid" AT_FDCWD (Unknown "/foo") AT_FDCWD (Path "/bar") in
  let state = initialize_state () in
  let state' = interpret trace state in
  assert_equal [] (diff_states state state');
  let trace = construct_link_trace "pid" AT_FDCWD (Path "/foo") AT_FDCWD (Unknown "/bar") in
  let state' = interpret trace state in
  assert_equal [] (diff_states state state')


let test_newfd _ =
  let _assert_newfd fd p_path inode s1 s2 assert_fun =
    begin
      assert_equal [Opinode_table; Eff_table; Fd_table] (diff_states s1 s2);
      assert_fun ();
      assert_touch_path p_path s2;
      match find_from_fdtable "pid" fd s2.k s2.r with
      | Some i -> assert_equal inode i;
      | None   -> assert_failure "newfd has errors"
    end
  in
  let trace = construct_newfd_trace "pid" AT_FDCWD (Path "/foo") "1" in
  let state = initialize_state () in
  let inode, state = to_inode "/foo" state in
  let state' = interpret trace state in
  _assert_newfd "1" "/" inode state state' (fun () -> assert_no_path_effect "/foo" state')


let test_rename _ =
  let _assert_rename p1 p2 inode s1 s2 assert_fun =
    begin
      assert_equal [Inode_table; Eff_table] (diff_states s1 s2);
      assert_fun ();
      assert_touch_path "/" s2;
      assert_not_remove_path p1 s2;
      let i, s = to_inode p2 s2 in
      assert_equal [] (diff_states s2 s);
      assert_equal inode i;
    end
  in
  let trace = construct_rename_trace "pid" AT_FDCWD (Path "/foo") AT_FDCWD (Path "/bar") in
  let state = initialize_state () in
  let inode, state = to_inode "/foo" state in
  let s1, s1' = (
    state,
    { state with c = add_effect state.c (Read "/bar", default_sysdesc ()) }
  )
  in
  let s2, s2' = interpret trace s1, interpret trace s1' in
  begin
    _assert_rename "/foo" "/bar" inode s1 s2 (fun () -> assert_create_path "/bar" s2);
    _assert_rename "/foo" "/bar" inode s1' s2' (fun () -> assert_write_path "/bar" s2');
  end


let test_rename_same_inodes _ =
  let link = construct_link_trace "pid" AT_FDCWD (Path "/foo") AT_FDCWD (Path "/bar") in
  let rename = construct_rename_trace "pid" AT_FDCWD (Path "/foo") AT_FDCWD (Path "/bar") in
  let state = interpret link (initialize_state ()) in
  let state' = interpret rename state in
  assert_equal [] (diff_states state state')


let test_symlink _ =
  let trace = construct_symlink_trace "pid" (Path "/bar") AT_FDCWD (Path "/foo") in
  let state = initialize_state () in
  let inode, state = to_inode "/foo" state in
  let state' = interpret trace state in
  begin
    assert_equal [Sym_table; Eff_table] (diff_states state state');
    assert_not_create_path "/foo" state';
    assert_touch_path "/" state';
    match (
      find_from_symtable inode state.s,
      find_from_symtable inode state'.s
    ) with
    | None, Some p -> assert_equal p "/bar"
    | _            -> assert_failure "Symlink has errors"
  end


let test_unlink_resource _ =
  let newfd1, newfd2, unlink, delfd1, delfd2 = (
    construct_newfd_trace "pid" AT_FDCWD (Path "/foo") "1",
    construct_newfd_trace "pid" AT_FDCWD (Path "/foo") "2",
    construct_hpath_trace "pid" AT_FDCWD (Path "/foo") Expunge,
    construct_delfd_trace "pid" "1",
    construct_delfd_trace "pid" "2"
  ) in
  let state = initialize_state () in
  let _, state = to_inode "/foo" state in
  let state' =
    state
    |> interpret newfd1
    |> interpret newfd2
    |> interpret unlink
  in
  assert_equal true (path_linked "/foo" state');
  let state' = interpret delfd1 state' in
  assert_equal true (path_linked "/foo" state');
  let state' = interpret delfd2 state' in
  assert_equal false (path_linked "/foo" state');
  let state' =
    state
    |> interpret newfd1
    |> interpret newfd2
    |> interpret unlink
    |> interpret delfd1
    |> interpret newfd1
    |> interpret delfd2
    |> interpret delfd1
  in
  assert_equal true (path_linked "/foo" state');
