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

open Test_utilities


let test_reset_effect_store _ =
  let state = initialize_state () in
  let eff = (Read "/foo", default_sysdesc ()) in
  let state = { state with c = add_effect state.c eff } in
  let state' = reset_effect_store state in
  begin
    assert_equal [eff] (get_effects state);
    assert_equal [] (get_effects state');
  end


let test_find_from_cwdtable _ =
  let state = initialize_state () in
  let inode = gen_inode state in
  let cwdt = add_to_cwdtable "pid" inode state.k state.d in 
  begin
    assert_equal false ((find_from_cwdtable "pid" state.k state.d) = Some inode);
    assert_equal (Some inode) (find_from_cwdtable "pid" state.k cwdt);
  end


let test_find_proc_fdtable _ =
  let state = initialize_state () in
  let inode = gen_inode state in
  let fdt = add_to_fdtable "pid" "1" (Some inode) state.k state.r in
  begin
    let initial_pfdt = find_proc_fdtable "pid" state.k state.r in
    match find_proc_fdtable "pid" state.k fdt with
    | None   -> assert_failure "find_proc_table has errors"
    | pfdt   -> assert_not_equal pfdt initial_pfdt
  end



let test_find_from_fdtable _ =
  let state = initialize_state () in
  let inode = gen_inode state in
  let fdt = add_to_fdtable "pid" "1" (Some inode) state.k state.r in
  begin
    assert_raises Not_found (fun _ -> find_from_fdtable "pid" "1" state.k state.r);
    assert_equal (Some inode) (find_from_fdtable "pid" "1" state.k fdt);
  end


let test_find_from_symtable _ =
  let state = initialize_state () in
  let inode = gen_inode state in
  let symt = add_to_symtable inode "/foo" state.s in
  begin
    assert_equal None (find_from_symtable inode state.s);
    assert_equal (Some "/foo") (find_from_symtable inode symt);
  end


let test_find_from_inodetable _ =
  let state = initialize_state () in
  let inode_p, state = to_inode "/" state in
  let inode, state = to_inode "/foo" state in 
  begin
    assert_equal None (find_from_inodetable inode_p "bar" state.i);
    assert_equal (Some inode) (find_from_inodetable inode_p "foo" state.i);
  end


let test_add_to_cwdtable _ =
  let state = initialize_state () in
  let cwdt = add_to_cwdtable "pid" (gen_inode state) state.k state.d in
  assert_not_equal state.d cwdt


let test_add_to_fdtable _ =
  let state = initialize_state () in
  let fdt = add_to_fdtable "pid" "1" (Some (gen_inode state)) state.k state.r in
  assert_not_equal state.r fdt


let test_add_to_symtable _ =
  let state = initialize_state () in
  let symt = add_to_symtable (gen_inode state) "/foo" state.s in
  assert_not_equal state.s symt


let test_add_to_inodetable _ =
  let state = initialize_state () in
  let inode = gen_inode state in
  let inode2 = gen_inode state in
  let inode3, state = to_inode "/" state in
  begin
    assert_raises
      (DomainError "The parent inode is the same with the inode of the child")
      (fun _ -> add_to_inodetable inode "foo" inode "/foo" state.i);
    assert_raises
      (DomainError "The parent inode does not exist")
      (fun _ -> add_to_inodetable inode "foo" inode2 "/foo" state.i);
    assert_raises
      (DomainError "The value of `path` is incompatible with the given `inode_p` and `base`")
      (fun _ -> add_to_inodetable inode3 "foo" inode "/foo/bar" state.i);
    assert_raises
      (DomainError "The value of `path` is incompatible with the given `inode_p` and `base`")
      (fun _ -> add_to_inodetable inode3 "foo" inode "/bar" state.i);
    let inodet = add_to_inodetable inode3 "foo" inode "/foo" state.i in
    assert_not_equal state.i inodet
  end


let test_add_to_proctable _ =
  let state = initialize_state () in
  let addr = gen_addr state in
  let proct = add_to_proctable  "pid" addr addr state.k in
  assert_not_equal state.k proct


let test_remove_from_fdtable _ =
  let state = initialize_state () in
  let inode = gen_inode state in
  let fdt = add_to_fdtable "pid" "1" (Some inode) state.k state.r in
  begin
    assert_equal fdt (remove_from_fdtable "foo" "1" state.k fdt);
    assert_equal fdt (remove_from_fdtable "pid" "2" state.k fdt);
    assert_not_equal fdt (remove_from_fdtable "pid" "1" state.k fdt);
  end


let test_copy_cwdtable _ =
  let state = initialize_state () in
  let addr = gen_addr state in
  let proct = add_to_proctable "pid2" addr addr state.k in
  begin
    assert_equal state.d (copy_cwdtable "pid3" addr proct state.d);
    let cwdt = copy_cwdtable "pid" addr proct state.d in
    assert_not_equal cwdt state.d;
    assert_equal (find_from_cwdtable "pid" proct cwdt) (find_from_cwdtable "pid2" proct cwdt);

    let inode = gen_inode state in
    let cwdt = add_to_cwdtable "pid2" inode proct cwdt in 
    assert_not_equal (find_from_cwdtable "pid" proct cwdt) (find_from_cwdtable "pid2" proct cwdt);
  end


let test_copy_fdtable _ =
  let state = initialize_state () in
  let addr = gen_addr state in
  let proct = add_to_proctable "pid2" addr addr state.k in
  begin
    assert_equal state.r (copy_fdtable "pid3" addr proct state.r);
    let fdt = copy_fdtable "pid" addr proct state.r in
    assert_not_equal fdt state.r;
    assert_equal (find_proc_fdtable "pid" proct fdt) (find_proc_fdtable "pid2" proct fdt);

    let inode = gen_inode state in
    let fdt = add_to_fdtable "pid2" "2" (Some inode) proct fdt in
    assert_not_equal (find_proc_fdtable "pid" proct fdt) (find_proc_fdtable "pid2" proct fdt);
  end


let test_copy_fd _ =
  let state = initialize_state () in
  let inode = gen_inode state in
  let fdt = add_to_fdtable "pid" "1" (Some inode) state.k state.r in
  let fdt2 = copy_fd "pid" "1" "2" state.k fdt in
  begin
    assert_equal fdt (copy_fd  "pid2" "1" "2" state.k fdt);
    assert_equal (Some inode) (find_from_fdtable "pid" "2" state.k fdt2);
  end


let test_add_effect _ =
  begin
    let state = initialize_state () in
    let state = { state with c = add_effect state.c (Read "/bar", default_sysdesc ()) } in
    assert_read_path "/bar" state;
    let state = { state with c = add_effect state.c (Create "/bar", default_sysdesc ()) } in
    assert_create_path "/bar" state;
    let state = { state with c = add_effect state.c (Touch "/bar", default_sysdesc ()) } in
    assert_touch_path "/bar" state;
    let state = { state with c = add_effect state.c (Write "/bar", default_sysdesc ()) } in
    assert_write_path "/bar" state;
    let state = { state with c = add_effect state.c (Remove "/bar", default_sysdesc ()) } in
    assert_remove_path "/bar" state;
  end


let test_add_rename_effect _ =
  begin
    let state, sdesc = init_state (), default_sysdesc () in
    let state' = { state with c = add_rename_effect state.c ("/bar", sdesc) } in
    assert_create_path "/bar" state';
    let state = { state with c = add_effect state.c (Read "/bar", sdesc) } in
    let state' = { state with c = add_rename_effect state.c ("/bar", sdesc) } in
    assert_read_path "/bar" state';
    assert_write_path "/bar" state';
    assert_syscall_effect (fun x -> Create "/bar" <> x) state'; 
  end


let test_inode_converters _ =
  let state = initialize_state () in
  let inode, state = to_inode "/foo/bar/" state in
  let inode_p, state' = to_inode "/foo/" state in
  begin
    assert_equal [] (diff_states state state');
    assert_equal (Some inode) (find_from_inodetable inode_p "bar" state'.i);
    assert_equal "/foo/bar" (to_path_singleton inode state');
    assert_equal "/foo" (to_path_singleton inode_p state');
    let _, state'' = to_inode "/foo/bar/baz" state' in
    assert_equal [Inode_table] (diff_states state' state'');
    let i, s3 = to_inode "/foo/bar" state'' in
    assert_equal [] (diff_states state'' s3);
    assert_equal i inode;
  end


let test_unique_effects _ = 
  begin
    let state, sdesc = initialize_state (), default_sysdesc () in
    let eff = add_effect state.c (Read "/bar", sdesc) in
    let eff = add_effect eff (Touch "/bar", sdesc) in
    let state' = { state with c = eff } in
    let uef = unique_effects (get_effects state') in
    assert_effects (fun x -> Read "/bar" = x) uef;
    assert_effects (fun x -> Touch "/bar" <> x) uef;

    let eff = add_effect eff (Create "/bar", sdesc) in
    let state' = { state' with c = eff } in
    let uef = unique_effects (get_effects state') in
    assert_effects (fun x -> Create "/bar" <> x) uef;

    let eff = add_effect eff (Write "/bar", sdesc) in
    let state' = { state' with c = eff } in
    let uef = unique_effects (get_effects state') in
    assert_effects (fun x -> Write "/bar" = x) uef;

    let eff = add_effect state.c (Create "/bar", sdesc) in
    let eff = add_effect eff (Touch "/bar", sdesc) in
    let eff = add_effect eff (Write "/bar", sdesc) in
    let eff = add_effect eff (Read "/bar", sdesc) in
    let state' = { state' with c = eff } in
    let uef = unique_effects (get_effects state') in
    assert_effects (fun x -> Create "/bar" = x) uef;
    assert_effects (fun x -> Read "/bar" <> x) uef;
    assert_effects (fun x -> Write "/bar" <> x) uef;
    assert_effects (fun x -> Touch "/bar" <> x) uef;

    let eff = add_effect eff (Remove "/bar", sdesc) in
    let state' = { state' with c = eff } in
    let uef = unique_effects (get_effects state') in
    assert_equal [] uef;
  end


let test_get_pathname _ =
  begin
    let state = initialize_state () in
    let inode, state = to_inode "/foo" state in
    let inode2, state = to_inode "/bar/baz/qux" state in
    let state = {
      state with
      d = add_to_cwdtable "pid" inode state.k state.d;
      r = add_to_fdtable "pid" "10" (Some inode2) state.k state.r;
    } in
    assert_equal (Some (Path "/foo/bar")) (get_pathname "pid" state AT_FDCWD (Path "/foo/bar")); 
    assert_equal (Some (Path "/foo/bar")) (get_pathname "pid" state AT_FDCWD (Path "bar"));
    assert_equal (Some (Path "/foo")) (get_pathname "pid" state AT_FDCWD (Path "."));
    assert_equal (Some (Path "/")) (get_pathname "pid" state AT_FDCWD (Path ".."));
    assert_equal (Some (Path "/bar/baz/qux/f")) (get_pathname "pid" state (Fd "10") (Path "f"));
    assert_equal (Some (Path "/bar/baz/qux")) (get_pathname "pid" state (Fd "10") (Path "."));
    assert_equal (Some (Path "/bar/baz")) (get_pathname "pid" state (Fd "10") (Path ".."));
    assert_equal None (get_pathname "pid" state (Fd "0") (Path "foo"));
    assert_equal None (get_pathname "pid" state (Fd "1") (Path "foo"));
    assert_equal None (get_pathname "pid" state (Fd "2") (Path "foo"));
    assert_equal (Some (Unknown "/path")) (get_pathname "pid" state AT_FDCWD (Unknown "/path"));
    assert_equal None (get_pathname "pid2" state (Fd "10") (Path ".."));
  end
