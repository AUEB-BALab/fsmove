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


open OUnit2

open Domains_test
open Interpreter_test
open Parser_test


let interpreter_tests =
  "test_interpreter" >:::
     [
       "test_nop"                >:: test_nop;
       "test_begin"              >:: test_begin;
       "test_end"                >:: test_end;
       "test_chdir"              >:: test_chdir;
       "test_newproc_not_found"  >:: test_newproc_not_found;
       "test_newproc_none"       >:: test_newproc_none;
       "test_newproc_cwd"        >:: test_newproc_cwd;
       "test_newproc_fd"         >:: test_newproc_fd;
       "test_newproc_fdcwd"      >:: test_newproc_fdcwd;
       "test_delfd_fd_not_open"  >:: test_delfd_fd_not_open;
       "test_delfd"              >:: test_delfd;
       "test_dupfd_123"          >:: test_dupfd_123;
       "test_dupfd_not_found"    >:: test_dupfd_not_found;
       "test_dupfd"              >:: test_dupfd;
       "test_fchdir_not_found"   >:: test_fchdir_not_found;
       "test_fchdir"             >:: test_fchdir;
       "test_hpath"              >:: test_hpath;
       "test_hpathsym"           >:: test_hpathsym;
       "test_link"               >:: test_link;
       "test_link_uknown"        >:: test_link_uknown;
       "test_newfd"              >:: test_newfd;
       "test_rename"             >:: test_rename;
       "test_rename_same_inodes" >:: test_rename_same_inodes;
       "test_symlink"            >:: test_symlink;
       "test_unlink_resource"    >:: test_unlink_resource;
     ]


let domains_tests =
  "test_domains" >:::
     [
       "test_reset_effect_store"    >:: test_reset_effect_store;
       "test_find_from_cwdtable"    >:: test_find_from_cwdtable;
       "test_find_proc_fdtable"     >:: test_find_proc_fdtable;
       "test_find_from_fdtable"     >:: test_find_from_fdtable;
       "test_find_from_symtable"    >:: test_find_from_symtable;
       "test_find_from_inodetable"  >:: test_find_from_inodetable;
       "test_add_to_cwdtable"       >:: test_add_to_cwdtable;
       "test_add_to_fdtable"        >:: test_add_to_fdtable;
       "test_add_to_symtable"       >:: test_add_to_symtable;
       "test_add_to_inodetable"     >:: test_add_to_inodetable;
       "test_add_to_proctable"      >:: test_add_to_proctable;
       "test_remove_from_fdtable"   >:: test_remove_from_fdtable;
       "test_copy_cwdtable"         >:: test_copy_cwdtable;
       "test_copy_fdtable"          >:: test_copy_fdtable;
       "test_copy_fd"               >:: test_copy_fd;
       "test_add_effect"            >:: test_add_effect;
       "test_add_rename_effect"     >:: test_add_rename_effect;
       "test_inode_converters"      >:: test_inode_converters;
       "test_unique_effects"        >:: test_unique_effects;
       "test_get_pathname"          >:: test_get_pathname;
     ]

let parser_tests =
  "test_parser" >:::
     [
       "test_parse_chdir"              >:: test_parse_chdir;
       "test_parse_clone"              >:: test_parse_clone;
       "test_parse_close"              >:: test_parse_close;
       "test_dupfd_dup1"               >:: test_parse_dupfd_dup1;
       "test_dupfd_dup23"              >:: test_parse_dupfd_dup23;
       "test_dupfd_fcntl"              >:: test_parse_dupfd_fcntl;
       "test_parse_link"               >:: test_parse_link;
       "test_parse_open"               >:: test_parse_open;
       "test_parse_rename"             >:: test_parse_rename;
       "test_parse_symlink"            >:: test_parse_symlink;
       "test_parse_tool_syscall"       >:: test_parse_tool_syscall;
       "test_parse_unfinished_syscall" >:: test_parse_unfinished_syscall;
       "test_pasre_consumed_hpaths"    >:: test_parse_consumed_hpaths;
       "test_parse_consumed_hpathsyms" >:: test_parse_consumed_hpathsyms;
       "test_parse_produced_hpaths"    >:: test_parse_produced_hpaths;
       "test_parse_expunged_hpaths"    >:: test_parse_expunged_hpaths;
       "test_stop_parser"              >:: test_stop_parser;
       "test_parse_invalid_trace"      >:: test_invalid_trace;
       "test_invalid_file"             >:: test_invalid_trace;
     ]


let () =
  [interpreter_tests; domains_tests; parser_tests;]
  |> test_list
  |> run_test_tt_main
