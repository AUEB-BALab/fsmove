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

open Fsmove
open Errors
open Syntax


module Parser_module = struct

  let is_tool_debug_msg syscall =
    Util.check_prefix "write(1" syscall

  let model_syscall _ =
    Begin "Tool"

  let stop_parser syscall =
    String.equal syscall "10 write(1, \"stop\") = 0"

end


module TestParser = Sys_parser.Make(Parser_module)
open TestParser


let list_of_traces traces =
  let rec _list_of_traces acc traces =
    match peek_trace traces with
    | Some trace -> _list_of_traces (trace :: acc) (Syntax.next_trace traces)
    | None       -> acc
  in
  _list_of_traces [] traces


let to_file ctx str =
  let filename, out_channel = bracket_tmpfile ctx in
  output_string out_channel str;
  close_out out_channel;
  filename


let assert_traces ctx str traces =
  str
  |> to_file ctx
  |> parse_trace_file 
  |> list_of_traces
  |> List.map (fun (pid, (con, _)) -> pid, con)
  |> assert_equal traces


let rec assert_mul_traces ctx syscalls traces =
  match syscalls with
  | []     -> ()
  | h :: t ->
    let _ = assert_traces ctx h traces in
    assert_mul_traces ctx t traces


let test_parse_chdir ctx =
  assert_traces ctx "10 chdir(\"/foo/bar\") = 0" [("10", Chdir (Path "/foo/bar"))];
  assert_traces ctx "10 chdir(0x7f3bbdf504ff) = 0" [("10", Chdir (Unknown "/UNKNOWN"))]


let test_parse_clone ctx =
  assert_traces ctx "10 clone(foo) = 4" [("10", Newproc (NONE, "4"))]


let test_parse_close ctx =
  assert_traces ctx "10 close(5) = 0" [("10", Delfd "5")]


let test_parse_dupfd_dup1 ctx =
  assert_traces ctx "10 dup(1) = 2" [("10", Dupfd ("1", "2"))]


let test_parse_dupfd_dup23 ctx =
  assert_traces ctx "10 dup2(3, 4) = 0" [("10", Dupfd ("3", "4"))];
  assert_traces ctx "10 dup3(3, 4) = 0" [("10", Dupfd ("3", "4"))]


let test_parse_dupfd_fcntl ctx =
  assert_traces ctx "10 fcntl(2, F_DUPFD, 11) = 11" [("10", Dupfd ("2", "11"))];
  assert_traces ctx "10 fcntl(7, F_SETFL, O_RDONLY) = 0" [("10", Nop)]


let test_parse_link ctx =
  [
    ("10", Hpathsym (AT_FDCWD, Path "/bar", Produce));
    ("10", Hpathsym (AT_FDCWD, Path "/foo", Consume));
    ("10", Link (AT_FDCWD, Path "/foo", AT_FDCWD, Path "/bar"));
  ] |> assert_traces ctx "10 link(\"/foo\", \"/bar\") = 10";

  [
    ("10", Hpathsym (Fd "1", Path "/bar", Produce));
    ("10", Hpathsym (AT_FDCWD, Path "/foo", Consume));
    ("10", Link (AT_FDCWD, Path "/foo", Fd "1", Path "/bar"));
  ] |> assert_traces ctx "10 linkat(AT_FDCWD, \"/foo\", 1, \"/bar\") = 10";

  [
    ("10", Hpathsym (AT_FDCWD, Path "/bar", Produce));
    ("10", Hpathsym (Fd "1", Path "/foo", Consume));
    ("10", Link (Fd "1", Path "/foo", AT_FDCWD, Path "/bar"));
  ] |> assert_traces ctx "10 linkat(1, \"/foo\", AT_FDCWD, \"/bar\") = 10";

  [
    ("10", Hpathsym (AT_FDCWD, Unknown "/UNKNOWN", Produce));
    ("10", Hpathsym (Fd "1", Unknown "/UNKNOWN", Consume));
    ("10", Link (Fd "1", Unknown "/UNKNOWN", AT_FDCWD, Unknown "/UNKNOWN"));
  ] |> assert_traces ctx "10 linkat(1, 0x7f3bbdf504ff, AT_FDCWD, 0x7f3bbdf504ff) = 10"


let test_parse_open ctx =
  [
    ("10", Hpath (AT_FDCWD, Path "/foo/bar", Consume));
    ("10", Newfd (AT_FDCWD, Path "/foo/bar", "2"));
  ] |> assert_traces ctx "10 open(\"/foo/bar\", O_RDONLY) = 2";

  [
    ("10", Hpath (AT_FDCWD, Path "/foo/bar", Consume));
    ("10", Newfd (AT_FDCWD, Path "/foo/bar", "2"));
  ] |> assert_traces ctx "10 open(\"/foo/bar\", O_WRONLY) = 2";

  [
    ("10", Hpath (AT_FDCWD, Path "/foo/bar", Consume));
    ("10", Newfd (AT_FDCWD, Path "/foo/bar", "2"));
  ] |> assert_traces ctx "10 open(\"/foo/bar\", O_RDWR) = 2";

  [
    ("10", Hpath (AT_FDCWD, Path "/foo/bar", Produce));
    ("10", Newfd (AT_FDCWD, Path "/foo/bar", "2"));
  ] |> assert_traces ctx "10 open(\"/foo/bar\", O_WRONLY|O_TRUNC) = 2";

  [
    ("10", Hpath (AT_FDCWD, Path "/foo/bar", Produce));
    ("10", Newfd (AT_FDCWD, Path "/foo/bar", "2"));
  ] |> assert_traces ctx "10 open(\"/foo/bar\", O_CREAT|O_WRONLY|O_TRUNC) = 2";


  [
    ("10", Hpath (AT_FDCWD, Unknown "/UNKNOWN", Produce));
    ("10", Newfd (AT_FDCWD, Unknown "/UNKNOWN", "2"));
  ] |> assert_traces ctx "10 open(0x7f3bbdf504ff, O_CREAT|O_WRONLY|O_TRUNC) = 2";

  [
    ("10", Hpath (AT_FDCWD, Path "/foo/bar", Produce));
    ("10", Newfd (AT_FDCWD, Path "/foo/bar", "2"));
  ] |> assert_traces ctx "10 openat(AT_FDCWD, \"/foo/bar\", O_CREAT|O_WRONLY|O_TRUNC) = 2";

  [
    ("10", Hpath (Fd "20", Path "/foo/bar", Produce));
    ("10", Newfd (Fd "20", Path "/foo/bar", "2"));
  ] |> assert_traces ctx "10 openat(20, \"/foo/bar\", O_CREAT|O_WRONLY|O_TRUNC) = 2"


let test_parse_rename ctx =
  [
    ("10", Hpathsym (AT_FDCWD, Path "/foo", Expunge));
    ("10", Rename (AT_FDCWD, Path "/foo", AT_FDCWD, Path "/bar"));
  ] |> assert_traces ctx "10 rename(\"/foo\", \"/bar\") = 0";

  [
    ("10", Hpathsym (AT_FDCWD, Unknown "/UNKNOWN", Expunge));
    ("10", Rename (AT_FDCWD, Unknown "/UNKNOWN", AT_FDCWD, Unknown "/UNKNOWN"));
  ] |> assert_traces ctx "10 rename(0x7f3bbdf504ff, 0x7f3bbdf504ff) = 0";

  [
    ("10", Hpathsym (AT_FDCWD, Path "/foo", Expunge));
    ("10", Rename (AT_FDCWD, Path "/foo", Fd "5", Path "bar"));
  ] |> assert_traces ctx "10 renameat(AT_FDCWD, \"/foo\", 5, \"bar\") = 0";

  [
    ("10", Hpathsym (Fd "5", Path "bar/foo", Expunge));
    ("10", Rename (Fd "5", Path "bar/foo", AT_FDCWD, Path "bar"));
  ] |> assert_traces ctx "10 renameat(5, \"bar/foo\", AT_FDCWD, \"bar\") = 0"


let test_parse_symlink ctx =
  [
    ("10", Hpathsym (AT_FDCWD, Path "/bar", Produce));
    ("10", Symlink (Path "/foo", AT_FDCWD, Path "/bar"));
  ] |> assert_traces ctx "10 symlink(\"/foo\", \"/bar\") = 0";

  [
    ("10", Hpathsym (AT_FDCWD, Unknown "/UNKNOWN", Produce));
    ("10", Symlink (Unknown "/UNKNOWN", AT_FDCWD, Unknown "/UNKNOWN"));
  ] |> assert_traces ctx "10 symlink(0x7f3bbdf504ff, 0x7f3bbdf504ff) = 0";

  [
    ("10", Hpathsym (AT_FDCWD, Path "bar", Produce));
    ("10", Symlink (Path "/foo", AT_FDCWD, Path "bar"));
  ]
  |> assert_traces ctx "10 symlinkat(\"/foo\", AT_FDCWD, \"bar\") = 0";

  [
    ("10", Hpathsym (Fd "5", Path "bar", Produce));
    ("10", Symlink (Path "/bar/foo", Fd "5", Path "bar"));
  ] |> assert_traces ctx "10 symlinkat(\"/bar/foo\", 5, \"bar\") = 0"


let test_parse_tool_syscall ctx =
  assert_traces ctx "10 write(1, \"barfoo\") = 145" [("10", Begin "Tool")]


let test_parse_unfinished_syscall ctx =
  [("10", Chdir (Path "/foo"))]
  |> assert_traces ctx "10 chdir( <unfinished ...>\n10 <... chdir resumed> \"/foo\") = 0";
  let trace_str = "10 fcntl(5, <unfinished ...>\n10 close(2) = 0\n10 <... fcntl resumed> F_DUPFD, 20) = 20" in
  [("10", Dupfd ("5", "20")); ("10", Delfd "2");]
  |> assert_traces ctx trace_str


let test_parse_consumed_hpaths ctx =
  let traces = [
    "10 access(\"/foo/bar\") = 0";
    "10 chmod(\"/foo/bar\") = 0";
    "10 chown(\"/foo/bar\") = 0";
    "10 execve(\"/foo/bar\") = 0";
    "10 fchmodat(AT_FDCWD, \"/foo/bar\") = 0";
    "10 fchownat(AT_FDCWD, \"/foo/bar\") = 0";
    "10 getxattr(\"/foo/bar\") = 0";
    "10 removexattr(\"/foo/bar\") = 0";
  ]
  in
  assert_mul_traces ctx traces [("10", Hpath (AT_FDCWD, Path "/foo/bar", Consume))]


let test_parse_consumed_hpathsyms ctx =
  let traces = [
    "10 lchown(\"/foo/bar\") = 0";
    "10 lgetxattr(\"/foo/bar\") = 0";
    "10 lremovexattr(\"/foo/bar\") = 0";
    "10 lsetxattr(\"/foo/bar\") = 0";
    "10 readlink(\"/foo/bar\") = 0";
    "10 readlinkat(AT_FDCWD, \"/foo/bar\") = 0";
    "10 utime(\"/foo/bar\") = 0";
    "10 utimensat(AT_FDCWD, \"/foo/bar\") = 0";
    "10 utimes(\"/foo/bar\") = 0";
  ]
  in
  assert_mul_traces ctx traces [("10", Hpathsym (AT_FDCWD, Path "/foo/bar", Consume))]


let test_parse_produced_hpaths ctx =
  let traces = [
    "10 mkdir(\"/foo/bar\") = 0";
    "10 mkdirat(AT_FDCWD, \"/foo/bar\") = 0";
    "10 mknod(\"/foo/bar\") = 0";
  ]
  in
  assert_mul_traces ctx traces [("10", Hpathsym (AT_FDCWD, Path "/foo/bar", Produce))]


let test_parse_expunged_hpaths ctx =
  let traces = [
    "10 rmdir(\"/foo/bar\") = 0";
    "10 unlink(\"/foo/bar\") = 0";
    "10 unlinkat(AT_FDCWD, \"/foo/bar\") = 0";
  ]
  in
  assert_mul_traces ctx traces [("10", Hpathsym (AT_FDCWD, Path "/foo/bar", Expunge))]


let test_stop_parser ctx =
  let trace_str = "10 close(1) = 0\n10 write(1, \"stop\") = 0\n10 close(2) = 0" in
  assert_traces ctx trace_str [("10", Delfd "1");]


let test_invalid_trace ctx =
  let assert_raises_parser_error trace =
    match assert_traces ctx trace [] with
    | () -> assert_failure "Exception not raised"
    | exception Error (ParserError (_, _), _) -> ()
  in
  assert_raises_parser_error "dfa";
  assert_raises_parser_error "10 syscall malformed (\"fdaf\") = 0";
  assert_raises_parser_error "10 syscall (\"fdaf\")";
  assert_raises_parser_error "10 chdir( <unfinished ...>\n11 <... chdir resumed> \"/foo\") = 0"


let test_invalid_file _ =
  (fun _ -> parse_trace_file "foo")
  |> assert_raises (Error (GenericError, Some "foo: No such file or directory"))
