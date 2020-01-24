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


exception Empty_stream


type clone_flag =
  | NONE
  | FS
  | CWD
  | FSCWD


type eff =
  | Consume
  | Expunge
  | Produce


type dir_fd =
  | AT_FDCWD
  | Fd of string


type path =
  | Unknown of string
  | Path of string


type construct =
  | Chdir of path
  | Delfd of string
  | Dupfd of (string * string)
  | Fchdir of string
  | Hpath of (dir_fd * path * eff)
  | Hpathsym of (dir_fd * path * eff)
  | Link of (dir_fd * path * dir_fd * path)
  | Newfd of (dir_fd * path * string)
  | Newproc of (clone_flag * string)
  | Rename of (dir_fd * path * dir_fd * path)
  | Symlink of (path * dir_fd * path)
  | Begin of string
  | End of string
  | Nop


type 'a stream =
  | Stream of 'a * (unit -> 'a stream)
  | Empty


type syscall_desc =
  {syscall: string;
   args: string;
   ret: string;
   err: string option;
   line: int;
  }


type trace = (string * (construct * syscall_desc))


let string_of_syscall sdesc =
  match sdesc with
  | {syscall = v1; args = v2; ret = v3; err = None; _ } ->
    v1 ^ "(" ^ v2 ^ ") = " ^ v3
  | {syscall = v1; args = v2; ret = v3; err = Some err; _ } ->
    v1 ^ "(" ^ v2 ^ ") = " ^ v3 ^ " " ^ err


let string_of_syscall_desc sdesc =
  match sdesc with
  | {syscall = v1; args = v2; ret = v3; err = None; line = v4 } ->
    "#" ^ (string_of_int v4) ^ " " ^ v1 ^ "(" ^ v2 ^ ") = " ^ v3
  | {syscall = v1; args = v2; ret = v3; err = Some err; line = v5} ->
    "#" ^ (string_of_int v5) ^ " " ^ v1 ^ "(" ^ v2 ^ ") = " ^ v3 ^ " " ^ err


let string_of_line line =
  "(" ^ (string_of_int line) ^ ")"


let string_of_path path =
  match path with
  | Path x | Unknown x -> x


let string_of_cflags flags =
  match flags with
  | NONE  -> "none"
  | FS    -> "fs"
  | CWD   -> "cwd"
  | FSCWD -> "fscwd"


let string_of_eff eff =
  match eff with
  | Consume -> "consumed"
  | Expunge -> "expunged"
  | Produce -> "produced"


let string_of_dirfd d =
  match d with
  | AT_FDCWD -> "at_fdcwd"
  | Fd f     -> f


(**
let string_of_oflags flags =
  let string_of_oflag flag =
    match flag with
    | RDONLY -> "rdonly"
    | WRONLY -> "wronly"
    | RDRW   -> "rdwr"
    | TRUNC  -> "trunc"
    | CREAT  -> "creat"
  in
  let str = flags
    |> Open_set.elements
    |> List.rev_map string_of_oflag
    |> String.concat ","
  in
  "{" ^ str ^ "}"
*)

let string_of_trace (trace, sdesc) =
  let line_str = string_of_line sdesc.line in
  let str_list = (
    match trace with
    | Begin b         -> ["begin"; b; line_str;]
    | End b           -> ["end"; b; line_str;]
    | Nop             -> ["nop"]
    | Chdir p         -> ["chdir"; string_of_path p; line_str;]
    | Newproc (c, f)  -> ["newproc"; string_of_cflags c; f; line_str;]
    | Delfd f         -> ["delfd"; f; line_str;]
    | Dupfd (f1, f2)  -> ["dupfd"; f1; f2; line_str;]
    | Fchdir f        -> ["fchdir"; f; line_str;]
    | Hpath (d, p, m) ->
      ["hpath"; string_of_dirfd d; string_of_path p; string_of_eff m; line_str;]
    | Hpathsym (d, p, m) ->
      ["hpathsym"; string_of_dirfd d; string_of_path p; string_of_eff m; line_str;]
    | Link (d1, p1, d2, p2) ->
      ["link"; string_of_dirfd d1; string_of_path p1; string_of_dirfd d2; string_of_path p2; line_str;]
    | Newfd (d, p, f) ->
      ["newfd"; string_of_dirfd d; string_of_path p; f; line_str;]
    | Rename (d1, p1, d2, p2) ->
      ["rename"; string_of_dirfd d1; string_of_path p1; string_of_dirfd d2; string_of_path p2; line_str;]
    | Symlink (p1, d, p2) ->
      ["symlink"; string_of_path p1; string_of_dirfd d; string_of_path p2; line_str;]
  ) in
  String.concat " " str_list


let next_trace traces =
  match traces with
  | Stream (_, thunk) -> thunk()
  | Empty -> raise Empty_stream


let peek_trace traces =
  match traces with
  | Stream (v, _) -> Some v
  | Empty -> None
