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


type clone_flag =
  | NONE
  | FS
  | CWD
  | FSCWD
(** The different flags that the `clone` construct supports. *)


type eff =
  | Consume
  | Expunge
  | Produce
(** The effects that FStrace constructs might have on a certain path. *)


type dir_fd =
  | AT_FDCWD
  | Fd of string
(** The file descriptor corresponding to a certain directory. *)


type path =
  | Unknown of string
  | Path of string
(** The type for representing paths.
  The `Unknown` constructor is used to model paths that we do not know
  their exact value, e.g., it corresponds to the value of the pointer in the
  case of `strace`. *)


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
(** The constructs of FStrace used to model all the system calls. *)


type syscall_desc =
  {syscall: string; (** The name of the system call. *)
   args: string; (** The string corresponding to the arguments of the system call. *)
   ret: string; (** The return value of the system call. *)
   err: string option; (** The error type and message of a failed system call. *)
   line: int; (** The line where the system call appears in traces. *)
  }
(** A record that stores all the information of a certain system
  call trace. *)


type trace = (string * (construct * syscall_desc))
(** The type representing a trace entry in FStrace.
  Every entry consists of a string value corresponding to PID,
  the FStrace construct and the system call description. *)


type 'a stream =
  | Stream of 'a * (unit -> 'a stream)
  | Empty
(** A polymorphic type representing a stream. *)


val string_of_syscall : syscall_desc -> string
(** This function converts a system call description into a string
  without including the number of the line where the system call appears. *)

val string_of_syscall_desc : syscall_desc -> string
(** This function converts a system call description into a string. *)


val string_of_trace : (construct * syscall_desc) -> string
(** This function converts an FStrace construct along with
  its system call description into a string. *)


val next_trace : trace stream -> trace stream
(** This function expects a stream of traces and returns
  the next stream (if any). *)


val peek_trace : trace stream -> trace option
(** This function expects a stream of traces and
 and returns the current trace (if any). *)
