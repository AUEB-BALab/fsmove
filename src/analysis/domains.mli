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


exception DomainError of string
(** Exception that is raised when we perform invalid operations
  on the analysis state. *)


type syscall_effect =
  | Create of string
  | Read of string
  | Remove of string
  | Touch of string
  | Write of string
(** Type that holds the effect that a system call might have
  on the file system. *)


type abstraction_effect =
  | Consumed of string
  | Modified of string
  | Produced of string
  | Expunged of string
(** Type that holds the effect that higher-level constructs, such as
  Puppet abstractions, might have on the file system. *)


type effect = (syscall_effect * Syntax.syscall_desc)
(** A type that represents an effect of a system call.*)


type inode
(** Type that represents an inode. *)


type process = string
(** Type that represents a process. *)


type addr_t


type fd = string
(** Type that represents a file descriptor. *)


type filename = string
(** Type that represents a file name. *)


type effect_store
(** A list that contains  the effect of a system call on the file system. *)


type inode_store
(** The type for inode table. *)


type proc_store


type proc_fd_store
(** The type that represents the file descriptor table of a process. *)


type fd_store
(** The type for the file descriptor table. *)


type cwd_store
(** The type for the current working directory table. *)


type symlink_store
(** The type for the symbolic link table. *)


type opened_inodes
(** The type that holds the open inodes. *)


type execution_block = string option
(** The type that represents the ID of the current execution block. *)


type parent_process = string option
(** The type that represents the process of the tool. *)


type state = 
  {k:  proc_store;
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
(** Abstract type that represents the state in FStrace. *)


val gen_inode : state -> inode
(** This function generates a new inode. *)


val gen_addr : state -> addr_t


val init_state : unit -> state
(** Initializes the state of the analysis. *)


val get_effects : state -> effect list
(** Retrieves the list of the effects of the current execution block. *)


val reset_effect_store : state -> state
(** Resets the effect store from the given state. *)


val find_from_cwdtable : process -> proc_store -> cwd_store -> inode option 
(** This function gets the inode of the working directory
  of the given process. *)


val find_proc_fdtable : process
  -> proc_store
  -> fd_store
  -> proc_fd_store option
(** This function gets the file descriptor table of a process. *)


val find_from_fdtable : process -> fd -> proc_store -> fd_store -> inode option  
(** This function finds the inode that corresponds to an open
  file descriptor with regards to the table of the provided process. *)


val find_from_symtable : inode -> symlink_store -> filename option
(** Gets the path to which the given inode points. *)


val find_from_inodetable : inode -> filename -> inode_store -> inode option
(** Gets the inode that corresponds to the given parent directory
 (as specified by the provided inode) and basename. *)


val find_from_proctable : process -> proc_store -> (addr_t * addr_t) option


val add_to_cwdtable : process -> inode -> proc_store -> cwd_store -> cwd_store 
(** This functions add a new entry to the table of working directories.
  Specifically, it associates a process with its current working directory. *)


val add_to_fdtable : process
  -> fd
  -> inode option
  -> proc_store
  -> fd_store
  -> fd_store
(** This function adds a new entry to the file descriptor table.
  It creates an entry with the given file descriptor and inode to
  the file descriptor table of the current process. *)


val add_to_symtable : inode -> filename -> symlink_store -> symlink_store
(** This function adds a new entry to the symbolic link table. *)


val add_to_inodetable : inode -> filename -> inode -> filename -> inode_store -> inode_store
(** This function adds a new entry to the inode table. *)


val remove_from_fdtable : process -> fd -> proc_store -> fd_store -> fd_store
(** This function removes an entry (i.e., pid, fd) from the file
  file descriptor table. *)


val init_proc_cwdtable : addr_t -> cwd_store -> cwd_store


val init_proc_fdtable : addr_t -> fd_store -> fd_store


val add_to_proctable : process
  -> addr_t
  -> addr_t
  -> proc_store
  -> proc_store


val copy_cwdtable : process -> addr_t -> proc_store -> cwd_store -> cwd_store


val copy_fdtable : process -> addr_t -> proc_store -> fd_store -> fd_store
(** It copies the file descriptor table of the first process to the
  second one. *)


val copy_fd : process -> fd -> fd -> proc_store -> fd_store -> fd_store
(** This function copies the file descriptor of a given process. *)


val open_inode : inode -> opened_inodes -> opened_inodes
(** This function opens a inode.
 Specfically, it increments the counter associated with the provided inode.
 This counter marks the number of the open handlers that operate on
 the given inode. *)


val close_inode : inode -> state -> state
(** This function removes closes an inode from the file descriptor table.
 It performs some checks:
   - if the inode is marked as unlinked and is currently open only by
     the current handler, then this function unlinks this inode.
   - if the inode is not marked as unlinked or it also open by other
     handlers, it does not affect the inode table. It only performs
     updates to the file descriptor table. *)


val unlink_resource : inode -> filename -> state -> state
(** This function unlinks a certain resource from the inode table.
  This resource is described by the inode of its parent directory and its
  basename.

  For unlinking, this function removes the corresponding entry
  from the inode table only if the resource is not open by any handler.
  If this is the case, the this function does not remove the entry from the inode table
  but instead it marks it as unlinked in order to be removed when the
  corresponding file descriptor is closed. *)


val add_effect : effect_store -> (syscall_effect * Syntax.syscall_desc) -> effect_store
(** This function adds the effect of a particular system call to the list of effects. *)


val add_rename_effect : effect_store -> (string * Syntax.syscall_desc) -> effect_store


val to_inode : string -> state -> (inode * state)
(** This function converts the given path to an inode.
 Note that this function might have side-effects to the state;
 therefore, it returns the resulting state along with the value of inode. *)


val to_path_singleton : inode -> state -> filename
(** Gets the unique path that points to the given inode. *)


val unique_effects : effect list -> effect list
(** Gets the unique system calls effects from a given list. *)


val get_pathname : process -> state -> Syntax.dir_fd -> Syntax.path -> Syntax.path option  
(** This function gets a path and a file descriptor and constructs
  an absolute path.
  
  If the given path is not absolute, this function interprets it as
  relative to the given file descriptor. *)


val path_linked : filename -> state -> bool
(** Checks whether the given path name is linked with an inode. *)
