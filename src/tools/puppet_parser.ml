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


let resource_str = "\\[[a-z0-9;]+Info: \\(/Stage\\[.*\\]/[A-Z][A-Za-z_-:]+/\\)?\\(\\(.*/\\)?\\([A-Z][A-Za-z_:]+\\[[^\\[{]*\\]\\):\\)"
let begin_str = "\\(" ^ resource_str ^ " Starting to evaluate the resource\\)?.*"
let end_str = "\\(" ^ resource_str ^ " Evaluated in [0-9\\.]+ seconds\\)?.*"
let b_str = "\\(\\[{iov_base=\\)?"
let begin_regex =  Str.regexp ("write[v]?(1,[ ]+" ^ b_str ^ "\"?\\(\\\\33\\)?" ^ begin_str)
let end_regex =  Str.regexp ("write[v]?(1,[ ]+" ^ b_str ^ "\"?\\(\\\\33\\)?" ^ end_str)


let puppet_resource_group = 7

let stop_pattern = "Applied catalog in"


let is_tool_debug_msg syscall_line =
  Util.check_prefix "writev(1," syscall_line ||
    Util.check_prefix "write(1," syscall_line


let model_syscall syscall_line =
  let _extract_resource pattern =
    if Str.string_match pattern syscall_line 0
    then
      try Some (Str.matched_group puppet_resource_group syscall_line)
      with Not_found -> None
    else None
  in
  match _extract_resource begin_regex with
  | None  -> (
      match _extract_resource end_regex with
      | None -> Syntax.Nop
      | Some resource -> Syntax.End resource
  )
  | Some resource -> Syntax.Begin resource


let stop_parser line =
  Util.string_contains line stop_pattern
