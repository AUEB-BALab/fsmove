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


open Str


(** Implementation of a map using strings as keys *)
module Strings = Map.Make(String)
module Ints = Map.Make(struct type t = int let compare = Core.compare end)
module StringPair = Map.Make(
    struct
        type t = (string * string)
        let compare = Core.compare
    end
)
module StringSet = Set.Make(String)


let path_regex = "^\\(/[^/ ]*\\)+/?$"


let check_prefix (prefix : string) (str : string) =
    Core.String.is_prefix str ~prefix: prefix


let is_absolute pathname =
    string_match (regexp path_regex) pathname 0


let to_quotes str =
  Printf.sprintf "\"%s\"" str


let rec has_elem lst elem =
    match lst with
    | h :: t ->
        if h = elem then true
        else has_elem t elem
    | [] -> false


let string_contains s1 s2 =
    let re = regexp_string s2 in
    try
        let _ = search_forward re s1 0 in
        true
    with Not_found -> false


let int_stream i =
    Stream.from (fun j -> Some (i + j))


let (~+) x =
  StringSet.singleton x


let (~@) x =
  StringSet.elements x


let (++) x y =
  StringSet.add x y


let (+-) x y =
  StringSet.remove x y
