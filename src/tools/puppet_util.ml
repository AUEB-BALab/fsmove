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


let file_resource_regex = Str.regexp ("File\\[\\(.*\\)\\]")
let title_group = 1


let is_file_resource resource =
  Str.string_match file_resource_regex resource 0


let extract_file_title resource =
  if is_file_resource resource
  then Some (Str.matched_group title_group resource)
  else None
