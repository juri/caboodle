(* 
Copyright 2005 Juri Pakaste <juri@iki.fi> 

This file is part of Caboodle.

Caboodle is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

Caboodle is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Caboodle; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Commondefs;;

type t
val create_level : vertex array -> mesh -> int -> int -> t
val intersections : t -> bool
val iter_vertices : (vertex -> unit) -> t -> unit
val iter_lines : (slopedline -> unit) -> t -> unit
val width : t -> int
val height : t -> int
val set_width : t -> int -> t
val set_height : t -> int -> t
val find_vertex : t -> (vertex -> bool) -> 
  vertex list * vertex * vertex list
val are_linked : t -> vertex -> vertex -> bool
val recalculate_slopes : t -> vertex -> t
val randomize_level : t -> unit
val find_all_intersections : t -> (float * float) list
val find_all_intersections_with_vertex : t -> vertex -> (float * float) list
val find_all_intersections_without_vertex : t -> vertex -> (float * float) list

