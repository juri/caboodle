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

let version = "0.4";;

type vertex = { 
    id : int;
    mutable coord_x : int;
    mutable coord_y : int;
  };;

type line = { 
    point1 : vertex;
    point2 : vertex;
  };;

type slopevalue = SlopeVal of float | Undefined
type slopedline = {
  l : line;
  mutable lslope : slopevalue; }

type mesh = bool array array;;

let string_of_vertex v =
  Printf.sprintf "(%d,%d)" v.coord_x v.coord_y;;

let default_size = (600, 400);;

let pi = 4. *. atan 1.;;
let two_pi = 8. *. atan 1.;;

