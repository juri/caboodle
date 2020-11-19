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

type t = { 
  vertices : vertex list;
  mesh : mesh;
  edges : slopedline array;
  mutable width : int;
  mutable height : int; }

type intersection = No_Intersection | Intersection of float * float;;

module PairSet = Set.Make(
  struct type t = int * int let compare = compare end);;

let string_of_intersection c =
  match c with
    No_Intersection -> "No intersection"
  | Intersection(x, y) -> 
      Printf.sprintf "Intersection at (%s,%s)" (string_of_float x) 
        (string_of_float y);;

(** Are two lines the same *)
let same_line (l1 : line) (l2 : line) : bool  =
  (l1.point1 == l2.point1 && 
   l1.point2 == l2.point2) ||
  (l1.point1 == l2.point2 && 
   l1.point2 == l1.point1);;

let slope (l : line) : slopevalue =
  let divisor = l.point2.coord_x - l.point1.coord_x in
  if divisor = 0 then 
    Undefined
  else
    SlopeVal(float_of_int (l.point2.coord_y - l.point1.coord_y) /.
      float_of_int (divisor));;

let create_line (v1 : vertex) (v2 : vertex) : line =
  { point1 = v1 ; point2 = v2 };;

let create_slopedline (l : line) : slopedline =
  { l=l; lslope=(slope l); };;

let string_of_line ?(with_slope=false) (l : line) : string =
  if with_slope then
    begin
      let output = Printf.sprintf "[%s -> %s (%s)]" (string_of_vertex l.point1) (string_of_vertex l.point2) in
      match slope l with
        SlopeVal(n) -> output (string_of_float n);
      | Undefined -> output "Undefined";
    end
  else
    Printf.sprintf "[%s -> %s]" (string_of_vertex l.point1) (string_of_vertex l.point2);; 

let create_level vertices mesh width height =
  let alllinks = List.flatten (Array.to_list (
    Array.mapi
      (fun i c -> Array.to_list (Array.mapi
        (fun j v -> if v then Some((i, j)) else None)
      c))
      mesh)) in
  let linkset = List.fold_left (fun set elt ->
    match elt with
      None -> set
    | Some(pair) ->
        match pair with
          (i, j) when i > j -> PairSet.add (j, i) set
        | _ -> PairSet.add pair set) PairSet.empty alllinks in
  let vertex_links = PairSet.fold (fun (e1, e2) vlist ->
    (vertices.(e1), vertices.(e2)) :: vlist) linkset [] in
  { vertices = Array.to_list vertices; 
    mesh = mesh;
    width = width; 
    height = height; 
    edges=Array.of_list (List.map (fun (v0, v1) -> 
      create_slopedline (create_line v0 v1)) vertex_links); };;

(** Do lines l1 and l2 cross *)
let cross (l1 : slopedline) (l2 : slopedline) : intersection =
  (* if they have a common endpoint, they do not cross *)
  if l1.l.point1 == l2.l.point1 || 
     l1.l.point1 == l2.l.point2 ||
     l1.l.point2 == l2.l.point1 || 
     l1.l.point2 == l2.l.point2 then
    No_Intersection
  else begin
    (* y = slope1*x - slope1*l1.point1.coord_x + l1.point1.coord_y
     * slope2*x - slope2*l2.point1.coord_x + l2.point1.coord_y = 
     *   slope1*x - slope1* ... etc
     *)
    let l1x0 = float_of_int l1.l.point1.coord_x in
    let l1y0 = float_of_int l1.l.point1.coord_y in
    let l2x0 = float_of_int l2.l.point1.coord_x in
    let l2y0 = float_of_int l2.l.point1.coord_y in
    let x_intersection slope1 slope2 l1 l2 =
      (((-.slope1) *. l1x0) +. l1y0 +.
          (slope2 *. l2x0) -. l2y0 ) /.
      (slope2 -. slope1);
    in
    let y_intersection x slope1 l1x0 l1y0 =
      (slope1 *. x) -. (slope1 *. l1x0) +. l1y0;
    in
    let check_endpoints x y l1 l2 =
      (* is the intersection between the endpoints of the lines *)
      let order_nums (n1 : float) (n2 : float) =
        (* sort n1 and n2 *)
        if n1 <= n2 then n1, n2 else n2, n1
      in
      let ordered (n1 : float) (n2 : float) (n3 : float) = 
        (* are n1, n2 and n3 in ascending order *)
        (n1 -. n2) < 0.0001 && (n2 -. n3) < 0.0001; 
      in
      let l1_min_x, l1_max_x = 
        order_nums l1x0 (float_of_int l1.point2.coord_x) in
      let l1_min_y, l1_max_y = 
        order_nums l1y0 (float_of_int l1.point2.coord_y) in
      let l2_min_x, l2_max_x = 
        order_nums l2x0 (float_of_int l2.point2.coord_x) in
      let l2_min_y, l2_max_y = 
        order_nums l2y0 (float_of_int l2.point2.coord_y) in
      if (ordered l1_min_x x l1_max_x) && (ordered l1_min_y y l1_max_y) &&
         (ordered l2_min_x x l2_max_x) && (ordered l2_min_y y l2_max_y) then
        Intersection (x, y)
      else
        No_Intersection;
    in

    match l1.lslope, l2.lslope with
      Undefined, Undefined -> No_Intersection;
    | SlopeVal(slope1), Undefined ->
        let x = float_of_int l2.l.point1.coord_x in
        let y = y_intersection x slope1 l1x0 l1y0 in
        check_endpoints x y l1.l l2.l;
    | Undefined, SlopeVal(slope2) ->
        let x = float_of_int l1.l.point1.coord_x in
        let y = y_intersection x slope2 l2x0 l2y0 in
        check_endpoints x y l1.l l2.l;
    | SlopeVal(slope1), SlopeVal(slope2) ->
      if abs_float (slope1 -. slope2) < 0.001 then
        (* same slope = the lines are parallel = they don't cross
         * TODO: unless they are on top of each other
         *)
        No_Intersection
      else begin
        let x = x_intersection slope1 slope2 l1 l2 in
        let y = y_intersection x slope1 l1x0 l1y0 in
        check_endpoints x y l1.l l2.l;
      end;
  end;;

let iteri_subarray_cond (f : ('a -> int -> bool)) (ar : 'a array) 
    (start : int) (finish : int) : bool =
  let finished = ref false in
  let found = ref false in
  let i = ref start in
  while not (!found or !finished) do
    found := f (Array.unsafe_get ar !i) !i;
    incr i;
    if !i >= finish then finished := true;
  done;
  !found;;

let intersections x =
  (* double loop (one loop for all the edges, inner loop for half of them) 
   * of all the edges to find intersections, abort as soon as possible *)
  let cr = iteri_subarray_cond 
    (fun edge1 idx1 ->
      let found = iteri_subarray_cond
        (fun edge2 idx2 ->
          match (cross edge1 edge2) with
            Intersection(x, y) -> Printf.printf "%s and %s cross\n%!" (string_of_line edge1.l) (string_of_line edge2.l); true;
          | No_Intersection -> false;)
        x.edges 0 idx1 in
      found;)
    x.edges 0 (Array.length x.edges) in
  cr;;

let find_all_intersections l =
  let intersections = ref [] in
  let _ = iteri_subarray_cond
    (fun edge1 idx1 ->
      let _ = iteri_subarray_cond
        (fun edge2 idx2 ->
          begin
            match (cross edge1 edge2) with
              Intersection(x, y) ->
                intersections := (x, y) :: !intersections;
            | _ -> ();
          end;
          false;) l.edges 0 idx1 in
    false;)
    l.edges 0 (Array.length l.edges) in
  !intersections;;

let find_all_intersections_with_vertex l v =
  let intersections = ref [] in
  let _ = iteri_subarray_cond
    (fun edge1 idx1 ->
        begin
          let _ = iteri_subarray_cond
            (fun edge2 idx2 ->
              if edge1.l.point1 == v || edge1.l.point2 == v || 
                  edge2.l.point1 == v || edge2.l.point2 == v then
                begin
                  match (cross edge1 edge2) with
                    Intersection(x, y) ->
                      intersections := (x, y) :: !intersections;
                  | _ -> ();
                end;
              false;) l.edges 0 idx1 in ();
        end;
      false;)
    l.edges 0 (Array.length l.edges) in
  !intersections;;

let find_all_intersections_without_vertex l v =
  let intersections = ref [] in
  let _ = iteri_subarray_cond
    (fun edge1 idx1 ->
      if not (edge1.l.point1 == v || edge1.l.point2 == v) then
        begin
          let _ = iteri_subarray_cond
            (fun edge2 idx2 ->
              if not (edge2.l.point1 == v || edge2.l.point2 == v) then
                begin
                  match (cross edge1 edge2) with
                    Intersection(x, y) ->
                      intersections := (x, y) :: !intersections;
                  | _ -> ();
                end;
              false;) l.edges 0 idx1 in ();
        end;
      false;)
    l.edges 0 (Array.length l.edges) in
  !intersections;;

let iter_vertices f l =
  List.iter f l.vertices;;

let iter_lines f l =
  Array.iter f l.edges;;

let width l =
  l.width;;

let height l =
  l.height;;

let set_width l w =
  l.width <- w;
  l;;

let set_height l h =
  l.height <- h;
  l;;

let find_vertex l p =
  let rec ifind vs hdaccu =
    match vs with
      [] -> raise Not_found;
    | head :: tail -> 
        if p head then (List.rev hdaccu, head, tail) 
        else ifind tail (head :: hdaccu);
  in
  ifind l.vertices [];;

let are_linked l v1 v2 =
  l.mesh.(v1.id).(v2.id);; 

let recalculate_slopes l v =
  Array.iter (fun l ->
    if l.l.point1 == v || l.l.point2 == v then
      l.lslope <- slope l.l;) l.edges;
  l;;

let randomize_level l =
  iter_vertices (fun v ->
    v.coord_x <- Random.int l.width;
    v.coord_y <- Random.int l.height;) l;
  Array.iter (fun l -> l.lslope <- slope l.l) l.edges;

