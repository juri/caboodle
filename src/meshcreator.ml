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
  mesh : mesh; 
  order : int;
  width : int;
  height : int; 
  xsize : int;
  ysize : int; 
  mutable idseq : int; }

let create_mesh (order : int) (xsize : int) (ysize : int) : t =
  let rec find_divisor d =
    if order mod d = 0 then d else find_divisor (d + 1);
  in
  let width = find_divisor (int_of_float (sqrt (float_of_int order))) in
  let height = order / width in
  { mesh=Array.make_matrix order order false;
    order=order;
    width=width;
    height=height; 
    xsize=xsize;
    ysize=ysize;
    idseq=0; };;

let edge_exists (m : t) (n1 : int) (n2 : int) =
  n1 >= 0 && n1 < m.order && n2 >= 0 && n2 < m.order && m.mesh.(n1).(n2);;

let add_edge (m : t) (n1 : int) (n2 : int) =
  m.mesh.(n1).(n2) <- true;
  m.mesh.(n2).(n1) <- true;;

let num_of_edges (m : t) n =
  Array.fold_left
    (fun count node ->
      if node then count + 1 else count)
    0 m.mesh.(n);;

(*********** possible_directions ***********)

(* The possible_directions stuff is used for randomly connecting the mesh. 
 * It records two pieces of information: which neighbours of a node 
 * (indexed clockwise from the top, starting from 0, top left being 1, 
 * etc) are available as potential endpoints of an edge, and an index 
 * pointing to one of those directions. *)
type possible_directions = {
  dirs : bool array;
  mutable point : int;
}

let string_of_pd pd =
  let start = "{ [|" in
  start ^ Array.fold_left (fun s b -> s ^ (string_of_bool b) ^ "; ") "" pd.dirs ^ "|]; " ^ (string_of_int pd.point) ^ " }";;

(* Where does pd currently point *)
let get_direction pd = pd.point;;

(* Move the point to the next available neighbour, raises Not_found if no
 * neighbours are available. *)
let move_to_next_direction pd : int= 
  let rec find_next i =
    match i with
      n when n < 9 ->
        if pd.dirs.((pd.point + i) mod 8) then
          pd.point <- (pd.point + i) mod 8
        else
          find_next (i + 1);
    | n ->
        raise Not_found;
  in
  find_next 1;
  pd.point;;

(* Mark a neighbour as unavailable *)
let remove_direction pd n = pd.dirs.(n) <- false;;

(* Are there any available neighbours left *)
let directions_left pd = 
  let found = ref false in
  for i = 0 to 7 do
    if pd.dirs.(i) then found := true;
  done;
  !found;;

(* Pick a random available neighbour and point to it *)
let move_to_random_direction pd =
  let choices = Array.fold_left (fun count dir ->
    if dir then count + 1 else count) 0 pd.dirs in
  for i = 0 to choices do
    ignore (move_to_next_direction pd);
  done;
  pd.point;;

(* Marks each neighbour either available (true) or unavailable (false)
 * based on what f returns. *)
let filter_directions pd f =
  Array.iteri 
    (fun idx elem -> pd.dirs.(idx) <- f idx elem) pd.dirs;;

let create_directions () =
  let dirs = Array.make 8 true in
  { dirs=dirs; point=0; };;

(********* possible_directions end *************)

(* probabilities for different number of edges *)
let edge_probabilities = 
  let rec probs prev lst acc =
    match lst with
      [] -> List.rev acc;
    | head :: tail ->
        let prob = prev + head in
        probs prob tail (prob :: acc);
  in
  probs 0 [15; 50; 35] [];;

let get_random_number_of_edges () =
  let rec get_edges edges rnd probs =
    match probs with
      [] -> edges;
    | head :: tail ->
        if rnd < head then edges else get_edges (edges + 1) rnd tail;
  in
  get_edges 2 (Random.int 100) edge_probabilities;;

(** Connect the mesh randomly but without creating intersections *)
let randomly_connect_mesh (m : t) : t =
  let max_edges = 4 in
  (* Address neighbours by number - 0 on top, 1 on top left, etc *)
  let get_index node where =
    match where with
      0 -> node - m.width
    | 1 -> node - m.width + 1
    | 2 -> node + 1
    | 3 -> node + m.width + 1
    | 4 -> node + m.width
    | 5 -> node + m.width - 1
    | 6 -> node - 1
    | 7 -> node - m.width - 1;
    | _ -> raise (Invalid_argument "foo");
  in
  (* Check if a neighbour exists *)
  let exists node where = 
    let toprow = node < m.width in
    let leftcol = node mod m.width = 0 in
    let rightcol = (node + 1) mod m.width = 0 in
    let bottomrow = node + m.width >= m.order in
    match where with
      0 -> not toprow;
    | 1 -> not (toprow || rightcol);
    | 2 -> not rightcol;
    | 3 -> not (bottomrow || rightcol);
    | 4 -> not bottomrow;
    | 5 -> not (bottomrow || leftcol);
    | 6 -> not leftcol;
    | 7 -> not (leftcol || toprow);
    | _ -> raise (Invalid_argument "foo");
  in
  (* Does adding an edge from n1 to n2 create an intersection with an
   * existing edge? *)
  let creates_intersection n1 n2 =
    match n2 with
      n when n = n1 - m.width + 1 -> edge_exists m (n1 - m.width) (n1 + 1);
    | n when n = n1 + m.width + 1 -> edge_exists m (n1 + m.width) (n1 + 1);
    | n when n = n1 + m.width - 1 -> edge_exists m (n1 + m.width) (n1 - 1);
    | n when n = n1 - m.width - 1 -> edge_exists m (n1 - m.width) (n1 - 1);
    | _ -> false;
  in
  (* Does adding an edge to n make n the endpoint of too many edges? *)
  let creates_too_many_edges n = 
    (num_of_edges m n) >= max_edges in
  (* Is the the neighbour pointed at by where (neighbour number) and target
   * (node index) an acceptable target for an edge starting from node? *)
  let acceptable node where target =
    exists node where && 
    not (edge_exists m node target || creates_intersection node target || 
          creates_too_many_edges target) in
  (* Filter out neighbours from directions that aren't acceptable endpoints
   * for edges starting from node *)
  let filter_unsuitable node directions =
    for i = 0 to 7 do
      let target = get_index node i in
      if not (acceptable node i target) then
        remove_direction directions i;
    done;
  in
  (* Filter out neigbours that already have a connection *)
  let filter_populated node neighbours =
    filter_directions neighbours (fun dir available ->
      match available with
        true -> (num_of_edges m (get_index node dir)) = 0;
      | false -> false;);
  in
  (* Randomly span the mesh depth first so that every node gets at least
   * one connection *)
  let rec span_depth node =
    let suitable = create_directions () in
    let rec try_neighbours () =
      filter_unsuitable node suitable;
      filter_populated node suitable;
      if (directions_left suitable) then
        begin
          let dir = move_to_random_direction suitable in
          let target = get_index node dir in
          add_edge m node target;
          span_depth target;
          try_neighbours ();
        end;
    in
    try_neighbours ();
  in 
  (* Add random connections to node *)
  let rec connect_node node connections directions =
    if directions_left directions then
      begin
        let where = move_to_next_direction directions in
        let target = get_index node where in
        if connections > 0 then
          begin
            if (Random.int 8 > 2) then
              begin
                add_edge m node target;
                filter_unsuitable node directions;
                connect_node node (connections - 1) directions;
              end
            else
              connect_node node connections directions;
          end
        else
          ();
      end
    else
      ();
  in
  (* Randomly connect the tree *)
  let random_walk () =
    span_depth 0;
  in
  (* Add connections to the start and end of the random walk. After the
   * spanning walk, they only have one connection and in the right 
   * circumstances could stay that way. This makes sure that they, too, have 
   * at least two connections. *)
  let add_connections_to_heads () =
    for node = 0 to (m.order - 1) do
      if (num_of_edges m node) = 1 then
        begin
          let dirs = create_directions () in
          filter_unsuitable node dirs;
          connect_node node 1 dirs;
        end;
    done;
  in
  (* Add random connections to every node *)
  let add_random_connections () =
    for node = 0 to (m.order - 1) do
      let directions = create_directions () in
      filter_unsuitable node directions;
      let edges = get_random_number_of_edges () - (num_of_edges m node) in
      connect_node node edges directions;
    done;
  in

  random_walk ();
  add_connections_to_heads ();
  add_random_connections ();
  m;;

let connect_mesh (m : t) : t =
  for i = 0 to m.order - 1 do
    let col = m.mesh.(i) in
    let top = i >= m.width in
    let right = (i + 1) mod m.width <> 0 in
    let bottom = (i + m.width) <= (m.order - 1) in
    let left = i mod m.width <> 0 in
    if top then col.(i-m.width) <- true; (* top *)
    if right then col.(i+1) <- true; (* right *)
    if bottom then col.(i + m.width) <- true; (* bottom *)
    if left then col.(i-1) <- true; (* left *)
  done;
  m;;

let connect_random_nodes (m : t) (n : int) : t =
  m;;

let init (order : int) (xsize : int) (ysize : int) : t =
  Random.self_init ();
  randomly_connect_mesh (create_mesh order xsize ysize);; 

module VertexSet = Set.Make(
  struct type t = vertex let compare = compare end);;

let get_next_id (m : t) : int =
  let id = m.idseq in
    m.idseq <- m.idseq + 1;
    id;;

(* Create vertices in a circle *)
let create_circle_vertices (m : t) : vertex array =
  let angle2radian a = a *. pi /. 180. in
  let sep_angle = 360. /. float_of_int m.order in
  let angles = Array.make m.order 0. in
  for i = 0 to (m.order - 1) do
    angles.(i) <- angle2radian (sep_angle *. (float_of_int i));
  done;
  let rec randomize_array fromarr toarr top =
    match top with
      n when n > 0 ->
        let copyindex = Random.int (top + 1) in
        toarr.(top) <- fromarr.(copyindex);
        Array.blit fromarr (copyindex + 1) fromarr copyindex (top - copyindex);
        randomize_array fromarr toarr (top - 1);
    | n -> 
        toarr.(top) <- fromarr.(top); 
        toarr;
  in
  let rand_angles = randomize_array angles 
    (Array.make m.order 0.) (m.order - 1) in

  let margin = 10 in
  let len = (float_of_int ((min m.xsize m.ysize) - (2 * margin))) /. 2. in
  let center_x = (float_of_int m.xsize) /. 2. in
  let center_y = (float_of_int m.ysize) /. 2. in
  let lst = ref [] in
  for i = 0 to (m.order - 1) do
    let r = rand_angles.(i) in
    let vx = len *. cos(r) in
    let vy = len *. sin(r) in
    lst := { coord_x=int_of_float(center_x +. vx); 
             coord_y=int_of_float(center_y +. vy); 
             id=i } :: !lst;
  done;
  Array.of_list (List.rev !lst);;

(* Create vertices in the mesh shape. Useful for debugging. *)
let create_ordered_vertices (m : t) : vertex array =
  let hsep = (m.xsize - 20) / m.width in
  let vsep = (m.ysize - 20) / m.height in
  let rec create2 n lst col row =
    let lst = { coord_x=(col * hsep) + 10; 
                coord_y=(row * vsep) + 10; id=get_next_id m; } :: lst in
    if n == 1 then lst else
      begin
        let ccall = create2 (n - 1) lst in
        if (col + 1) >= m.width then
          ccall 0 (row + 1)
        else
          ccall (col + 1) row;
      end;
  in
  Array.of_list (List.rev (create2 m.order [] 0 0));;
        
let create_level (m : t) : Level.t =
  let vertices = create_circle_vertices m in
  Level.create_level vertices m.mesh m.xsize m.ysize;;

