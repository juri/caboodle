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

(* Get the level size for level n *)
let get_level_size (n : int) : int = 
  let rec fib4 n =
    match n with
      n when n > 4 -> n + fib4 (n - 1);
    | n when n < 4 -> 0;
    | n -> n;
  in
  6 + fib4 (n + 2);;

type color = { r : float; g : float; b : float; }

type point = Cairo.point = { x : float; y : float }

type solveddialog = {
  dialog : GWindow.dialog_any;
  level_label : GMisc.label;
  time_label : GMisc.label; 
  parent : GWindow.window; }

type level_display = {
	mutable level : Level.t;
	mutable pm : GDraw.pixmap;
	mutable need_update : bool;
  mutable line_width : float;
  mutable line_color : color;
  mutable end_inactive_color : color;
  mutable end_active_color : color;
  mutable end_linked_color : color;
  mutable end_radius : float;
  mutable drag_pt : point;
  mutable dragged : vertex option;
  mutable current_level : int;
  mutable start_time : float;
  mutable paused : bool;
  mutable previous_dragged : vertex option;
  mutable inactive_image : Cairo.image_surface option;
  mutable show_intersections : bool;
  update_time_fun : (level_display -> unit);
  update_level_fun : (level_display -> unit);
  solveddialog : solveddialog; 
}

let string_of_point (p : point) : string =
  "{ x=" ^ string_of_float p.x ^ "; y=" ^ string_of_float p.y ^ " }";;

let point_of_vertex (v : vertex) : point =
  { x=float_of_int v.coord_x; y=float_of_int v.coord_y; };;


let paint_level (surface : Cairo.t) (disp : level_display) : unit =
  let setcolor surface c = Cairo.set_source_rgb surface c.r c.g c.b in
  (** Is vertex v being dragged *)
  let is_being_dragged v =
    match disp.dragged with
      Some(d) when d == v -> true
    | _ -> false;
  in
  (** Is vertex v linked to the vertex being dragged *)
  let is_linked v =
    match disp.dragged with
      Some(d) when Level.are_linked disp.level d v -> true;
    | _ -> false;
  in
  (** Paint line l on Cairo surface *)
  let paint_line surface l =
    let p1, p2 = point_of_vertex l.l.point1, point_of_vertex l.l.point2 in
    Cairo.move_to surface p1.x p1.y;
    Cairo.line_to surface p2.x p2.y;
    Cairo.stroke surface;
  in
  (** Paint vertex v on Cairo surface in with the specified color *)
  let paint_vertex surface v color =
    let p = point_of_vertex v in
    Cairo.save surface;
    Cairo.new_path surface;
    Cairo.arc surface p.x p.y disp.end_radius 0. two_pi;
    setcolor surface color;
    Cairo.fill surface;
    Cairo.restore surface;
    Cairo.save surface;
    setcolor surface disp.line_color;
    Cairo.set_line_width surface (disp.line_width /. 2.);
    Cairo.new_path surface;
    Cairo.arc surface p.x p.y disp.end_radius 0. two_pi;
    Cairo.stroke surface;
    Cairo.restore surface;
  in
  let paint_intersections intersections surface : unit =
    Cairo.save surface;
    Cairo.set_line_width surface 4.;
    Cairo.set_source_rgb surface 1. 0. 0.;
    List.iter (fun (x, y) ->
      Cairo.arc surface x y 10. 0. two_pi;
      Cairo.stroke surface;) intersections;
    Cairo.restore surface;
  in
  (** Paint inactive lines and vertices and return an image_surface with the 
      result *)
  let paint_inactive activev =
    let image = Cairo.image_surface_create Cairo.FORMAT_ARGB32 (Level.width disp.level) (Level.height disp.level) in
    let surface = Cairo.create image in
    Cairo.save surface;
    setcolor surface disp.line_color;
    Cairo.set_line_width surface disp.line_width;
    let paint_lines_iterator l =
      if not (is_being_dragged l.l.point1 || is_being_dragged l.l.point2) then
        paint_line surface l;
    in
    Level.iter_lines paint_lines_iterator disp.level;
    Cairo.restore surface;
    let paint_vertices_iterator v =
      if not (is_being_dragged v || is_linked v) then
        paint_vertex surface v disp.end_inactive_color;
    in
    Level.iter_vertices paint_vertices_iterator disp.level;
    if disp.show_intersections then paint_intersections 
      (match activev with
        Some(v) ->
          (Level.find_all_intersections_without_vertex disp.level v)
      | None -> Level.find_all_intersections disp.level) surface;
    image;
  in
  (** Copy the image_surface img to the drawing surface *)
  let copy_image img =
    Cairo.set_source_surface surface img 0. 0.;
    Cairo.paint surface;
  in
  (** Paint active lines and vertices *)
  let paint_active activev =
    Cairo.save surface;
    setcolor surface disp.line_color;
    Cairo.set_line_width surface disp.line_width;
    let paint_lines_iterator l =
      if (is_being_dragged l.l.point1 || is_being_dragged l.l.point2) then
        paint_line surface l;
    in
    Level.iter_lines paint_lines_iterator disp.level;
    Cairo.restore surface;
    let paint_vertices_iterator v =
      let dragged = is_being_dragged v in
      if (dragged || is_linked v) then
        begin
          paint_vertex surface v 
            (if dragged then disp.end_active_color else disp.end_linked_color);
          if dragged then
            begin
              let p = point_of_vertex v in
              Cairo.save surface;
              setcolor surface disp.end_active_color;
              Cairo.set_line_width surface (disp.line_width /. 2.);
              Cairo.new_path surface;
              Cairo.arc surface p.x p.y (disp.end_radius *. 2.) 0. two_pi;
              Cairo.stroke surface;
              Cairo.restore surface;
            end;
        end;
    in
    Level.iter_vertices paint_vertices_iterator disp.level;
    if disp.show_intersections then paint_intersections
      (Level.find_all_intersections_with_vertex disp.level activev) surface;
  in

  begin
    match disp with
      { dragged = Some(v1); previous_dragged = Some(v2); inactive_image = Some(img); }
          when v1 = v2 ->
        copy_image img;
        paint_active v1;
    | { dragged = Some(v) } ->
        let img = paint_inactive (Some v) in
        disp.previous_dragged <- Some(v);
        disp.inactive_image <- Some(img);
        copy_image img;
        paint_active v;
    | _ ->
        disp.previous_dragged <- None;
        disp.inactive_image <- None;
        copy_image (paint_inactive None);
  end;;

let get_surface (disp : level_display) : Cairo.t =
  let cr = Cairo_lablgtk.create disp.pm#pixmap in
  disp.pm#rectangle ~x:0 ~y:0 ~width:(Level.width disp.level) 
      ~height:(Level.height disp.level) ~filled:true ();
  cr;; 

let paint (disp : level_display) : unit =
  paint_level (get_surface disp) disp;
  ();;

let expose (da : GMisc.drawing_area) (disp : level_display) 
    (x : int) (y : int) (width : int) (height : int) : unit =
  let gwin = da#misc#window in
  let d = new GDraw.drawable gwin in
  d#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height disp.pm#pixmap;;

let expose_cb (da : GMisc.drawing_area) (disp : level_display) 
    (ev : GdkEvent.Expose.t) : bool =
  let area = GdkEvent.Expose.area ev in
  let module GR = Gdk.Rectangle in
  if disp.need_update then (paint disp);
  expose da disp (GR.x area) (GR.y area) (GR.width area) (GR.height area); 
  true;;

let new_pixmap (width : int) (height : int) : GDraw.pixmap =
  let drawable = GDraw.pixmap ~width ~height () in
  drawable#set_foreground `WHITE;
  drawable#rectangle ~x:0 ~y:0 ~width ~height ~filled:true();
  drawable;;

let update_display (d : level_display) : unit =
  d.pm <- new_pixmap (Level.width d.level) (Level.height d.level);
  d.need_update <- true;;

let create_display (height : int) (width : int) 
    (update_time_fun : (level_display -> unit)) 
    (update_level_fun : (level_display -> unit)) 
    (solveddialog : solveddialog) : level_display =
  { level=Meshcreator.create_level 
      (Meshcreator.init (get_level_size 1) height width); 
    pm=new_pixmap height width; 
    need_update=true; 
    line_width=1.5;
    line_color={ r=0.4 ; g=0.4 ; b=0.4;};
    end_inactive_color={ r=0.5 ; g=0.5 ; b=1.; };
    end_active_color={ r=1. ; g=0.4 ; b=0.4 ; };
    end_linked_color={ r=1. ; g=0.7 ; b=0.7 ; };
    end_radius=6.;
    drag_pt={ x=0.; y=0.; };
    dragged=None;
    current_level=1;
    start_time=Unix.time (); 
    paused=false;
    previous_dragged=None;
    inactive_image=None;
    show_intersections=false;
    update_time_fun=update_time_fun;
    update_level_fun=update_level_fun; 
    solveddialog=solveddialog; };;

let config_cb (disp : level_display) (event : GdkEvent.Configure.t) : bool =
  let w = GdkEvent.Configure.width event in
  let h = GdkEvent.Configure.height event in
  let has_grown = w > (Level.width disp.level) || 
    h > (Level.height disp.level) in
  if has_grown then
    begin
      ignore (Level.set_height disp.level h);
      ignore (Level.set_width disp.level w);
      update_display disp;
    end;
  true;;

let refresh (da : GMisc.drawing_area) (disp : level_display) : unit =
  disp.need_update <- true;
  GtkBase.Widget.queue_draw da#as_widget;;

let idle_refresh (da : GMisc.drawing_area) (disp : level_display) : unit =
  ignore (GMain.Idle.add (fun () -> refresh da disp; false;));;

let format_time_elapsed (s : int) : string =
  Printf.sprintf "%02d:%02d" (s / 60) (s mod 60);;
  
let show_solved_dialog (disp : level_display) : unit =
  disp.paused <- true;
  let now = Unix.time () in
  let message = "<b><big>Congratulations</big></b>\nLevel solved!\n" in
  let dlg = GWindow.dialog ~title:"Level solved" ~destroy_with_parent:true 
    ~parent:disp.solveddialog.parent ~position:`CENTER_ON_PARENT () in
  let okbutton = GButton.button ~stock:`OK ~packing:dlg#action_area#add () in
  ignore (okbutton#connect#clicked ~callback:(fun () -> dlg#response `DELETE_EVENT));
  let hbox = GPack.hbox ~packing:(dlg#vbox#pack ~padding:6) () in
  let vbox = GPack.vbox ~packing:(hbox#pack ~padding:12) () in
  let mainlabel = GMisc.label ~markup:message ~packing:vbox#add () in
  let tbl = GPack.table ~columns:2 ~rows:2 ~row_spacings:6 
    ~col_spacings:12 ~packing:vbox#add () in
  let lhlabel = GMisc.label ~text:"Level" ~packing:(tbl#attach ~left:0 ~top:0) () in
  let thlabel = GMisc.label ~text:"Time" ~packing:(tbl#attach ~left:0 ~top:1) () in
  let llabel = GMisc.label ~text:(string_of_int disp.current_level) 
    ~packing:(tbl#attach ~left:1 ~top:0) () in
  let tlabel = GMisc.label 
    ~text:(format_time_elapsed (int_of_float (now -. disp.start_time))) 
    ~packing:(tbl#attach ~left:1 ~top:1) () in
  dlg#misc#show_all ();
  ignore (dlg#run ());
  dlg#destroy ();

(*
disp.solveddialog.level_label#set_text (string_of_int disp.current_level);
  disp.solveddialog.time_label#set_text (
    format_time_elapsed (int_of_float (now -. disp.start_time)));
(*
  begin
    match disp.solveddialog.dialog#run () with
      `DELETE_EVENT -> print_endline "got delete";
    | `OK -> print_endline "got ok";
  end;
  *)
  ignore (disp.solveddialog.dialog#run ());
  disp.solveddialog.dialog#misc#hide();
  *)
  disp.paused <- false;;

let start_level (da : GMisc.drawing_area) (disp : level_display) (lev : int) : 
    unit =
  let h = Level.height disp.level in
  let w = Level.width disp.level in
  disp.level <- Meshcreator.create_level (Meshcreator.init (get_level_size lev) w h);
  disp.current_level <- lev;
  disp.dragged <- None;
  disp.start_time <- Unix.time ();
  disp.update_level_fun disp;
  disp.update_time_fun disp;
  disp.previous_dragged <- None;
  disp.inactive_image <- None;
  refresh da disp;;

let show_intersections_activate (da : GMisc.drawing_area) 
    (disp : level_display) : unit =
  disp.show_intersections <- not disp.show_intersections;
  refresh da disp;;

let randomize_level (da : GMisc.drawing_area) (disp : level_display) : unit =
  Level.randomize_level disp.level;
  disp.inactive_image <- None;
  disp.previous_dragged <- None;
  
  idle_refresh da disp;;

let move_next_level (da : GMisc.drawing_area) (disp : level_display) : unit =
  show_solved_dialog disp;
  start_level da disp (disp.current_level + 1);;

let idle_check_intersections (da : GMisc.drawing_area) (disp : level_display) : unit =
  ignore (GMain.Idle.add (fun () ->
    if not (Level.intersections disp.level) then move_next_level da disp;
    false;));;

let button_cb (da : GMisc.drawing_area) (disp : level_display) 
    (event : GdkEvent.Button.t) : bool =
  let find_dragged drag_pt =
    try
      let vl1, mv, vl2 = Level.find_vertex disp.level (fun v ->
        abs_float (float_of_int v.coord_x -. drag_pt.x) < disp.end_radius &&
        abs_float (float_of_int v.coord_y -. drag_pt.y) < disp.end_radius) in
      disp.dragged <- Some(mv); 
      ();  
    with Not_found -> 
      begin
        disp.dragged <- None;
        ();
      end;
  in
  match GdkEvent.get_type event with
    `BUTTON_PRESS ->
      disp.drag_pt <- { x=GdkEvent.Button.x event; 
                        y=GdkEvent.Button.y event; };
      if GdkEvent.Button.button event == 1 then begin
        find_dragged disp.drag_pt;
        idle_refresh da disp;
      end else
        Printf.printf "%f %f\n%!" disp.drag_pt.x disp.drag_pt.y;
      true;
  | `BUTTON_RELEASE ->
      if GdkEvent.Button.button event == 1 then begin
        disp.dragged <- None;
        idle_refresh da disp; 
        idle_check_intersections da disp;
      end;
      true;
  | _ -> false;;

let idle_recalc_slopes (level : Level.t) (vertex : vertex) : unit =
  ignore (GMain.Idle.add (fun () ->
    ignore (Level.recalculate_slopes level vertex); false;));;

let motion_notify_cb (da : GMisc.drawing_area) (disp : level_display) 
    (event : GdkEvent.Motion.t) : bool =
  let drag_pt = { x=GdkEvent.Motion.x event ; y=GdkEvent.Motion.y event } in
  disp.drag_pt <- drag_pt;
  begin
    match disp.dragged with
      None -> ();
    | Some(v) -> 
        v.coord_x <- int_of_float drag_pt.x;
        v.coord_y <- int_of_float drag_pt.y;
        idle_recalc_slopes disp.level v;
        idle_refresh da disp;
  end;
  true;;
    
let read_glade_file () : Glade.glade_xml Gtk.obj =
  let rec findglade dir =
    match dir with
      [] -> raise Not_found;
    | head :: tail ->
      let tryfile = Path.joinpath (List.rev ("share/caboodle/caboodle.glade" :: dir)) in
      match Sys.file_exists tryfile with
        true -> tryfile;
      | false -> findglade tail;
  in
  let pieces = List.rev (Path.splitdir Sys.executable_name) in
  Glade.create ~file:(findglade (List.tl pieces)) ();;

let display : unit =
  let gladexml = read_glade_file () in
  let w = new GWindow.window (GtkWindow.Window.cast 
    (Glade.get_widget gladexml ~name:"caboodle_window")) in
  let da = new GMisc.drawing_area (GtkMisc.DrawingArea.cast 
    (Glade.get_widget gladexml ~name:"game_area")) in
  let timelabel = new GMisc.label (GtkMisc.Label.cast 
    (Glade.get_widget gladexml ~name:"time_label")) in
  let levellabel = new GMisc.label (GtkMisc.Label.cast 
    (Glade.get_widget gladexml ~name:"level_label")) in
  let sdialog = {
    dialog=new GWindow.dialog (GtkWindow.Dialog.cast
      (Glade.get_widget gladexml ~name:"level_solved_dialog"));
    level_label=new GMisc.label (GtkMisc.Label.cast
      (Glade.get_widget gladexml ~name:"dialog_level_label"));
    time_label=new GMisc.label (GtkMisc.Label.cast
      (Glade.get_widget gladexml ~name:"dialog_time_label")); 
    parent=w; } in
  let aboutdialog = new GWindow.about_dialog (GtkWindow.AboutDialog.cast
    (Glade.get_widget gladexml ~name:"about_dialog")) in
  aboutdialog#set_version version;
  ignore (aboutdialog#event#connect#delete 
    (fun _ -> aboutdialog#misc#hide (); true; ));

  let width, height = default_size in
  da#misc#set_size_request ~width:width ~height:height ();
  ignore (w#connect#destroy GMain.quit);
  let update_time disp = 
    if not disp.paused then
      begin
        let diff = int_of_float (Unix.time () -. disp.start_time) in
        timelabel#set_text (format_time_elapsed diff);
      end;
  in
  let update_level disp =
    levellabel#set_text (string_of_int disp.current_level);
  in
  let disp = create_display width height update_time update_level sdialog in
  let _ = List.iter 
    (fun (name, handler) -> 
      Glade.bind_handler ~name ~handler:(`Simple handler) gladexml)
    [ ("on_quit_activate", (fun () -> GMain.quit ()));
      ("on_about_activate", (fun () -> aboutdialog#show ()));
      ("on_randomize_level_activate", (fun () -> randomize_level da disp));
      ("on_show_intersections_activate", 
       (fun () -> show_intersections_activate da disp));
      ("on_new_activate", (fun () -> start_level da disp 1)); ] in
  update_level disp;
  update_time disp;
  ignore (GMain.Timeout.add 1000 (fun () -> update_time disp; true;));
  da#event#add [ `BUTTON_PRESS ; `BUTTON_RELEASE ; `BUTTON_MOTION ];
  ignore (da#event#connect#expose (expose_cb da disp));
  ignore (da#event#connect#configure (config_cb disp));
  ignore (da#event#connect#button_press (button_cb da disp));
  ignore (da#event#connect#button_release (button_cb da disp));
  ignore (da#event#connect#motion_notify (motion_notify_cb da disp));
  da#misc#set_can_focus true;
  w#show ();
  GMain.main ();;

let main =
  display;

