(* Kheireddine Anissa - 3502069
   Luong Binh Thanh - 3504859
*)

(* In charge of loading and parsing a file describing the scene.
    We decided against using ocamllex/ocamlyacc or dedicated lexers/parsers
    to keep it simple and understandable for students,
    who have not necessarily had any course about parsing and compilation.
    We also only use the standard library (though using Batteries would have been nice).

    To compile, you have to link with the Str module.
    See https://caml.inria.fr/pub/docs/manual-ocaml/libstr.html
*)

(* Vector but also camera, light, objects. Replace by you own modules *)
open Projet.Projet

(* Text and line of error *)
exception SyntaxError of string * int

let fs = float_of_string
let is = int_of_string
let si = string_of_int
let sf = string_of_float

(*
When we parse several tokens at the same time, we give to the parsing functions the number
of the token before the ones we want to parse.
*)

(* To parse a point just after the tag of the line starting after token i*)
let parse_point i tokens =
  create_point (fs tokens.(i + 1)) (fs tokens.(i + 2)) (fs tokens.(i + 3))

let parse_colour i tokens =
  create_couleur (is tokens.(i + 1)) (is tokens.(i + 2)) (is tokens.(i + 3))

let parse_camera materials (camera, objects, lights) tokens  =
  let eye = parse_point 0 tokens in
  let look_at = parse_point 3 tokens in
  let fov = fs tokens.(7) in
  let image_width = is tokens.(8) in
  let image_height = is tokens.(9) in
  ({o=eye;pvise=look_at;angle=fov ;w=image_width;h=image_height} , objects, lights)

let parse_light materials (camera, objects, lights) tokens =
  let light_direction = normaliser (point_to_vect (parse_point 0 tokens)) in
  let light_colour = parse_colour 3 tokens in 
  let light_intensity = fs tokens.(7) in
  (camera, objects, Prim {dir_pt = light_direction; couleur = light_colour; intense = light_intensity}::lights)

(* let parse_light materials (camera, objects, lights) tokens =
  let light_direction = parse_point 0 tokens in
  let light_colour = parse_colour 3 tokens in 
  let light_intensity = fs tokens.(7) in
  (camera, objects, Ponc {dir_pt = light_direction; couleur = light_colour; intense = light_intensity}::lights);; *)

let parse_sphere materials (camera, objects, lights) tokens =
  let center = parse_point 0 tokens in
  let radius = fs tokens.(4) in
  let sphere_material = Hashtbl.find materials tokens.(5) in
  (camera, create_sphere center radius sphere_material::objects, lights)

let parse_material materials scene tokens =
  let name = tokens.(1) in
  let colour  = parse_colour 1 tokens in
  let k = parse_point 4 tokens in
  let alpha = fs tokens.(8) in
  Hashtbl.add materials name (create_materiau colour k.x k.y k.z alpha);
  scene

(** Load the scene, as a triplet (camera option, object list, light list) *)
let load_scene fileName =
  let file = open_in fileName in
  let blank_splitter = Str.regexp "[ \t]+" in
  let numLine = ref 0 in
  let rec parse_line materials scene =
    incr numLine;
    try
      let line = input_line file in
      let tokens = Array.of_list (Str.split blank_splitter line) in
      let new_scene  = (match tokens.(0) with
       | "camera" -> parse_camera
       | "light"  -> parse_light
       | "sphere" -> parse_sphere
       | "material" -> parse_material
       | _ as t -> raise (SyntaxError("Unrecognized object: " ^ t, !numLine)))
          materials scene tokens
      in
      parse_line materials new_scene
    with  End_of_file -> scene
  in
  try
    parse_line (Hashtbl.create 10) ({o=create_point 0. 0. 0.;h=0;w=0;angle=0.;pvise=create_point 0. 0. 0.}, [], [])
  with
  | Invalid_argument _ -> raise (SyntaxError( "Not enough arguments", !numLine))

(** Save the scene, from a triplet (camera option, object list, light list) *)
let save_scene scene fileName = 
  let file = open_out fileName
  and camera, objects, lights = scene in
  let indice = ref 0 in
  let push_camera (camera : camera) =
    output_string file ("camera "^(sf camera.o.x)^" "^(sf camera.o.y)^" "^(sf camera.o.z)
                      ^" "^(sf camera.pvise.x)^" "^(sf camera.pvise.y)^" "^(sf camera.pvise.z)
                      ^" "^(sf camera.angle)^" "^(si camera.w)^" "^(si camera.h)^"\n")
  in let rec push_objects (objects : sphere list) =
    incr indice;
    match objects with
    | [] -> ()
    | h::q -> output_string file ("material sphereMat"^(si !indice)^" "^(si h.materiau.couleur.r)
              ^" "^(si h.materiau.couleur.g)^" "^(si h.materiau.couleur.b)^" "^(sf h.materiau.ka)
              ^" "^(sf h.materiau.kd)^" "^(sf h.materiau.ks)^" "^(sf h.materiau.beta)^"\n"
              ^"sphere "^(sf h.c.x)^" "^(sf h.c.y)^" "^(sf h.c.z)^" "^(sf h.rayon)
              ^" sphereMat"^(si !indice)^"\n");
              push_objects q
  in let rec push_lights (lights : lumiere list) =
    match lights with
    | [] -> ()
    | Prim {dir_pt = d;couleur = c;intense = ins}::q -> output_string file ("light "^(sf d.i)^" "^(sf d.j)^" "^(sf d.k)^" "^(si c.r)^" "^(si c.g)^" "^(si c.b)^" "^(sf ins)^"\n");
                                                        push_lights q
    | Ponc {dir_pt = o;couleur = c;intense = ins}::q -> output_string file ("light "^(sf o.x)^" "^(sf o.y)^" "^(sf o.z)^" "^(si c.r)^" "^(si c.g)^" "^(si c.b)^" "^(sf ins)^"\n");
                                                        push_lights q
  in    
  push_camera camera;
  push_objects objects;
  push_lights lights;
  close_out file

exception NotEnoughArguments
let main () =
  match Array.length Sys.argv with
  | 0 -> raise NotEnoughArguments
  | _ -> main_LR (load_scene Sys.argv.(1));;
main ();; 