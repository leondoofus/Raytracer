(* Kheireddine Anissa - 3502069
   Luong Binh Thanh - 3504859
*)

open Projet.Projet

let lS = sph1 :: sph2::create_list_sphere (create_point 0. (-1.8) 10.) (create_vect 1. 0. 0.) (create_vect (0.) (0.) (-1.)) 3 0.4 materiau0;;
main_LR(camera, lS, [lumiere2]);;