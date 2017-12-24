(* Kheireddine Anissa - 3502069
   Luong Binh Thanh - 3504859
*)

open Kdtree.Kdtree	
open Projet.Projet

let liste = create_osphere sph1
	:: create_osphere sph2
	:: create_otriangle (create_triangle (create_point 0. (-1.) 8.) (create_point (-2.) (-1.) 10.) (create_point (-1.) (0.) 10.) {couleur=create_couleur 200 0 200;ka=0.8;kd=0.1;ks=0.6;beta=10.})
	:: create_otriangle (create_triangle (create_point (1.) (-2.5) 9.) (create_point (-1.) (-1.) 9.) (create_point (1.) (-1.) 16.) {couleur=create_couleur 200 0 200;ka=0.8;kd=0.1;ks=0.6;beta=10.})
	:: create_oplane (create_plane (create_point 0. (-2.) 10.) (create_vect 0. 1. 0.) {couleur=create_couleur 0 250 0;ka=0.6;kd=0.3;ks=0.2;beta=10.})
	:: create_list_sphere_object (create_list_sphere (create_point 0. (-1.8) 10.) (create_vect 1. 0. 0.) (create_vect (0.) (0.) (-1.)) 3 0.2 materiau0);;
main_object (camera, liste, [lumiere2]);;
(* main_arbre (camera, liste, [lumiere2]) (int_of_string Sys.argv.(1));; *)
(* main_arbre (camera, liste, [lumiere2]) 1;; *)
