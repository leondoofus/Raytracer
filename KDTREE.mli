(* Kheireddine Anissa - 3502069
   Luong Binh Thanh - 3504859
*)

module KDTREE :
	sig
		open Projet.Projet

		type boitelimite = {c1: point;c2: point; tree: kdtree} 
		and noeud =  | Boite of boitelimite
					 | Feuille of objet list
		and kdtree = {g: noeud ; d: noeud }
		and objet = | Sphere of sphere 
					| Plane of plane 
					| Triangle of triangle
					
		exception OutOfBound

		val create_osphere : sphere -> objet
		val create_oplane : plane -> objet
		val create_otriangle : triangle -> objet
		val min_point : objet -> point
		val max_point : objet -> point
		val intersectRaySegment : rayon -> point -> point -> bool
		val intersectRayRectangle : rayon -> point -> point -> point -> point -> bool
		val intersectRayPlan : rayon -> point -> point -> point -> point -> bool
		val intersectRayBoite : rayon -> point -> point -> bool
		val intersectPlanCote : plane -> point -> point -> point -> point -> bool
		val intersectPlanBoite : plane -> point -> point -> bool
		val constr_tree : objet list -> int -> int -> point -> point -> kdtree
		val hauteur_tree : kdtree -> int
		val list_object_par_rayon : kdtree -> rayon -> point -> point -> int -> objet list
		val print_listObject : objet list -> unit
		val print_tree : kdtree -> unit
		val create_list_sphere_object : sphere list -> objet list
		val materiau_object : objet -> materiau
		val intersect_object : objet -> vecteur -> float * vecteur
		val intersect_all_object : vecteur -> objet list -> lumiere -> float * vecteur * int
		val intersect_all_object_tree : kdtree -> rayon -> lumiere -> point -> point -> float * vecteur * materiau
		val main_object : camera * objet list * lumiere list -> unit
		val main_arbre : camera * objet list * lumiere list -> unit
	end