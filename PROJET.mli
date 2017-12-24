(* Kheireddine Anissa - 3502069
   Luong Binh Thanh - 3504859
*)

module PROJET : 
	sig
		type point = {x : float; y : float; z : float}
		type vecteur = {i : float; j : float; k : float}
		type rayon = {o : point; d : vecteur}
		type camera = {o : point; h : int; w : int; angle : float; pvise : point}
		type couleur = {r: int; g: int; b: int}
		type materiau = {couleur: couleur; ka: float; kd: float; ks: float; beta: float}
		type sphere = {c : point; rayon : float}
		type 'a comp_lumiere = {direction : 'a; couleur : couleur; intense : float}
		type lumiere = Prim of (vecteur comp_lumiere) 
			 		| Ponc of (point comp_lumiere)
		type plane = {p0 : point; p : vecteur; materiau : materiau}
		type triangle = {v0 : point; v1 : point; v2 : point}


		val create_point : int->int->int -> point
		val create_vect : int->int->int -> vecteur
		val create_couleur : int->int->int -> couleur
		val create_sphere : point -> float -> materiau -> sphere
		val create_materiau : couleur -> float -> float -> float -> float -> materiau
		val print_vec : vecteur -> unit
		val print_point : point -> unit
		val (+|) : vecteur -> vecteur -> vecteur (*addition*)
		val (>@) : vecteur -> float -> vecteur (*multiplication par un scalaire*)
		val (-|) : vecteur -> vecteur -> vecteur (*soustraction*)
		val norme : vecteur -> float
		val (|.) : vecteur -> vecteur -> float (*produit scalaire*)
		val (^|) : vecteur -> vecteur -> vecteur (*produit vectoriel*)
		val normaliser : vecteur -> vecteur
		val (+~) : point -> vecteur -> point (*ajout un vecteur à un point*)
		val (|>) : point -> point -> vecteur (*construit un vecteur à partir de 2 points*)
		val create_rayon : point -> vecteur -> rayon
		val point_to_vect : point -> vecteur
		val create_plane : point -> vecteur -> materiau -> plane
		val create_triangle : point -> point -> point -> materiau -> triangle
		val create_normal_triangle : triangle -> vecteur
		val colineaire : vecteur -> vecteur -> bool
		val orthogonal : vecteur -> vecteur -> bool
		val point_dans_plan : point -> plane -> bool
		val points_to_plan : point -> point -> point -> plane
		val rayon_dans_plan : rayon -> plane -> bool
		val borne_min_points : point -> point -> point
		val borne_max_points : point -> point -> point
		val point_sur_rayon : rayon -> point -> bool

		val intersect : sphere -> rayon -> float * vecteur
		val intersect2 : plane -> rayon -> float * vecteur
		val intersect3 : triangle -> rayon -> float * vecteur
		val laceRayon : camera -> int -> int -> rayon
		val intersect_all : rayon -> sphere list -> lumiere -> float * vecteur * int
		val illuminate : materiau -> vecteur -> vecteur -> vecteur -> float -> float -> int -> int -> int -> float * float * float
		val create_list_sphere : point -> vecteur -> vecteur -> int -> float -> materiau -> sphere list
		val continu : unit -> unit
		val main_LR : camera * sphere list * lumiere list -> unit

	end;;
