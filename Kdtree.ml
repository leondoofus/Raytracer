(* Kheireddine Anissa - 3502069
   Luong Binh Thanh - 3504859
*)

module Kdtree =
struct
	open Graphics
	open Unix
	open Projet.Projet

	type boitelimite = {c1: point;c2: point; tree: kdtree} 

	and noeud =  | Boite of boitelimite
				 | Feuille of objet list

	and kdtree = {g: noeud ; d: noeud }

	and objet = | Sphere of sphere 
				| Plane of plane 
				| Triangle of triangle
	
	exception OutOfBound

	let create_osphere sphere = Sphere (sphere)
	let create_oplane plane = Plane (plane)
	let create_otriangle triangle = Triangle (triangle)

	(* Min et max sont les points min et max du carré englobant l'objet *)
	let min_point objet =
		match objet with
		| Sphere s -> create_point (s.c.x -. s.rayon) (s.c.y -. s.rayon) (s.c.z -. s.rayon) 
		| Triangle t -> create_point (min (min t.v0.x t.v1.x) t.v2.x) (min (min t.v0.y t.v1.y) t.v2.y) (min (min t.v0.z t.v1.z) t.v2.z)
		| _ -> raise OutOfBound

	let max_point objet =
		match objet with
		| Sphere s -> create_point (s.c.x +. s.rayon) (s.c.y +. s.rayon) (s.c.z +. s.rayon)
		| Triangle t -> create_point (max (max t.v0.x t.v1.x) t.v2.x) (max (max t.v0.y t.v1.y) t.v2.y) (max (max t.v0.z t.v1.z) t.v2.z)
		| _ -> raise OutOfBound 

	let intersectRaySegment (rayon : rayon) p1 p2 = (*partiel, on traite seulement le cas où le rayon et les points sont dans le même plan, le cas où 2 rayons se superposent n'est pas calculé*)
		let v = p1 |> p2 in
		if colineaire rayon.d v then false
		else
			begin
				let a1 = rayon.d.i
				and b1 = -. v.i
				and c1 = p1.x -. rayon.o.x
				and a2 = rayon.d.j
				and b2 = -. v.j
				and c2 = p1.y -. rayon.o.y in
				let dx = (c1 *. b2) -. (c2 *. b1)
				and dy = (a1 *. c2) -. (a2 *. c1)
				and d = (a1 *. b2) -. (a2 *. b1) in
				let x = dx /. d
				and y = dy /. d in
				0. < y && y < 1. && x >= 0.
			end

	let intersectRayRectangle rayon p1 p2 p3 p4 = (*partiel, on traite seulement le cas où le rayon et les points sont dans le même plan*)
		if point_sur_rayon rayon p1 || point_sur_rayon rayon p2 || point_sur_rayon rayon p3 || point_sur_rayon rayon p4 then true
		else intersectRaySegment rayon p1 p2 || intersectRaySegment rayon p2 p3 || intersectRaySegment rayon p3 p4 || intersectRaySegment rayon p4 p1

	let intersectRayPlan rayon p1 p2 p3 p4 =
		if point_sur_rayon rayon p1 || point_sur_rayon rayon p2 || point_sur_rayon rayon p3 || point_sur_rayon rayon p4 then true
		else
			begin
				let plan = points_to_plan p1 p2 p3 in
				if rayon_dans_plan rayon plan then intersectRayRectangle rayon p1 p2 p3 p4
				else if orthogonal rayon.d plan.p then false
				else
					begin
						let numerateur = plan.p.i *. plan.p0.x +. plan.p.j *. plan.p0.y +. plan.p.k *. plan.p0.z -. plan.p.i *.rayon.o.x -. plan.p.j *.rayon.o.y -. plan.p.k *.rayon.o.z
						and denominateur = plan.p.i *. rayon.d.i +. plan.p.j *. rayon.d.j +. plan.p.k *. rayon.d.k in
						let t = numerateur /. denominateur in
						if t < 0. then false
						else
							begin
								let i = rayon.o +~ (rayon.d >@ t) in
								(p1.x <= i.x && p1.y <= i.y && p1.z <= i.z && i.x <= p3.x && i.y <= p3.y && i.z <= p3.z) || (p3.x <= i.x && p3.y <= i.y && p3.z <= p1.z && i.x <= p1.x && i.y <= p1.y && i.z <= p1.z)
							end
					end
			end

	let intersectRayBoite rayon p1 p2 =
		let c1 = borne_min_points p1 p2
		and c2 = borne_max_points p1 p2
		in
		let x1,y1,z1 = c1.x,c1.y,c1.z 
		and x2,y2,z2 = c2.x,c2.y,c2.z
		in
		let c3 = create_point x1 y2 z1 
		and c4 = create_point x2 y2 z1
		and c5 = create_point x2 y1 z1
		and c6 = create_point x1 y2 z2 
		and c7 = create_point x2 y1 z2
		and c8 = create_point x1 y1 z2
		in
		intersectRayPlan rayon c1 c3 c4 c5 || intersectRayPlan rayon c5 c4 c2 c7 || intersectRayPlan rayon c8 c6 c2 c7 ||
		intersectRayPlan rayon c1 c3 c6 c8 || intersectRayPlan rayon c1 c8 c7 c5 || intersectRayPlan rayon c3 c6 c2 c4

	let intersectPlanCote plan p1 p2 p3 p4 =
		if point_dans_plan p1 plan || point_dans_plan p2 plan || point_dans_plan p3 plan || point_dans_plan p4 plan then true
		else
			not (colineaire (points_to_plan p1 p2 p3).p plan.p)

	let intersectPlanBoite plan p1 p2 = 
		let c1 = borne_min_points p1 p2
		and c2 = borne_max_points p1 p2
		in
		let x1,y1,z1 = c1.x,c1.y,c1.z 
		and x2,y2,z2 = c2.x,c2.y,c2.z
		in
		let c3 = create_point x1 y2 z1 
		and c4 = create_point x2 y2 z1
		and c5 = create_point x2 y1 z1
		and c6 = create_point x1 y2 z2 
		and c7 = create_point x2 y1 z2
		and c8 = create_point x1 y1 z2
		in
		intersectPlanCote plan c1 c3 c4 c5 || intersectPlanCote plan c5 c4 c2 c7 || intersectPlanCote plan c7 c2 c6 c8 ||
		intersectPlanCote plan c8 c6 c3 c1 || intersectPlanCote plan c1 c8 c7 c5 || intersectPlanCote plan c3 c6 c2 c4

	let rec constr_tree listObject k kmax c1 c2 =
		let median = create_point ((c1.x +. c2.x) /. 2.) ((c1.y +. c2.y) /. 2.) ((c1.z +. c2.z) /. 2.)
		and lgauche = ref [] 
		and ldroite = ref []
		and maxgauche = ref c1
		and mindroit = ref c2
		and constr_noeud listObject k kmax c1 c2 =
			if k >= kmax || List.length listObject <= 1 then
				Feuille (listObject)
			else
				Boite ({c1=c1;c2=c2;tree=constr_tree listObject k kmax c1 c2})
		in
		match listObject with
		| [] -> {g=Feuille([]);d=Feuille([])}
		| _ ->
		begin
			for i=0 to List.length listObject - 1 do
				let elem = List.nth listObject i in
				match elem with
				| Plane p ->
					begin
						let c1',c2' =
							match (k mod 3) with
							| 0 -> create_point ((c1.x +. c2.x) /. 2.) c2.y c2.z, create_point ((c1.x +. c2.x) /. 2.) c1.y c1.z
							| 1 -> create_point c2.x ((c1.y +. c2.y) /. 2.) c2.z, create_point c1.x ((c1.y +. c2.y) /. 2.) c1.z
	 						| _ -> create_point c2.x c2.y ((c1.z +. c2.z) /. 2.), create_point c1.x c1.y ((c1.z +. c2.z) /. 2.)
	 					in
	 					if intersectPlanBoite p c1 c1' then lgauche := elem :: !lgauche;
	 					if intersectPlanBoite p c2' c2 then ldroite := elem :: !ldroite
					end
				| _ ->
				begin
					let min_e = min_point elem and max_e = max_point elem in

					if (k mod 3) = 0 then
						begin	
							if min_e.x <= median.x then
								begin
									lgauche := elem :: !lgauche;
									maxgauche := borne_max_points !maxgauche max_e
								end;
							if median.x < max_e.x then
								begin
									ldroite := elem :: !ldroite;
									mindroit := borne_min_points !mindroit min_e
								end
						end
					else if (k mod 3) = 1 then
						begin
							
							if min_e.y <= median.y then
								begin
									lgauche := elem :: !lgauche;
									maxgauche := borne_max_points !maxgauche max_e
								end;
							if median.y < max_e.y then
								begin
									ldroite := elem :: !ldroite;
									mindroit := borne_min_points !mindroit min_e
								end
						end
					else
						begin
							
							if min_e.z <= median.z then
								begin
									lgauche := elem :: !lgauche;
									maxgauche := borne_max_points !maxgauche max_e
								end;
							if median.z < max_e.z then
								begin
									ldroite := elem :: !ldroite;
									mindroit := borne_min_points !mindroit min_e
								end
						end
				end
			done;
			if !lgauche = !ldroite then {g = Feuille(!lgauche); d = Feuille (!ldroite)}
			else {g = constr_noeud !lgauche (k+1) kmax c1 !maxgauche; d = constr_noeud !ldroite (k+1) kmax !mindroit c2}
		end

	let rec hauteur_tree tree =
		let hauteur_noeud noeud =
			match noeud with
			| Feuille f -> 0
			| Boite {c1=c1;c2=c2;tree=tree} -> hauteur_tree tree
		in
		1 + max (hauteur_noeud tree.g) (hauteur_noeud tree.d)

	let rec list_object_par_rayon tree rayon c1 c2 k = 
		let list_noeud noeud rayon k =
			match noeud with
			| Feuille f -> f
			| Boite b -> list_object_par_rayon b.tree rayon b.c1 b.c2 k
		in
		let c1',c2' =
			match (k mod 3) with
			| 0 -> create_point ((c1.x +. c2.x) /. 2.) c2.y c2.z, create_point ((c1.x +. c2.x) /. 2.) c1.y c1.z
			| 1 -> create_point c2.x ((c1.y +. c2.y) /. 2.) c2.z, create_point c1.x ((c1.y +. c2.y) /. 2.) c1.z
 			| _ -> create_point c2.x c2.y ((c1.z +. c2.z) /. 2.), create_point c1.x c1.y ((c1.z +. c2.z) /. 2.)
 			in
 			match intersectRayBoite rayon c1 c1', intersectRayBoite rayon c2' c2 with
 			| true,false -> list_noeud tree.g rayon (k+1)
 			| false,true -> list_noeud tree.d rayon (k+1)
 			| true,true -> (list_noeud tree.g rayon (k+1)) @ (list_noeud tree.d rayon (k+1))
 			| _,_ -> []

	let rec print_listObject lO=
		match lO with
		| [] -> print_string "Fin de print list\n"
		| Sphere s :: q -> print_string "Sphere at "; print_point s.c; print_listObject q
		| Triangle t :: q-> print_string "Triangle at \n\t"; print_point t.v0; print_string "\t"; print_point t.v1; print_string "\t"; print_point t.v2; print_listObject q
		| Plane p :: q -> print_string "Plan at \n"; print_point p.p0; print_vec p.p; print_listObject q

	let rec print_tree tree =
		let rec print_noeud noeud =
			match noeud with
			| Feuille f -> print_string "Feuille \n"; print_listObject f
			| Boite {c1=c1;c2=c2;tree=tree} -> print_string "Boite \n"; (* print_point c1; print_point c2; *) print_tree tree
		in
		print_string "Gauche \n\t\t";
		print_noeud tree.g;
		print_string "Milieu\n";
		print_string "Droit \n\t\t";
		print_noeud tree.d;;

	let rec create_list_sphere_object l =
		match l with
		| [] -> []
		| h :: q -> create_osphere h :: create_list_sphere_object q

	let materiau_object o =
		match o with
			| Sphere s -> s.materiau
			| Triangle t -> t.materiau
			| Plane p -> p.materiau

	let intersect_object o rayon =
		match o with
			| Sphere s -> intersect s rayon
			| Triangle t -> intersect3 t rayon
			| Plane p -> intersect2 p rayon

	let intersect_all_object ray lO lum =
		let res = ref (infinie,vectnul,0) in
		let dmin = ref infinie in
		let indice = ref 0 in
		for i = 0 to List.length lO - 1 do
			let h = List.nth lO i in 	
			let dist,normale = intersect_object h ray in
				if dist < !dmin then 
				begin 
					res := dist,normale,i; 
					dmin := dist;
					indice := i;
				end;
		done;
		match !dmin with
			| d when d = infinie -> !res
			| _ ->
				begin
					let vec_lum =
						match lum with
							| Prim ({dir_pt = d;couleur=c;intense=ii}) -> d
							| Ponc ({dir_pt = o;couleur=c;intense=ii}) -> o |> (ray.o +~ (ray.d >@ !dmin))
					in let new_rayon = create_rayon (ray.o +~ (ray.d >@ !dmin)) (normaliser (vec_lum >@ (-1.))) in
					for i = 0 to List.length lO - 1 do
						let h = List.nth lO i in 
						let dist,normale = intersect_object h new_rayon in
							if dist < infinie then res := ombre,normale,!indice;
					done;
				end;
				!res

	let intersect_all_object_tree tree ray lum c1 c2 =
		let lO = list_object_par_rayon tree ray c1 c2 0 in
		match lO with
		| [] -> infinie,vectnul,{couleur=create_couleur 0 0 0;ka=0.;kd=0.;ks=0.;beta=0.}
		| _ ->
		begin
			let res = ref (infinie,vectnul,{couleur=create_couleur 0 0 0;ka=0.;kd=0.;ks=0.;beta=0.}) in
			let dmin = ref infinie in
			let mat = ref {couleur=create_couleur 0 0 0;ka=0.;kd=0.;ks=0.;beta=0.} in
			for i = 0 to List.length lO - 1 do
				let h = List.nth lO i in 	
				let dist,normale = intersect_object h ray in
					if dist < !dmin then 
					begin 
						res := dist,normale,materiau_object h; 
						dmin := dist;
						mat := materiau_object h;
					end;
			done;
			match !dmin with
				| d when d = infinie -> !res
				| _ ->
					begin
						let vec_lum =
							match lum with
								| Prim ({dir_pt = d;couleur=c;intense=ii}) -> d
								| Ponc ({dir_pt = o;couleur=c;intense=ii}) -> o |> (ray.o +~ (ray.d >@ !dmin))
						in let new_rayon = create_rayon (ray.o +~ (ray.d >@ !dmin)) (normaliser (vec_lum >@ (-1.))) in
						let lO = list_object_par_rayon tree new_rayon c1 c2 0 in
						match lO with
						| [] -> !res
						| _ ->
							for i = 0 to List.length lO - 1 do
								let h = List.nth lO i in 
								let dist,normale = intersect_object h new_rayon in
									if dist < infinie then res := ombre,normale,!mat;
							done;
						!	res
					end;
		end

	let main_object scene =
		let t = Unix.gettimeofday () in
	    Printf.printf "Temps debut %f\n" t;
		let camera, lO, lL = scene in
		let lumiere =
			match lL with
				| h::q -> h
				| [] -> Prim {dir_pt = vectnul;couleur= create_couleur 0 0 0; intense= 0.}
		in
		open_graph (" "^string_of_int(camera.w)^"x"^string_of_int(camera.h));
		fill_rect 0 0 camera.w camera.h;
		for i = 0 to camera.w - 1 do
			for j = 0 to camera.h - 1 do
				let rayon = lanceRayon camera i j in
				let distance,normale,indice = intersect_all_object rayon lO lumiere in
				let materiau = materiau_object (List.nth lO indice) in
				let r,g,b =	if distance = infinie then (49,79,79) (*Si rayon ne touche rien*)
							else
								begin
									let v = rayon.d >@ (-1.) in
									let llight, distance_to_lum, lum_intense, lum_r, lum_g, lum_b =
										match lumiere with
											| Prim {dir_pt = d;couleur=c;intense=ins} -> normaliser (d >@ (-1.)), infinie, ins, c.r, c.g, c.b
											| Ponc {dir_pt = o;couleur=c;intense=ins} -> let direct_lum = (camera.o +~ (rayon.d >@ distance)) |> o in normaliser direct_lum, norme direct_lum, ins, c.r, c.g, c.b
									in let h = normaliser (llight +| v) in
									if distance = ombre then illuminate materiau normale h llight lum_intense 0. lum_r lum_g lum_b
									else
									illuminate materiau normale h llight distance_to_lum lum_intense lum_r lum_g lum_b
								end in
				set_color (rgb r g b);
				plot i j;
			done
		done;
		let t' = Unix.gettimeofday () in
    	Printf.printf "Temps fin %f\n" t';
    	Printf.printf "Temps d'exec %f\n" (( t' -. t ) /. 10.);
		while continu () do
			();
		done;
		Graphics.close_graph

	let main_arbre scene kmax=
		let t = Unix.gettimeofday () in
	    Printf.printf "Temps debut %f\n" t;
		let camera, lO, lL = scene in
		let lumiere =
			match lL with
				| h::q -> h
				| [] -> Prim {dir_pt = vectnul;couleur= create_couleur 0 0 0; intense= 0.}
		in
		let c1,c2 = create_point (-.infinie) (-.infinie) (-.infinie), create_point infinie infinie infinie
		in
		let tree = constr_tree lO 0 kmax c1 c2
		in
		open_graph (" "^string_of_int(camera.w)^"x"^string_of_int(camera.h));
		fill_rect 0 0 camera.w camera.h;
		for i = 0 to camera.w - 1 do
			for j = 0 to camera.h - 1 do
				let rayon = lanceRayon camera i j in
				let distance,normale,materiau = intersect_all_object_tree tree rayon lumiere c1 c2 in
				let r,g,b =	if distance = infinie then (49,79,79) (*Si rayon ne touche rien*)
							else
								begin
									let v = rayon.d >@ (-1.) in
									let llight, distance_to_lum, lum_intense, lum_r, lum_g, lum_b =
										match lumiere with
											| Prim {dir_pt = d;couleur=c;intense=ins} -> normaliser (d >@ (-1.)), infinie, ins, c.r, c.g, c.b
											| Ponc {dir_pt = o;couleur=c;intense=ins} -> let direct_lum = (camera.o +~ (rayon.d >@ distance)) |> o in normaliser direct_lum, norme direct_lum, ins, c.r, c.g, c.b
									in let h = normaliser (llight +| v) in
									if distance = ombre then illuminate materiau normale h llight lum_intense 0. lum_r lum_g lum_b
									else
									illuminate materiau normale h llight distance_to_lum lum_intense lum_r lum_g lum_b
								end in
				set_color (rgb r g b);
				plot i j;
			done
		done;
 		let t' = Unix.gettimeofday () in
    	Printf.printf "Temps fin %f\n" t';
    	Printf.printf "Temps d'exec %f\n" (( t' -. t ) /. 10.);
		while continu () do
			();
		done;
		Graphics.close_graph
end;;