(* Kheireddine Anissa - 3502069
   Luong Binh Thanh - 3504859
*)

module Projet = 

struct	
	open Graphics

	type point = {x : float; y : float; z : float}
	type vecteur = {i : float; j : float; k : float}
	type rayon = {o : point; d : vecteur}
	type camera = {o : point; h : int; w : int; angle : float; pvise : point}
	type couleur = {r: int; g: int; b: int}
	type materiau = {couleur: couleur; ka: float; kd: float; ks: float; beta: float}
	type sphere = {c : point; rayon : float; materiau : materiau}
	type 'a comp_lumiere = {dir_pt : 'a; couleur : couleur; intense : float}
	type lumiere = Prim of (vecteur comp_lumiere) 
				 | Ponc of (point comp_lumiere)
	type plane = {p0 : point; p : vecteur; materiau : materiau}
	type triangle = {v0 : point; v1 : point; v2 : point; materiau : materiau}

	let create_point a b c = {x=a;y=b;z=c}

	let create_vect a b c =	{i=a;j=b;k=c}

	let create_couleur a b c = {r=a;g=b;b=c}

	let create_sphere o r m = {c=o;rayon=r;materiau=m}

	let create_materiau c ka kd ks beta = {couleur=c;ka=ka;kd=kd;ks=ks;beta=beta}

	let ia = 0.2
	let pi = 4. *. atan 1.
	let epsilon = 0.00000000001 (*Erreur calcul*)
	let infinie = max_float
	let ombre = (-1.)

	let vectnul = create_vect 0. 0. 0.
	let origine = create_point 0. 0. 0.
	let camera = {o=origine;h=500;w=700;angle=(8.*.pi)/.3.;pvise=create_point 0. 0. 5.}
	let materiau0 = {couleur=create_couleur 255 0 0;ka=0.4;kd=0.1;ks=0.7;beta=10.}
	let materiau1 = {couleur=create_couleur 200 130 0;ka=0.8;kd=0.1;ks=0.6;beta=10.}
	let materiau2 = {couleur=create_couleur 100 150 150;ka=0.8;kd=0.1;ks=0.5;beta=10.}
	let sph1 = {c=create_point 0. 0. 10.;rayon=0.5;materiau=materiau1}
	let sph2 = {c=create_point 2. 0. 10.;rayon=0.7;materiau=materiau2}

	let lumiere = Prim {dir_pt= create_vect 1. (-2.) (1.);couleur= create_couleur 255 255 255;intense= 3000.}
	let lumiere2 = Ponc {dir_pt=create_point (-2.) 3. 8.;couleur= create_couleur 255 255 255;intense= 2000.}
	

	let print_vec v =
		print_string ("Vecteur ["^(string_of_float v.i)^" "^(string_of_float v.j)^" "^(string_of_float v.k)^"]\n")

	let print_point p =
		print_string ("Point ("^(string_of_float p.x)^" "^(string_of_float p.y)^" "^(string_of_float p.z)^")\n")

	let (+|) v1 v2 = (*addition*)
		create_vect (v1.i +. v2.i) (v1.j +. v2.j) (v1.k +. v2.k)

	let (>@) v s = (*multiplication par un scalaire*)
		create_vect (v.i *. s) (v.j *. s) (v.k *. s)

	let (-|) v1 v2 = (*soustraction*)
		v1 +| (v2 >@ (-1.));;

	let norme v =
		sqrt ((v.i **2.) +. (v.j  **2.) +. (v.k **2.))

	let (|.) v1 v2 = (*produit scalaire*)
		(v1.i *. v2.i) +. (v1.j *. v2.j) +. (v1.k *. v2.k)

	let (^|) v1 v2 = (*produit vectoriel*)
		create_vect ((v1.j *. v2.k) -. (v2.j *. v1.k)) ((v1.k *. v2.i) -. (v2.k *. v1.i)) ((v1.i *. v2.j) -. (v2.i *. v1.j))

	let normaliser v =
	 	v >@ (1./.(norme v))

	let (+~) p v = (*ajout un vecteur à un point*)
		create_point (p.x +. v.i) (p.y +. v.j) (p.z +. v.k)
	  
	let (|>) p1 p2 = (*construit un vecteur à partir de 2 points*)
		create_vect (p2.x -. p1.x) (p2.y -. p1.y) (p2.z -. p1.z)

	let create_rayon o d =
	 	{o=o;d=normaliser d}

	let point_to_vect p =
		create_vect p.x p.y p.z

	let create_plane p0 p materiau=
		{p0=p0;p=normaliser p;materiau=materiau}

	let create_triangle v0 v1 v2 materiau=
		{v0=v0;v1=v1;v2=v2;materiau=materiau}

	let create_normal_triangle triangle =
		let u = triangle.v0 |> triangle.v1
		and v = triangle.v0 |> triangle.v2
		in normaliser (u ^| v)

	let colineaire v1 v2 =
		if v1 = vectnul || v2 = vectnul then true
		else
			(v1.i *. v2.j = v1.j *. v2.i) && (v1.i *. v2.k = v1.k *. v2.i) && (v1.j *. v2.k = v1.k *. v2.j)

	let orthogonal v1 v2 =
		(v1 |. v2) = 0.

	let point_dans_plan point plan=
		((plan.p0 |> point) |. plan.p) = 0.

	let points_to_plan p1 p2 p3 =
		let norm = normaliser ((p1 |> p2) ^| (p1 |> p3)) in
		create_plane p1 norm {couleur=create_couleur 0 0 0;ka=0.;kd=0.;ks=0.;beta=0.}

	let rayon_dans_plan (rayon : rayon) plan = 
		(point_dans_plan rayon.o plan) && (orthogonal rayon.d plan.p)

	let borne_min_points p1 p2 =
		create_point (min p1.x p2.x) (min p1.y p2.y) (min p1.z p2.z)

	let borne_max_points p1 p2 =
		create_point (max p1.x p2.x) (max p1.y p2.y) (max p1.z p2.z)

	let point_sur_rayon (rayon : rayon) p =
		let v = normaliser (rayon.o |> p) in
		colineaire rayon.d v && v.i *. rayon.d.i >= 0.

	let intersect sphere rayon =
		let d = rayon.d in
		let x0 = rayon.o.x
		and y0 = rayon.o.y
		and z0 = rayon.o.z
		and xd = d.i
		and yd = d.j
		and zd = d.k
		and xc = sphere.c.x
		and yc = sphere.c.y
		and zc = sphere.c.z
		and r = sphere.rayon in
		let a = (xd *. xd) +. (yd *. yd) +. (zd *. zd)
		and b = (2. *. xd *. (x0 -. xc)) +. (2. *. yd *. (y0 -. yc)) +. (2. *. zd *. (z0 -. zc))
		and c = ((x0 -. xc) ** 2.) +. ((y0 -. yc) ** 2.) +. ((z0 -. zc) ** 2.) -. (r *. r) in
		if a = 0. then
			begin
				let t = -. b /. c in
				if t >= epsilon then (t, normaliser (sphere.c |> (rayon.o +~ (d >@ t))))
				else (infinie, vectnul)
			end
		else
			begin
				let delta = (b *. b) -. (4. *. a *. c) in
				match delta with
					| rr when rr >= 0. ->
						begin
							let t1 = (-.b +. (sqrt delta)) /. (2. *. a)
							and t2 = (-.b -. (sqrt delta)) /. (2. *. a) in
							match t1,t2 with
								| r1,r2 when r1 >= epsilon && r2 >= epsilon ->
									if (r1 <= r2) then (r1, normaliser (sphere.c |> (rayon.o +~ (d >@ t1))))
									else (r2, normaliser (sphere.c |> (rayon.o +~ (d >@ t2))))
								| r1,r2 when r1 >= epsilon -> (r1, normaliser (sphere.c |> (rayon.o +~ (d >@ t1))))
								| r1,r2 when r2 >= epsilon -> (r2, normaliser (sphere.c |> (rayon.o +~ (d >@ t2))))
								| _,_ -> (infinie, vectnul)
						end
					| _ -> (infinie, vectnul)
			end

	let intersect2 plan rayon =
		let vp = rayon.d |. plan.p in
		match vp with
			| 0. -> (infinie, vectnul)
			| _ ->
			begin
				let t = ((rayon.o |> plan.p0) |. plan.p) /. vp in
				match t with
					| r when r < epsilon -> (infinie, vectnul)
					| _ -> (t, plan.p)
			end

	let intersect3 triangle rayon =
		let p = create_plane triangle.v0 (create_normal_triangle triangle) triangle.materiau in
		let dist,norm = intersect2 p rayon in
		match dist with
		| d when d = infinie -> (infinie,vectnul)
		| _ ->
			begin
				let m = rayon.o +~ (rayon.d >@ dist)
				and a = triangle.v0
				and b = triangle.v1
				and c = triangle.v2
				in
				let ab = a |> b
				and am = a |> m
				and ac = a |> c
				and ba = b |> a
				and bm = b |> m
				and bc = b |> c
				and ca = c |> a
				and cm = c |> m
				and cb = c |> b
				in
				let p1 = (ab ^| am) |. (am ^| ac)
				and p2 = (ba ^| bm) |. (bm ^| bc)
				and p3 = (ca ^| cm) |. (cm ^| cb)
				in
				if (p1 >= 0. && p2 >= 0. && p3 >= 0.) then (dist,norm)
				else (infinie,vectnul)
			end

	let lanceRayon camera i j =
		let up = create_vect 0. 1. 0.
		and op = normaliser (camera.o |> camera.pvise) in
		let u = up ^| op in
		let v = op ^| u
		and image_ratio = (float_of_int camera.h) /. (float_of_int camera.w) in
		let half_height = image_ratio *. (tan (camera.angle /. 2.))
		and half_width = tan (camera.angle /. 2.) in
		let c = camera.pvise +~ (u >@ (-1. *. half_width)) +~ (v >@ (-1. *. half_height)) in
		let x = u >@ ((2. *. half_width) /. (float_of_int camera.w)) in
		let y = v >@ ((2. *. half_height) /. (float_of_int camera.h)) in
		create_rayon camera.o (camera.o |>  (c +~ (x >@(float_of_int i)) +~ (y >@ (float_of_int j))))	

	let intersect_all ray lS lum =
		let res = ref (infinie,vectnul,0) in
		let dmin = ref infinie in
		let indice = ref 0 in
		for i = 0 to List.length lS - 1 do
			let h = List.nth lS i in 	
			let dist,normale = intersect h ray in
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
					for i = 0 to List.length lS - 1 do
						let h = List.nth lS i in 	
						let dist,normale = intersect h new_rayon in
							if dist < infinie then res := ombre,normale,!indice;
					done;
				end;
				!res


	let illuminate (materiau:materiau) normale h llight distance_to_lum lum_intense lum_r lum_g lum_b=
		let intensity =
			match distance_to_lum with
			| d when d = infinie -> lum_intense
			| _ -> max 0. (lum_intense -. (4. *. pi *. (distance_to_lum ** 2.)))
		in
		let total = lum_r + lum_g + lum_b in
		let percent_r = (float_of_int lum_r) /. (float_of_int total)
		and percent_g = (float_of_int lum_g) /. (float_of_int total)
		and percent_b = (float_of_int lum_b) /. (float_of_int total) in
		let ia_r = (float_of_int lum_r) *. ia *. (float_of_int materiau.couleur.r) *.materiau.ka/.255.
		and ia_g = (float_of_int lum_g) *. ia *. (float_of_int materiau.couleur.g) *.materiau.ka/.255.
		and ia_b = (float_of_int lum_b) *. ia *. (float_of_int materiau.couleur.b) *.materiau.ka/.255.

		and id_r = materiau.kd *. (llight |. normale) *. intensity *. (float_of_int materiau.couleur.r) /. 255.
		and id_g = materiau.kd *. (llight |. normale) *. intensity *. (float_of_int materiau.couleur.g) /. 255.
		and id_b = materiau.kd *. (llight |. normale) *. intensity *. (float_of_int materiau.couleur.b) /. 255.

		and is_r = percent_r *. ((h |. normale) ** materiau.beta) *. materiau.ks *. intensity *. (float_of_int materiau.couleur.r) /. 255.
		and is_g = percent_g *. ((h |. normale) ** materiau.beta) *. materiau.ks *. intensity *. (float_of_int materiau.couleur.g) /. 255.
		and is_b = percent_b *. ((h |. normale) ** materiau.beta) *. materiau.ks *. intensity *. (float_of_int materiau.couleur.b) /. 255.

		in max (min (int_of_float (ia_r +. id_r +. is_r)) 255) (int_of_float ia_r), max (min (int_of_float (ia_g +. id_g +. is_g)) 255) (int_of_float ia_g), max (min (int_of_float (ia_b +. id_b +. is_b)) 255) (int_of_float ia_b)
		

	let create_list_sphere point u v dim rayon materiau =
		let top = point +~ ((u +| v)  >@ (rayon *. (1. -. (2. *. (float_of_int dim)))))
		and u' = normaliser u
		and v' = normaliser v
		and list_sphere = ref [] in
		for i = 0 to 2 * dim - 1 do
			for j = 0 to 2 * dim - 1 do
				list_sphere := create_sphere (top +~ (u' >@ ((float_of_int i) *. 2. *. rayon)) +~ (v' >@ ((float_of_int j) *. 2. *. rayon))) rayon materiau:: !list_sphere;
			done
		done;
		!list_sphere


	let continu () =
		let ev = Graphics.wait_next_event[Graphics.Key_pressed] in
			ev.Graphics.key <> 'q'
		
	let main_LR scene =
		let camera, lS, lL = scene in
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
				let distance,normale,sphere_ind = intersect_all rayon lS lumiere in
				let materiau_sph = (List.nth lS sphere_ind).materiau in
				let r,g,b =	if distance = infinie then (49,79,79) (*Si rayon ne touche rien*)
							else
								begin
									let v = rayon.d >@ (-1.) in
									let llight, distance_to_lum, lum_intense, lum_r, lum_g, lum_b =
										match lumiere with
											| Prim {dir_pt = d;couleur=c;intense=ins} -> normaliser (d >@ (-1.)), infinie, ins, c.r, c.g, c.b
											| Ponc {dir_pt = o;couleur=c;intense=ins} -> let direct_lum = (camera.o +~ (rayon.d >@ distance)) |> o in normaliser direct_lum, norme direct_lum, ins, c.r, c.g, c.b
									in let h = normaliser (llight +| v) in
									if distance = ombre then illuminate materiau_sph normale h llight lum_intense 0. lum_r lum_g lum_b
									else
									illuminate materiau_sph normale h llight distance_to_lum lum_intense lum_r lum_g lum_b
								end in
				set_color (rgb r g b);
				plot i j;
			done
		done;
		while continu () do
			();
		done;
		Graphics.close_graph

end;;