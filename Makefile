all:
	ocamlc -c graphics.cma PROJET.mli Projet.ml
	ocamlc -c graphics.cma Projet.cmo KDTREE.mli Kdtree.ml

	ocamlc -w -24 -o raytracer-demo graphics.cma Projet.cmo raytracer-demo.ml
	ocamlc -o sceneLoader graphics.cma Projet.cmo str.cma sceneLoader.ml
	ocamlc -w -24 -o kdtree-demo graphics.cma unix.cma Projet.cmo Kdtree.cmo kdtree-demo.ml
clean:
	rm -f raytracer-demo sceneLoader kdtree-demo  *.cmo *.cmi
