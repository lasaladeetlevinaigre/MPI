(* type de graphe adopté *)
(* 
adj : matrice d'adjacence pondérée par le nombre d'arêtes, dans laquelle les sommets fusionnés ont été éliminés (ligne et colonne mises à zéro). Pour chaque classe d'équivalence, seul le représentant de la classe est conservé.

degrees : tableau des degrés de chaque élément de Vk, valant zéro pour les sommets qui ne sont pas représentants de leur classe d'équivalence

forest : tableau représentant la partition Vk; forest.(i) est égal au père de i dans la forête UnionFind ou à moins la taille de l'arbre si i est une racine

nb_edges : nombre courant d'arêtes du graphe
 *)

 type multigraph = {
	adj : int array array; 
	degrees : int array; 
	forest : int array; 
	mutable nb_edges : int
};;

(* fonctions d'affichages *)
let disp_arr a =
	let n = Array.length a in 
	for i = 0 to n-1 do
		Printf.printf "%d\t" a.(i)
	done
;;

let disp_mat m =
	let n = Array.length m in	
	for i = 0 to n-1 do
		disp_arr m.(i);
		Printf.printf "\n";
	done
;;

let disp_graph g =
	Printf.printf "\n==========\n";
	Printf.printf "- matrice d'adjacence\n";
	disp_mat g.adj;
	Printf.printf "- table des fusions (union-find)\n";
	disp_arr g.forest;
	Printf.printf "\n- table des degrés\n";
	disp_arr g.degrees
;;

(* choix aléatoire d'une arête *)
let rnd_edge g =
	let x = ref (Random.int g.nb_edges) in 
	let u = ref 0 in
	while g.degrees.(!u) <= !x do
		x := !x - g.degrees.(!u);
		incr u; 
	done;
	let v = ref 0 in
	while g.adj.(!u).(!v) <= !x do
		x := !x - g.adj.(!u).(!v);
		incr v
	done; 
	(!u, !v)
;;

(* recherche dans forest (union-find) *)
let rec find forest i =
	if forest.(i) < 0 then i 
	else begin
		let j = find forest forest.(i) in 
		forest.(i) <- j;
		j
	end
;;

(* union dans forest (union-find) *)
let merge forest i j =
	let ri = find forest i in 
	let rj = find forest j in 
	if ri <> rj then begin
		let ti = - forest.(ri) in 
		let tj = - forest.(rj) in 
		if ti < tj then begin
			forest.(ri) <- rj;
			forest.(rj) <- - ti - tj 
		end 
		else begin
			forest.(rj) <- ri;
			forest.(ri) <- - ti - tj 
			end
	end
;;

(* contraction d'une arête *)
let contract g u v =
	merge g.forest u v;
	let n = Array.length g.degrees in
	(* mise à jour de la table des degrés *)
	g.degrees.(v) <- g.degrees.(v) + g.degrees.(u) - 2 * g.adj.(u).(v); 
	g.degrees.(u) <- 0;
	(* mise à jour du nb d'arêtes *)
	g.nb_edges <- g.nb_edges - 2 * g.adj.(u).(v);
	(* mise à jour de la matrice d'adjacence *)
	g.adj.(u).(v) <- 0;
	g.adj.(v).(u) <- 0;
	for i = 0 to n - 1 do
		g.adj.(i).(v) <- g.adj.(i).(u) + g.adj.(i).(v);
		g.adj.(i).(u) <- 0;
		g.adj.(v).(i) <- g.adj.(u).(i) + g.adj.(v).(i);
		g.adj.(u).(i) <- 0
	done
;;

(* initialisation d'un multigraphe *)
let make_graph g =
	let n = Array.length g in
	let adj = Array.init n (fun i -> Array.copy g.(i)) in 
	let degrees = Array.make n 0 in
	let forest = Array.make n (-1) in
	let nb_edges = ref 0 in
	for u = 0 to n - 1 do
		for v = 0 to n - 1 do
			degrees.(u) <- degrees.(u) + g.(u).(v); 
			nb_edges := !nb_edges + g.(u).(v);
		done 
	done;
	{adj; degrees; nb_edges = !nb_edges; forest}
;;

(* algorithme de karger *)
let karger g_base =
	let g = make_graph g_base in
	(* let _ = disp_graph g in *)
	let n = Array.length g_base in
	for i = 0 to n - 3 do
		let u, v = rnd_edge g in
    contract g u v;
    (* disp_graph g; *)
  done; 
  g
;;

(* exemple *)
let g_base = [|
	[|0; 1; 1; 2; 0|];
	[|1; 0; 1; 1; 0|];
	[|1; 1; 0; 1; 1|];
	[|2; 1; 1; 0; 2|];
	[|0; 0; 1; 2; 0|];
|];;
karger g_base;;
