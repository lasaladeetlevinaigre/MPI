type graph = int list array;;

(* question 1 *)
let g = [|
	[];
	[0; 2];
	[1; 3];
	[4];
	[2];
	[3];
	[4; 7];
	[5; 6];
|];;

(* question 2 *)
let succ g u =
	g.(u);;

succ g 2;;

(* question 3 *)
let size g =
	Array.length g;;

size g;;

let dfs g src =
	let visited = Array.make (size g) false in
	let rec dfs u =
		if not visited.(u) then (
			visited.(u) <- true;
			List.iter dfs	(succ g u);
		) in
	dfs src;
	visited;;	

dfs g 1;;

(* question 4 *)
let mirror g =
	let n = size g in
	let rev_g = Array.make n [] in
	for u = 0 to n-1 do
		List.iter
			(fun v -> rev_g.(v) <- u :: rev_g.(v))
			g.(u)
	done;
	rev_g;;

(* question 5 *)
let post_order g =
  let n = size g in
  let visited = Array.make n false in
  let order = ref [] in
  let rec dfs v =
    if not visited.(v) then (
      visited.(v) <- true;
      List.iter dfs (succ g v);
      order := v :: !order
    ) 
  in
  for v = 0 to n-1 do dfs v done;
  !order;;

post_order g;;

(* question  6 *)
(* Si u et v sont deux sommets d'un graphe orienté, on dit que u et v sont fortement connectés s'il existe un chemin de u à v et un chemin de v à u. Une composante fortement connexe est un sous-ensemble de sommets tous fortement connectés deux à deux, maximal pour l'inclusion. *)

(* question  7 *)
(* Il y a 4 composantes fortement connexes définies par les ensembles : {0}, {1, 2, 3, 4}, {5}, {6, 7}. *)

(* question  8 *)
(* Un tableau décrivant les composantes fortements connexes est : [|0;1;1;1;1;2;3;3|]. *)

(* question  9 *)
let kosaraju_sharir g =
  let n = size g in
  let nc = ref 0 in
  let num = Array.make n 0 in
  let visited = Array.make n false in
  let rec dfs v =
    if not visited.(v) then (
      visited.(v) <- true;
      num.(v) <- !nc;
      List.iter dfs (succ g v)
    )
  in
  (* premier parcours *)
  let trav1 = post_order (mirror g) in
  (* second parcours *)
  List.iter (fun v -> if not visited.(v) then (dfs v; incr nc)) trav1;
  (* résultat *)
  !nc, num;;

(* variable ou négation de variable *)
type litteral = V of int | NV of int;;
type clause = litteral list;;
type fnc = clause list;;

(* question 10 *)
(* impossible dans le cas général ! *)

(* question 11 *)
let rec var_max_clause (c : clause) = match c with
  | [] -> min_int
  | (V x) :: q -> max x (var_max_clause q)
  | (NV x) :: q -> max x (var_max_clause q);;

let rec var_max (phi : fnc) = match phi with
  | [] -> min_int
  | c :: q -> max (var_max_clause c) (var_max q);;

let x0 = V 0;;
let nx0 = NV 0;;
let x1 = V 1;;
let nx1 = NV 1;;
let x2 = V 2;;
let nx2 = NV 2;;

let c1 = [x0; nx1];;
let c2 = [x1; nx2];;
let c3 = [x2; nx0];;

let phi = [c1; c2; c3];;

(* question 12 *)
let sat2graph phi =
  let = var_max phi in
  let g = Array.make (2*(n+1)) [] in
  let add_clause = function
    | [V i]        -> g.(2*i+1) <- (2*i)::g.(2*i+1)
    | [NV i]       -> g.(2*i) <- (2*i+1)::g.(2*i)
    | [V i; V j]   -> g.(2*i+1) <- (2*j)::g.(2*i+1);
                      g.(2*j+1) <- (2*i)::g.(2*j+1)
    | [V i; NV j]  -> g.(2*i+1) <- (2*j+1)::g.(2*i+1);
                      g.(2*j) <- (2*i)::g.(2*j)
    | [NV i; V j]  -> g.(2*i) <- (2*j)::g.(2*i);
                      g.(2*j+1) <- (2*i+1)::g.(2*j+1)
    | [NV i; NV j] -> g.(2*i) <- (2*j+1)::g.(2*i);
                      g.(2*j) <- (2*i+1)::g.(2*j)
  in
  List.iter add_clause phi;
  g;;

sat2graph phi;;
(* question 13 *)
let sat2 phi =
  let g = sat2graph phi in
  let _, scc = kosaraju_sharir g in
  let n = Array.length g  
	and i = ref 0 in
  while !i < n/2 && scc.(2* !i) != scc.(2* !i+1) do incr i done;
  !i = n/2;;

(* 
Désignons par p le nombre de variables propositionnelles et par q le nombre de clauses dans phi.
Le nombre de sommets dans g est n = 2p.
L'appel (sat2graph phi) est en O(q) : on parcourt toutes les clauses pour construire g.
L'appel (kosaraju_sharir g) est en O(n^2) = O(p^2) : se rappeler que le parcours dfs est en O(|V| + |E|).
La boucle while est en O(n) = O(p).
L'appel (sat2 phi) est en O(q + p^2), donc polynomial.
*)
