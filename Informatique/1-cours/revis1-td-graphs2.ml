type graph = bool array array;;
type label = int array;;

(* transformation d'une liste d'adjacence *)
(* en matrice d'adjacence *)
let tab2mat tab =
	let n = Array.length tab in
	let mat = Array.make_matrix n n false in
	for i = 0 to n-1 do
		List.iter (fun j -> mat.(i).(j) <- true) tab.(i) (* tab.(i) est une liste *)
	done;
	mat;;

(* affichage sympa des tableaux de booléens *)
(* affichage d'un booléen sous la forme 0/1 *)
let disp_bool b =
	if b 
	then Printf.printf "%d\t" 1
	else Printf.printf "%d\t" 0;;
(* affichage d'une matrice de booléens sous la forme 0/1 *)
(* version 1 *)
let disp t =
	let n = Array.length t in
	for i = 0 to n-1 do
		for j = 0 to n-1 do
			disp_bool t.(i).(j)
		done;
		Printf.printf "\n";
	done;;
(* version 2 *)
let disp t =
	let n = Array.length t in
	for i = 0 to n-1 do
		Array.iter (fun x -> disp_bool x) t.(i);
		Printf.printf "\n";
	done;;
(* version 3 *)
let disp t =
	let n = Array.length t in
	Array.iter 
		(fun line -> Array.iter (fun x -> disp_bool x) line; Printf.printf "\n";)
		t;;

(* ------------------------- *)
(* tests *)

(* question 1.1 *)
let g1 = [|
	[|false; false; true; true; false|];
	[|false; false; false; true; true|];
	[|true; false; false; false; true|];
	[|true; true; false; false; false|];
	[|false; true; true; false; false|];
|];;

let tab_g1 = [|
	[2; 3];
	[3; 4];
	[4; 0];
	[0; 1];
	[1; 2]
|];;

let g1 = tab2mat tab_g1;;

let tab_g2 = [|
	[4; 5; 6];
	[6; 7; 8];
	[5; 8; 9];
	[4; 7; 9];
	[0; 3; 8];
	[0; 2; 7];
	[0; 1; 9];
	[1; 3; 5];
	[1; 2; 4];
	[2; 3; 6]
|];;

let g2 = tab2mat tab_g2;;

(* question 1.2 *)
let lbl1 = [|0; 0; 1; 1; 0|];;
let lbl1 = [|0; 1; 2; 3; 4|];;

(* question 1.4 *)
let is_col (g : graph) (lbl : label) : bool =
	let n = Array.length g in
	let color = ref true in
	for i = 0 to n-2 do
		for j = i+1 to n-1 do
			if g.(i).(j) && lbl.(i) = lbl.(j) 
			then color := false;
		done;
	done;
	!color;;

(* question 1.4 bis *)
(* recherche de toutes les coloration possibles !!! *)
let tab_g3 = [| 
	[1; 2]; 
	[2; 0]; 
	[0; 1] 
|];;
let g3 = tab2mat tab_g3;;
disp g3;;

(* ------------------------- *)
let dfs g src = 
	let n = Array.length g in
	let visited = Array.make n false in
	let rec aux i =
		if not visited.(i) then (
			visited.(i) <- true;
			Printf.printf "%d " i;
			for j = 0 to n-1 do
				if g.(i).(j) then aux j
			done;
		)
	in aux src;
	visited;;

let succ g u =
	let n = Array.length g in
	let lst = ref [] in
	for i = 0 to n-1 do
		if g.(u).(i) 
		then lst := i :: !lst
	done;
	!lst;;

let succ g u =
	let n = Array.length g in
	let rec aux i = match i with
		| j when j=n -> []
		| _ -> let lst = aux (i+1) in 
					 if g.(u).(i) then i :: lst else lst
	in aux 0;;

let succ g u =
	let n = Array.length g in
	let rec aux = function
		| i when i = n -> []
		| i -> let lst = aux (i+1) in 
					 if g.(u).(i) then i :: lst else lst
	in aux 0;;

let dfs1 g src =
	let n = Array.length g in
	let visited = Array.make n false in
	let rec aux i =
		if not visited.(i) then (
			visited.(i) <- true;
			List.iter (fun u -> aux u) (succ g i)
		)
	in aux src;
	visited;;

let dfs2 g src =
	let n = Array.length g in
	let visited = Array.make n false in
	let pred = Array.make n (-1) in
	let rec aux i =
		if not visited.(i) then (
			visited.(i) <- true;
			List.iter 
				(fun u -> if not visited.(u) then pred.(u) <- i; 
									aux u)
				(succ g i)
		)
	in aux src;
	pred;;




