(* quelques fonctions d'affichage *)
let rec print_lst = function
  | [] -> ()
  | head :: tail -> 
      Printf.printf "%d\n" head; 
      print_lst tail
;;

let print_answers question answers =
  Printf.printf "\n* Question %d\n" question;
  print_lst answers
;;

(* question 1 *)
let u_0 = 1;;

let fill_u n =
  let u = Array.make n u_0 in
  for i = 1 to n - 1 do
    u.(i) <- 15731 * u.(i-1) mod 32003
  done;
  u
;;

let u = fill_u 32003;;
let () = print_answers 1 [
  u.(1000); 
  u.(10000); 
  u.(30000)
];;

(* question 2 *)
(* type graph = bool array array *)

let create_graph m n =
  let g = Array.make_matrix n n false in
  for i = 0 to n-2 do
    for j = i+1 to n-1 do
      if u.(1 + i + j*(j-1)/ 2) mod m = 0 then begin
        g.(i).(j) <- true;
        g.(j).(i) <- true
      end
    done
  done;
  g
;;

let nb_edges g =
  let n = Array.length g in
  let cnt = ref 0 in
  for i = 0 to n - 2 do
    for j = i + 1 to n - 1 do
      if g.(i).(j) then incr cnt;
    done
  done;
  !cnt

let degree g u =
  let cnt = ref 0 in
  for i = 0 to (Array.length g) - 1 do
    if g.(u).(i) then incr cnt
  done;
  !cnt
;;

let max_degree g =
  let d = ref 0 in
  for i = 0 to (Array.length g) - 1 do
    if (degree g i) > !d then d := degree g i
  done;
  !d
;;

let () = print_answers 2 [
  nb_edges g_5_10;
  nb_edges g_10_50;
  nb_edges g_50_250
];;

let g_5_10 = create_graph 5 10;;
let g_10_50 = create_graph 10 50;;
let g_50_250 = create_graph 50 250;;

let () = print_answers 2 [
  max_degree g_5_10;
  max_degree g_10_50;
  max_degree g_50_250
];;

(* question 3 *)

let smaller g u v =
  let du = degree g u in
  let dv = degree g v in
  (dv < du) || ( (du = dv) && (u <= v) )
;;

let min_idx v comp =
  let idx = ref 0 in
  for i = 0 to Array.length v - 1 do
    if comp i (!idx) then idx := i
  done;
  !idx
;;

let smaller_in_graph g =
  let smaller_comp = smaller g in
  min_idx g smaller_comp
;;

let greater_in_graph g =
  let smaller_comp u v = smaller g v u in
  min_idx g smaller_comp
;;

let () = print_answers 3 [
  smaller_in_graph g_5_10;
  smaller_in_graph g_10_50;
  smaller_in_graph g_50_250
]
;;

let () = print_answers 3 [
  greater_in_graph g_5_10;
  greater_in_graph g_10_50;
  greater_in_graph g_50_250
]
;;

(* question 4 *)
type partie = bool array

let print_game v =
  let n = Array.length v in
  print_string "{ ";
  for i = 0 to n - 1 do
    if v.(i) then begin
      print_int i;
      print_string "; "
    end
  done;
  print_endline "}"

let is_cover g v =
  let n = Array.length g in
  let res = ref true in
  let i = ref 0 in
  let j = ref 0 in
  while (!res) && (!i < n - 1) do
    j := !i + 1;
    while (!res) && (!j < n) do
      if g.(!i).(!j) && (not (v.(!i) || v.(!j))) then
        res := false;
      incr j
    done;
    incr i
  done;
  !res

(* 
Essayer de compléter la partie `v`, 
supposée remplie jusqu'à `i - 1`, 
avec encore `k` élements de manière à satisfaire `test` 
*)
let rec try_complete v k i test =
  if k = 0 then begin
    (* print_game v; *)
    test v;
  end
  (* Plus de place pour de nouveaux éléments *)
  else if i = Array.length v then
    false
  else begin
    (* On essaye d'abord avec l'élément `i` *)
    v.(i) <- true;
    if try_complete v (k - 1) (i + 1) test then
      true
    else begin
      (* Essayons donc sans *)
      v.(i) <- false;
      try_complete v k (i + 1) test
    end
  end

(* Renvoie la première partie de [|0; n - 1|], dans l'ordre
lexicographique, à satisfaire le prédicat `test` *)
let try_game_lexico n test =
  let v = Array.make n false in
  let trouve = ref false in
  let nb_elements = ref 0 in
  while (not !trouve) && (!nb_elements <= n) do
    if try_complete v (!nb_elements) 0 test then
      trouve := true;
    incr nb_elements
  done;
  v

let exact_min_cover g =
  let n = Array.length g in
  let test = is_cover g in
  try_game_lexico n test

let g_6_16 = create_graph 6 16;;
let g_7_20 = create_graph 7 20;;
let () = print_endline "* Question 4";;
let () = print_game (exact_min_cover g_5_10);;
let () = print_game (exact_min_cover g_6_16);;
let () = print_game (exact_min_cover g_7_20);;

let () = print_newline ();;

(** Question 5 *)

(* On fait un parcours de graphe classique (ici en profondeur) avec un
compteur que l'on incrémente chaque fois que l'on change de
composante. Le tableau memorisant la composante d'un sommet nous sert
également de marque pour le parcours. *)
let components g =
  let n = Array.length g in
  let comp = Array.make n 0 in
  let id_comp = ref 0 in
  (* Parcours en profondeur à partir d'un sommet *)
  let rec explorer s =
    for i = 0 to n - 1 do
      if g.(s).(i) && comp.(i) = 0 then begin
        comp.(i) <- !id_comp;
        explorer i
      end
    done
  in
  (* Lancer le parcours à partir de chaque sommet *)
  for i = 0 to n - 1 do
    if comp.(i) = 0 then begin
      incr id_comp;
      comp.(i) <- !id_comp;
      explorer i;
    end
  done;
  comp

let max_vect v =
  let max = ref (-42) in
  for i = 0 to Array.length v - 1 do
    if v.(i) > !max then max := v.(i)
  done;
  !max

let components_to_counts v =
  let n = Array.length v in
  let counts = Array.make n 0 in
  for i = 0 to n - 1 do
    counts.(v.(i)) <- counts.(v.(i)) + 1
  done;
  counts

let g_50_50 = create_graph 50 50
let g_125_100 = create_graph 125 100
let g_50_250 = create_graph 50 250
let g_360_250 = create_graph 360 250

let () = print_answers 5 [
  max_vect (components g_5_10);
  max_vect (components g_6_16);
  max_vect (components g_7_20);
  max_vect (components g_10_50);
  max_vect (components g_50_50);
  max_vect (components g_125_100);
  max_vect (components g_50_250);
  max_vect (components g_360_250)
]

let () = print_answers 5 [
  max_vect (components_to_counts (components g_5_10));
  max_vect (components_to_counts (components g_6_16));
  max_vect (components_to_counts (components g_7_20));
  max_vect (components_to_counts (components g_10_50));
  max_vect (components_to_counts (components g_50_50));
  max_vect (components_to_counts (components g_125_100));
  max_vect (components_to_counts (components g_50_250));
  max_vect (components_to_counts (components g_360_250))
]

(** Question 6 *)

let count_occurrence_vect vect elem =
  let count = ref 0 in
  for i = 0 to Array.length vect - 1 do
    if vect.(i) = elem then incr count
  done;
  !count

let subgraph g components k =
  let n = Array.length g in
  let n' = count_occurrence_vect components k in
  let g' = Array.make_matrix n' n' false in
  (* name_in_g.(j) is the original name of node `j` in g *)
  let name_in_g = Array.make n' (-1) in
  let index = ref 0 in
  for i = 0 to n - 1 do
    if components.(i) = k then begin
      name_in_g.(!index) <- i;
      incr index
    end
  done;
  (* Now we can add edges to subgraph *)
  for i' = 0 to n' - 1 do
    for j' = 0 to n' - 1 do
      let i = name_in_g.(i') in
      let j = name_in_g.(j') in
      if g.(i).(j) then g'.(i').(j') <- true
    done
  done;
  g'

let game_size v =
  let taille = ref 0 in
  for i = 0 to Array.length v - 1 do
    if v.(i) then incr taille
  done;
  !taille

let opt_min_cover g =
  let total = ref 0 in
  let components = components g in
  for k = 1 to max_vect components do
    let g' = subgraph g components k in
    total := !total + game_size (exact_min_cover g')
  done;
  !total

let () = print_answers 6 [
  opt_min_cover g_50_50;
  opt_min_cover g_125_100;
  opt_min_cover g_360_250;
]

(** Question 7 *)

let has_edge g i =
  let rec aux j =
    (j < Array.length g) && (g.(i).(j) || aux (j + 1))
  in
  aux 0

let copy_graph g =
  let n = (Array.length g) in
  let g' = Array.make_matrix n n false in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      g'.(i).(j) <- g.(i).(j)
    done
  done;
  g'

let approx_cover g =
  let g = copy_graph g in
  let couverture = Array.make (Array.length g) false in
  let i = ref 0 in
  while !i < Array.length g do
    if has_edge g !i then begin
      couverture.(!i) <- true;
      for j = 0 to Array.length g - 1 do
        g.(!i).(j) <- false;
        g.(j).(!i) <- false;
      done;
    end;
    incr i;
  done;
  couverture

let () = print_answers 7 [
  game_size (approx_cover g_5_10);
  game_size (approx_cover g_6_16);
  game_size (approx_cover g_7_20);
  game_size (approx_cover g_10_50);
  game_size (approx_cover g_50_50);
  game_size (approx_cover g_125_100);
  game_size (approx_cover g_50_250);
  game_size (approx_cover g_360_250);
]

(** Question 8 *)

let approx_cover_opt g =
  let g = copy_graph g in
  let couverture = Array.make (Array.length g) false in
  let finish = ref false in
  while not !finish do
    let v = smaller_in_graph g in
    if has_edge g v then begin
      couverture.(v) <- true;
      for j = 0 to Array.length g - 1 do
        g.(v).(j) <- false;
        g.(j).(v) <- false;
      done;
    end else
      finish := true
  done;
  couverture

let () = print_answers 8 [
  game_size (approx_cover_opt g_5_10);
  game_size (approx_cover_opt g_6_16);
  game_size (approx_cover_opt g_7_20);
  game_size (approx_cover_opt g_10_50);
  game_size (approx_cover_opt g_50_50);
  game_size (approx_cover_opt g_125_100);
  game_size (approx_cover_opt g_50_250);
  game_size (approx_cover_opt g_360_250);
]

(** Question 9 *)

let covered_neighbors g couverture s =
  let i = ref 0 in
  let res = ref true in
  while (!res) && (!i < Array.length g) do
    if g.(s).(!i) && (not couverture.(!i)) then res := false;
    incr i
  done;
  !res

let approx_cover_opt_min g =
  let g' = copy_graph g in
  let couverture = Array.make (Array.length g') false in
  let added_vertices = ref [] in
  let finish = ref false in
  while not !finish do
    let v = smaller_in_graph g' in
    if has_edge g' v then begin
      couverture.(v) <- true;
      added_vertices := v :: !added_vertices;
      for j = 0 to Array.length g' - 1 do
        g'.(v).(j) <- false;
        g'.(j).(v) <- false;
      done;
    end else
      finish := true
  done;
  let rec remove_redundant = function
    | [] -> ()
    | head :: tail ->
       if covered_neighbors g couverture head then
         couverture.(head) <- false;
       remove_redundant tail
  in remove_redundant (!added_vertices);
  couverture

let () = print_answers 9 [
  game_size (approx_cover_opt_min g_5_10);
  game_size (approx_cover_opt_min g_6_16);
  game_size (approx_cover_opt_min g_7_20);
  game_size (approx_cover_opt_min g_10_50);
  game_size (approx_cover_opt_min g_50_50);
  game_size (approx_cover_opt_min g_125_100);
  game_size (approx_cover_opt_min g_50_250);
  game_size (approx_cover_opt_min g_360_250);
]