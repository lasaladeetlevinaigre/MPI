(* code à compléter *)
(* type alphabet *)
type alphabet = A | B

(* type automate fini non déterministe *)
type nfa = {
  nfa_init : int list;                     (* états initiaux *)
  nfa_finals : int list;                   (* états acceptants *)
  nfa_trans : (int * alphabet * int) list; (* transitions *)
}

(* type automate fini déterministe *)
type dfa = {
  dfa_init : int;                             (* état initial *)
  dfa_finals : int list;                      (* états acceptants *)
  dfa_delta : int * alphabet -> int;          (* transitions *)
}

(* question 1 *)
let rec puiss2 x =
  
(* question 2 *)
let rec signature l =

(* question 3 *)
let  list_of_sign s =

(* question 4 *)

(* question 5 *)

(* ajoute un élément à une liste triée sans doublons *)
let rec ajoute i lst = 
  match lst with
    | [] -> [i]
    | x :: q when i = x -> lst
    | x :: q when i < x -> i :: lst
    | x :: q -> x :: (ajoute i q)

(* fusionne deux listes triées sans doublons *)
let rec fusion lst1 lst2 = 
  match (lst1, lst2) with
    | [], [] -> []
    | _, [] -> lst1
    | [], _ -> lst2
    | x1 :: q1, x2 :: q2 when x1 = x2 -> x1 :: (fusion q1 q2)
    | x1 :: q1, x2 :: q2 when x1 < x2 -> x1 :: (fusion q1 lst2)
    | x1 :: q1, x2 :: q2 -> x2 :: (fusion lst1 q2)

(* renvoie la liste des états q' de l'AFND tels que (q, l, q') in F *)
let successeurs q c nfa =
  let rec aux lst = 
    match lst with
      | [] -> []
      | (i, a, j) :: qlst when i = q && a = c -> ajoute j (aux qlst)
      | (i, a, j) :: qlst -> aux qlst
  in aux nfa.nfa_trans

let rec deltapart p c nfa =

(* question 6 *)
let dico_trans nfa = 

(* question 7 *)
let delta_det dico (q, c) =

(* question 8 *)
let rec intersecte lst1 lst2 =

let rec finals_det dico nfa = 

(* question 9 *)
let determinise nfa =

(* question 10 *)
let rec deltastar q mot dfa = 
  
let reconnait mot dfa =


(* exemple 1 *)
let afnd1 = {
  nfa_init = [1; 2];
  nfa_finals = [4];
  nfa_trans = [ (1, A, 3); (2, B, 2); (2, B, 3); (3, A, 3); (3, A, 4)]
}
let afd1 = determinise afnd1
let rec_afd1 = reconnait [B; B; A; A] afd1

(* exemple 2 *)
let afnd2 = {
  nfa_init = [1; 2; 3];
  nfa_finals = [4; 3];
  nfa_trans = [ 
    (1, A, 3); (2, B, 2); (2, B, 3); (3, A, 3); (3, A, 4);
    (3, A, 1); (3, B, 4); (4, A, 2); (4, A, 5); (5, A, 1);
    (5, A, 2); (5, A, 3); (5, B, 6); (6, A, 3); (6, B, 1)]
}
let afd2 = determinise afnd2
let rec_afd2 = reconnait [B; B; A; A] afd2
