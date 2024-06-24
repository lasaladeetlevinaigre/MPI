open List;;

type alphabet = A | B;;

type afd ={
  init : int; (* états initiaux *)
  finals : int list; (* états finals *)
  delta : int * alphabet -> int; (* transitions *)
};;

(* Exemple *)
let a1 = {
  init = 1;
  finals = [3];
  delta = function
    |(1,A) -> 2
    |(1,B) -> 1
    |(2, A) -> 1
    |(2, B) -> 3
    |(3, A) -> 1
    | _ -> 0
};;

let a2 = {
  init = 1;
  finals = [2; 4; 5; 6];
  delta = function
      (1, A) -> 2
    | (1, B) -> 4
    | (2, A) -> 3
    | (2, B) -> 5
    | (3, A) -> 2
    | (3, B) -> 6
    | (4, A) -> 3
    | (4, B) -> 1
    | (5, _) -> 6
    | (6, _) -> 5
    | _ -> 0
};;

(* Déterminisation d'un automate *)
type afnd = {
  nd_init : int list; (* états initiaux *)
  nd_finals : int list; (* états finals *)
  nd_trans : (int * alphabet * int) list; (* transitions *)
};;

(* Exemple *)
let afnd1 = {
  nd_init = [1; 2];
  nd_finals = [4];
  nd_trans = [ (1, A, 3); (2, B, 2); (2, B, 3); (3, A, 3); (3, A, 4)]
};;

(* question 2 *)
let rec puiss2 x =
  match x with
    | 0 -> 1
    | _ -> 2 * (puiss2 (x - 1));;

puiss2 3;;

let rec signature l =
  match l with
    | [] -> 0
    | x :: q -> (puiss2 (x - 1)) + signature q;;

signature [8; 3; 4; 1];;

let  list_of_sign s =
  let rec aux s i = 
    match s with
      | 0 -> []
      | 1 -> [i]
      | _ -> let r = s mod 2 and q = s / 2 in
	  if r = 0 then aux q (i + 1)
	  else i :: (aux q (i + 1))
  in aux s 1;;

list_of_sign 141;;

(* question 3 *)
(* fonctions utiles *)
(* ajoute un élément à une liste triée sans doublons *) 
let rec ajoute i l = 
  match l with
    | [] -> [i]
    | x :: q when i = x -> l
    | x :: q when i < x -> i :: l
    | x :: q -> x :: (ajoute i q);;

(* fusionne deux listes triées sans doublons *)
let rec fusion l1 l2 = 
  match (l1, l2) with
    | [], [] -> []
    | _, [] -> l1
    | [], _ -> l2
    | x1 :: q1, x2 :: q2 when x1 = x2 -> x1 :: (fusion q1 q2)
    | x1 :: q1, x2 :: q2 when x1 < x2 -> x1 :: (fusion q1 l2)
    | x1 :: q1, x2 :: q2 -> x2 :: (fusion l1 q2);;

(* renvoie la liste des états q' de l'AFND  tels que (q, l, q') in T*)
let successeurs q c afnd =  
  let rec aux l = 
    match l with
      | [] -> []
      | (i, a, j) :: ql  when i = q && a = c -> ajoute j (aux ql)
      | (i, a, j) :: ql -> aux ql
  in aux afnd.nd_trans;;

(* fonction demandée *)
let rec deltapart p c afnd =
    match p with
      | [] -> []
      | q :: ql -> fusion (successeurs q c afnd) (deltapart ql c afnd);;

deltapart [1; 2] B afnd1;;

(* question 4 *)
let dico_trans afnd = 
  let rec etend n dico  =
    if mem_assoc n dico then dico 
    else
      let p = list_of_sign n in 
      let n1 = signature (deltapart p A afnd)
      and n2 = signature (deltapart p B afnd)
      in
      let dico1 = (n, (n1, n2)) :: dico in 
      let dico2 = etend n1 dico1 in 
	etend n2 dico2
  in etend (signature afnd.nd_init) [];;
	  
let dico = dico_trans afnd1;;

(* question 5 *)
let delta_det dico (q, c) =
  let (qa, qb) = assoc q dico in
    match c with
      | A -> qa
      | B -> qb;;
    
delta_det dico (3, B);;

(* question 6 *)
(* fonction utile *)
let rec intersecte l1 l2 =
  match (l1, l2) with
    | [], [] -> false
    | [], _ -> false
    | _, [] -> false
    | x1 :: q1, x2 :: q2 when x1 = x2 -> true
    | x1 :: q1, x2 :: q2 when x1 < x2 -> intersecte q1 l2
    | x1 :: q1, x2 :: q2 -> intersecte l1 q2;;

(* fonction demandée *)
let rec finals_det dico afnd =
  match dico with
    | [] -> []
    | (q, _) :: dq when (intersecte (list_of_sign q)
			   afnd.nd_finals) -> q :: (finals_det dq afnd)
    | (q, _) :: dq -> (finals_det dq afnd);;

finals_det dico afnd1;;

(* question 7 *)
let determinise afnd =
  let dico = dico_trans afnd in
  let d_init = signature afnd.nd_init in
  let d_delta = delta_det dico in
  let d_finals = finals_det dico afnd in
    {init = d_init;
     finals = d_finals;
     delta = d_delta};;


let afd1 = determinise afnd1;;

(* question 8 *)
let rec deltastar q mot a =
  match mot with
    | [] -> q
    | [l] -> a.delta (q, l)
    | l :: m1 -> let q1 = a.delta (q, l) in
  deltastar q1 m1 a;;
  
let reconnait mot a =
  let qf = deltastar a.init mot a in
    mem qf a.finals;;

reconnait [A; B] a1;;

reconnait [B; B; A; A] afd1;;

