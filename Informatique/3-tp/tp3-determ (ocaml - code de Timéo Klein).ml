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

(* question 2.1 *)
let rec puiss2 x = 1 lsl x

(* question 2.2 *)

let (<<) f g x = f (g x)
let signature l = List.fold_left (fun acc v -> acc + puiss2 v) 0 l;;

(* question 2.3 *)
let id x = x
let list_of_sign s = List.filter_map id (List.init 31 (fun i -> if (s lsr i) land 1 = 1 then Some i else None))

(* question 3 *)
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

let deltapart p c nfa =
  let rec aux tmp = function
    | [] -> tmp
    | hd :: tl -> aux (fusion tmp (successeurs hd c nfa)) tl
  in aux [] p

(* question 4 *)
let dico_trans nfa =
  let rec etend q dico =
    if List.mem_assoc q dico then dico
    else
      let p = list_of_sign q in
      let sA = signature (deltapart p A nfa)
      and sB = signature (deltapart p B nfa) in
      etend sA (etend sB ((q, (sA, sB)) :: dico))
  in etend (signature nfa.nfa_init) []

(* question 5 *)
let delta_det dico (q, c) = match c, List.assoc q dico with (A, (v, _)) | (B, (_, v)) -> v

(* question 6 *)
let rec intersecte lst1 lst2 = match lst1, lst2 with
  | [], _ | _, [] -> false
  | (h1 :: t1), (h2 ::  _) when h1 < h2 -> intersecte t1 lst2
  | (h1 ::  _), (h2 :: t2) when h2 < h1 -> intersecte t2 lst1
  | _ -> true

let rec finals_det dico nfa =
  List.fold_left (fun acc (v, _) -> if intersecte (list_of_sign v) nfa.nfa_finals then v :: acc else acc) [] dico

(* question 7 *)
let determinise nfa =
  let dico = dico_trans nfa in
  {
    dfa_init = signature nfa.nfa_init;
    dfa_finals = finals_det dico nfa;
    dfa_delta = fun (q, c) -> if List.mem_assoc q dico then
                             if c = A then fst (List.assoc q dico)
                             else snd (List.assoc q dico)
                           else 0
  }

(* question 8 *)
let reconnait mot dfa =
  let rec aux cur = function
    | [] -> List.mem cur dfa.dfa_finals
    | hd :: tl -> match dfa.dfa_delta (cur, hd) with
                  | 0 -> false
                  | v -> aux v tl
  in aux dfa.dfa_init mot

(* exemple 1 *)
let afnd1 = {
  nfa_init = [1; 2];
  nfa_finals = [4];
  nfa_trans = [ (1, A, 3); (2, B, 2); (2, B, 3); (3, A, 3); (3, A, 4)]
}
let afd1 = determinise afnd1
let rec_afd1 = reconnait [B; B] afd1

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
