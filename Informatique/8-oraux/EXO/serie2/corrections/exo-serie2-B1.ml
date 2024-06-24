(* `V i` correspond à la variable propositionnelle d'indice `i` et `NV
   i` à la négation de la variable propositionnelle d'indice `i` *)
type litteral =
  | V of int
  | NV of int

type clause = litteral array
type formule = clause list
type valuation = bool array

(* indice : litteral -> int *)
let indice = function V i | NV i -> i

let initialise (n : int) : valuation =
  Array.init n (fun _ -> Random.int 2 = 0)

(* Question 1 *)
let evalue (c : clause) (v : valuation) : bool =
  let evalue_litteral (l:litteral) :bool = match l with
    |V i -> v.(i)
    |NV i -> not v.(i)
  in    
  let evaluation_litteraux = Array.map (fun l -> evalue_litteral l) c in
  Array.mem true evaluation_litteraux

let _ = evalue [|V 0; NV 1; V 2|] [|true; true; true|]
let _ = evalue [|V 0; NV 1; V 2|] [|false; true; false|]
let _ = evalue [||] [|true; true; false|]

(* Question 4, après corrections *)
(* Objectif : prendre en entrée une formule `f` sur `n` variables, un
   nombre d'essais maximum `k` supposé strictement positif et s'évaluer
   en une option sur une valuation qui satisfait `f` si on en trouve
   une ou `None` si on échoue après `k` tentatives *)
let random_sat (f : formule) (n : int) (k : int) : valuation option =
  assert (k >= 1);
  let v = initialise n in
  let rec test (g : formule)  (k : int) : valuation option =
    match g with
    | [] -> Some v
    | c :: cs ->
       if evalue c v then
         test cs k
       else if k < 1 || Array.length c = 0 then
         None
       else begin
         let i = Random.int (Array.length c) in
         v.(indice c.(i)) <- not v.(indice c.(i));
         test f (k - 1)
       end
  in
  test f k

(* Question 3 *)
let _ = random_sat [] 3 10
let _ = random_sat [[||]] 3 10
let _ = random_sat [[|V 0; NV 0|]] 3 10
let _ = random_sat [[|V 0; V 1|]; [|V 2; NV 0; V 1|]] 3 10
let _ = random_sat [[|V 0; V 1|]; [|NV 1|]; [|NV 0|]] 3 10

(* Question 5 *)
(* Fonction qui compte le nombre de clauses satisfaites par une valuation *)
let rec nb_sat (f:formule) (v:valuation) :int = match f with
  |[] -> 0
  |c::q -> if evalue c v then 1 + nb_sat q v else nb_sat q v

let maxsat (f:formule) (n:int) (k:int) :int* valuation =
  let meilleure_val = ref (initialise n) in
  let meilleur_nb = ref (nb_sat f !meilleure_val) in
  for k = 0 to n-2 do
    let nouvelle_val = initialise n in
    let nouveau_nb = nb_sat f nouvelle_val in
    if nouveau_nb > !meilleur_nb then begin
        meilleure_val := nouvelle_val;
        meilleur_nb := nouveau_nb
      end
  done;
  !meilleur_nb, !meilleure_val
    
let _ = maxsat [[|V 0; V 1|]; [|NV 1|]; [|NV 0|]] 3 1
let _ = maxsat [] 3 10
