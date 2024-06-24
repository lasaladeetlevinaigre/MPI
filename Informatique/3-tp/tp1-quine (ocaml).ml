type binop = And | Or | Imp

type fmla =
  | True
  | False
  | Var of int (* dans 1..n *)
  | Not of fmla
  | Bin of binop * fmla * fmla

type valuation = bool array

(* question 1 *)
let rec eval v f = match f with
  | True  -> true
  | False -> false
  | Var i -> assert (1 <= i && i < Array.length v); v.(i)
  | Not f -> not (eval v f)
  | Bin (And, f1, f2) -> eval v f1 && eval v f2
  | Bin (Or,  f1, f2) -> eval v f1 || eval v f2
  | Bin (Imp, f1, f2) -> not (eval v f1) || eval v f2

(* question 2 *)
let varmax f =
  let rec aux m = function
    | True | False    -> m
    | Var i           -> max i m
    | Not f           -> aux m f
    | Bin (_, f1, f2) -> aux (aux m f1) f2 
  in
  aux 0 f

(* question 3 *)
let rec subst i ps f = match f with
  | Var j when i = j -> ps
  | Var _            -> f
  | True | False     -> f
  | Not f            -> Not (subst i ps f)
  | Bin (op, f1, f2) -> Bin (op, subst i ps f1, subst i ps f2)


(* question 4 *)
let f_true = True
let f_false = False
let f_var i = Var i
let f_not f = Not f
let f_or f1 f2 = Bin (Or, f1, f2)
let f_and f1  f2 = Bin (And, f1, f2)
let f_implies f1  f2 = Bin (Imp, f1, f2)
let f_xor f1 f2 = f_or (f_and f1 (Not f2)) (f_and (Not f1) f2)
let f_equiv f1 f2 = f_and (f_implies f1 f2) (f_implies f2 f1)

let smart_not f = match f with
  | True  -> False
  | False -> True
  | _     -> Not f

let smart_and f1 f2 = match f1, f2 with
  | False, _ | _, False -> False
  | True,  f | f, True  -> f
  | _, _                -> Bin (And, f1, f2)

let smart_or f1 f2 = match f1, f2 with
  | True,  _ | _, True  -> True
  | False, f | f, False -> f
  | _, _                -> Bin (Or, f1, f2)

let smart_imp f1 f2 = match f1, f2 with
  | False, _ | _, True  -> True
  | True,  f            -> f
  | f, False            -> smart_not f
  | _, _                -> Bin (Imp, f1, f2)

(* question 5 *)
let rec simplify f = match f with
  | Var _ | True | False -> f
  | Not f             -> smart_not (simplify f)
  | Bin (And, f1, f2) -> smart_and (simplify f1) (simplify f2)
  | Bin (Or,  f1, f2) -> smart_or  (simplify f1) (simplify f2)
  | Bin (Imp, f1, f2) -> smart_imp (simplify f1) (simplify f2)

(* question 6 *)
let rec quine_sat f = match simplify f with
  | True  -> true
  | False -> false
  | f     -> 
      let v = varmax f in
      quine_sat (subst v True f) || quine_sat (subst v False f)


(* tests *)
let f1 = Bin (And, Var 1, Var 2)
let val1 = [| true; true; false |]
let eval1 = eval val1 f1
let varmax1 = varmax f1

let ps = Bin(Or, Var 3, Var 1)
let subst1 = subst 1 ps f1
let subst2 = subst 2 ps f1

let subst3 = subst 1 True f1
let smplf3 = simplify subst3
let subst4 = subst 1 False f1
let smplf4 = simplify subst4

let quine1 = quine_sat f1