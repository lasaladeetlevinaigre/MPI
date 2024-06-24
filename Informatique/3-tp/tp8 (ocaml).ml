(* types *)
exception Error

type binop = And | Or | Imp

type fmla =
  | False
  | True
  | Var of int
  | Not of fmla
  | Bin of binop * fmla * fmla

type token = 
  | TRUE 
  | FALSE 
  | VAR of int 
  | NOT 
  | LPAR 
  | RPAR 
  | OR 
  | AND 
  | IMP 
  | EOF

(* analyse lexicale *)
let lexer s =
  let n = String.length s in
  let rec q_0 i =
    if i = n 
    then EOF, i
    else match s.[i] with
      | '('   -> LPAR, i 
      | ')'   -> RPAR,  i 
      | '~'   -> NOT, i
      | 'V'   -> TRUE, i  
      | 'F'   -> FALSE, i
      | 'A'   -> AND, i
      | 'O'   -> OR, i
      | 'I'   -> IMP, i
      | 'x'   -> q_var (i+1) (i+1)
      | ' '   -> q_0 (i+1)
      | _     -> raise Error
  and q_var i j =
    if j < n && '0' <= s.[j] && s.[j] <= '9'
    then q_var i (j + 1)
    else
      if i = j
      then raise Error
      else VAR (int_of_string (String.sub s i (j-i))), j-1
  in
  let rec make_lst acc i = match q_0 i with
    | EOF, _    -> List.rev (EOF :: acc)
    | token, i  -> make_lst (token :: acc) (i+1)
  in 
  make_lst [] 0

(* analyse syntaxique *)
let rec parseS l = match parseF l with
  | f, [ EOF ]  -> f
  | _           -> raise Error

and parseF l = match l with 
  | TRUE  :: l  -> True, l
  | FALSE :: l  -> False, l
  | VAR x :: l  -> Var x, l
  | NOT   :: l  -> let f, l = parseF l in Not f, l
  | LPAR  :: l  -> (
      match parseB l with
        | f, RPAR :: l -> f, l
        | _            -> raise Error
    )
  | _           -> raise Error

and parseB l =
  let f1, l = parseF l in
  let op, l = parseO l in
  let f2, l = parseF l in
  Bin(op, f1, f2), l

and parseO l = match l with
  | AND :: l  -> And, l
  | OR  :: l  -> Or,  l
  | IMP :: l  -> Imp, l
  | _         -> raise Error

let parser lst = parseS lst;;


let f_str = "( (x1 A x2) I (~ x3 O x4) )";;
let lst_token = lexer f_str;;
let fml = parser lst_token;;