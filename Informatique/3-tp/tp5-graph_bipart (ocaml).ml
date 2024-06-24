(* fonctions préliminaires *)

type graph = bool array array;;
type label = int array;;

let disp_bool b =
  if b
  then Printf.printf "%d\t" 1
  else Printf.printf "%d\t" 0;;

let disp_tab_bool t =
  Array.iter disp_bool t;
  Printf.printf "\n";;

let disp g =
  Printf.printf "\n";
  Array.iter disp_tab_bool g;;

(* question 1.4 *)
let is_col g lbl =
  let n = Array.length g in
  let p = Array.length lbl in
  let color = ref true in
  if n <> p then color := false
  else 
  for i = 0 to (n-2) do
    for j = (i+1) to (n-1) do
      if g.(i).(j) && lbl.(i) = lbl.(j) 
      then color := false 
    done
  done;
  !color;;

(* question 2.2 *)
let two_col g = 
  let n = Array.length g in
  let lbl = Array.make n (-1) 
  in
  let rec browse i k = 
    lbl.(i) <- k;
    for j = 0 to n-1 do 
      if g.(i).(j) && lbl.(j) = -1 
      then browse j (1-k) 
    done
  in
  for i = 0 to n-1 do 
    if lbl.(i) = -1 
    then browse i 0
  done;
  lbl;; 

(* question 3.2 *)
let min_col g lbl s =
  let n = Array.length g in
  let color = Array.make n false 
  in
  for i = 0 to (n-1) do
    if g.(s).(i) && lbl.(i) <> -1 
    then color.(lbl.(i)) <- true 
  done;
  let c = ref 0 in
  while color.(!c) do 
    c := !c + 1 
  done;
  !c;;

(* question 3.3 *)
let greedy g num =
  let n = Array.length g in
  let color = Array.make n (-1) in
  for i = 0 to (n-1) do
    let k = num.(i) in
    color.(k) <- min_col g color k done;
  color;;

let deg g =
  let n = Array.length g in
  let deg = Array.make n 0 
  in
  for i = 0 to (n-1) do 
    for j = 0 to (n-1) do
      if g.(i).(j) 
      then deg.(i) <- deg.(i) + 1 
    done 
  done;
  deg;;

(* question 4.1 *)
let swap tab i j =
  let temp = tab.(i) in
  tab.(i) <- tab.(j);
  tab.(j) <- temp;;

let deg_sort g =
  let n = Array.length g in
  let deg = deg g in
  let num = Array.init n (fun x -> x) 
  in
  for i = 0 to n-2 do
    let max = ref i in
    let deg_max = ref deg.(num.(i)) 
    in
    for j = i+1 to n-1 do
      let d =  deg.(num.(j)) in 
      if d > !deg_max 
      then (max := j; deg_max := d) 
    done;
    swap num i !max 
  done;
  num;;

(* question 4.2 *)
let welsh_powell g =
    greedy g (deg_sort g);;

(* ------------------------- *)
(* tests *)
let g1 = [|
  [|false; false; true; true; false|];
  [|false; false; false; true; true|];
  [|true; false; false; false; true|];
  [|true; true; false; false; false|];
  [|false; true; true; false; false|];
|];;

let g2 = [|
  [|false; false; false; false; true; true; true; false; false; false|];
  [|false; false; false; false; false; false; true; true; true; false|];
  [|false; false; false; false; false; true; false; false; true; true|];
  [|false; false; false; false; true; false; false; true; false; true|];
  [|true; false; false; true; false; false; false; false; true; false|];
  [|true; false; true; false; false; false; false; true; false; false|];
  [|true; true; false; false; false; false; false; false; false; true|];
  [|false; true; false; true; false; true; false; false; false; false|];
  [|false; true; true; false; true; false; false; false; false; false|];
  [|false; false; true; true; false; false; true; false; false; false|]
|];;

let g3 = [|
  [|false; true; true|];
  [|true; false; true|];
  [|true; true; false|]
|];;

let lbl1 = [|0; 1; 2; 3; 4|];;

disp g1;;
is_col g1 lbl1;;