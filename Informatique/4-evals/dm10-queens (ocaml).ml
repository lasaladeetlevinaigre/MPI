(* vérification des contraintes *)
let check board k =
  let i = ref 0 in
  while (!i < k) && (board.(!i) != board.(k)) && (abs(board.(!i) - board.(k)) != abs(!i-k)) do
    incr i
  done;
  !i = k
;;

(* calcul d'une solution *)
let solve n =
  let board = Array.make n 0 in
  let rec aux1 k =
    (k = n) || aux2 k 0
  and aux2 k v =
    if v < n 
    then (board.(k) <- v; (check board k) && (aux1 (k+1))) || (aux2 k (v+1))
    else  false
  in
  if aux1 0 then board else raise Not_found
;;

(* affichage d'une grille *)
let show n =
  let board = solve n in
  Printf.printf "\n";
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      Printf.printf (if j = board.(i) then " Q" else " .");
    done;
    Printf.printf "\n"
  done
;;

show 8;;