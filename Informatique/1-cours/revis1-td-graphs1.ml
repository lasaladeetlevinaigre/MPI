type graph = (int, int list) Hashtbl.t

(* fonctions préliminaires *)
let rec print_lst lst = match lst with
  | [] -> Printf.printf "\n"
  | h :: q -> Printf.printf "%d " h; print_lst q;;

let print_hashtbl ht =
  print_newline();
  Hashtbl.iter (fun x y -> Printf.printf "%d -> " x; print_lst y) ht;;

let rec lst_remove lst x = match lst with
  | [] -> []
  | y :: q  when y = x -> lst_remove q x
  | y :: q -> (y :: lst_remove q x);;

(* question 1 *)
let vertex_exist (gr: graph) v = 
  Hashtbl.mem gr v;;

let edge_exist (gr: graph) v1 v2 = 
  List.mem v2 (Hashtbl.find gr v1);;

let vertex_neighbors (gr: graph) v = 
  Hashtbl.find gr v;;

let edge_add (gr: graph) v1 v2 =
  if not (edge_exist gr v1 v2) then (
    Hashtbl.replace gr v1 (v2::(vertex_neighbors gr v1));
    Hashtbl.replace gr v2 (v1::(vertex_neighbors gr v2))
  );;

let di_edge_add (gr: graph) v1 v2 =
  if not (edge_exist gr v1 v2) then 
  Hashtbl.replace gr v1 (v2::(vertex_neighbors gr v1));;

let edge_remove (gr: graph) v1 v2 =
  let lst_neighbors = vertex_neighbors gr v1 in
  let new_lst_neighbors = lst_remove lst_neighbors v2
  in Hashtbl.replace gr v1 new_lst_neighbors;
  let lst_neighbors = vertex_neighbors gr v2 in
  let new_lst_neighbors = lst_remove lst_neighbors v1
  in Hashtbl.replace gr v2 new_lst_neighbors;;

let di_edge_remove (gr: graph) v1 v2 =
  let lst_neighbors = Hashtbl.find gr v1 in
  let new_lst_neighbors = lst_remove lst_neighbors v2
  in Hashtbl.replace gr v1 new_lst_neighbors;;

let vertex_add (gr: graph) v = 
  if not (vertex_exist gr v) then Hashtbl.add gr v [];;

let vertex_remove (gr: graph) v =
  Hashtbl.iter 
    (fun u lst  -> 
      let new_lst = lst_remove lst v in
      Hashtbl.replace gr u new_lst)
    gr;
  Hashtbl.remove gr v;;

(* question 2 *)
let g1 = Hashtbl.create 25;;
Hashtbl.add g1 0 [1;2;3;4;5;6;7;8];;
Hashtbl.add g1 1 [0;2;8;9];;
Hashtbl.add g1 2 [0;1;3;10];;
Hashtbl.add g1 3 [0;2;4;11];;
Hashtbl.add g1 4 [0;3;5;12];;
Hashtbl.add g1 5 [0;4;6;13];;
Hashtbl.add g1 6 [0;5;7;14];;
Hashtbl.add g1 7 [0;6;8;15];;
Hashtbl.add g1 8 [0;7;1;16];;
Hashtbl.add g1 9 [1;10;16;17];;
Hashtbl.add g1 10 [2;9;11;18];;
Hashtbl.add g1 11 [3;10;12;19];;
Hashtbl.add g1 12 [4;11;13;20];;
Hashtbl.add g1 13 [5;12;14;21];;
Hashtbl.add g1 14 [6;13;15;22];;
Hashtbl.add g1 15 [7;14;16;23];;
Hashtbl.add g1 16 [8;15;9;24];;
Hashtbl.add g1 17 [9];;
Hashtbl.add g1 18 [10];;
Hashtbl.add g1 19 [11];;
Hashtbl.add g1 20 [12];;
Hashtbl.add g1 21 [13];;
Hashtbl.add g1 22 [14];;
Hashtbl.add g1 23 [15];;
Hashtbl.add g1 24 [16];;

let g2 = Hashtbl.create 8;;
Hashtbl.add g2 0 [1; 2];;
Hashtbl.add g2 1 [0; 2];;
Hashtbl.add g2 2 [0; 1];;
Hashtbl.add g2 3 [5];;
Hashtbl.add g2 4 [5];;
Hashtbl.add g2 5 [3; 4; 7];;
Hashtbl.add g2 6 [];;
Hashtbl.add g2 7 [5];;

let g3 = Hashtbl.create 9;;
Hashtbl.add g3 0 [1;3];;
Hashtbl.add g3 1 [0;2;3];;
Hashtbl.add g3 2 [1;5];;
Hashtbl.add g3 3 [0; 4; 6;];;
Hashtbl.add g3 4 [1;3;5;7];;
Hashtbl.add g3 5 [2;4;8];;
Hashtbl.add g3 6 [3;7];;
Hashtbl.add g3 7 [4;6;8];;
Hashtbl.add g3 8 [5;8];;

(* question 3 *)
let dfs g v_init =
    
  let n = Hashtbl.length g in
  let visited = Array.make n false in
  let pred = Array.make n (-1) in

  let rec dfs_visit gr u =
    visited.(u) <- true;
    List.iter (
      fun v -> 
        if not visited.(v) then (
          pred.(v) <- u;
          dfs_visit gr v
        )
      ) 
      (vertex_neighbors gr u)
  in

  dfs_visit g v_init;
  pred;;

(* question 4 *)
let bfs g v_init =

  let n = Hashtbl.length g in
  let visited = Array.make n false in
  let pred = Array.make n (-1) in

  let q = Queue.create () in
  Queue.add v_init q;
  visited.(v_init) <- true;

  while not (Queue.is_empty q) do
    let v = Queue.take q in
    List.iter 
    (fun w ->
      if not visited.(w) then (
        visited.(w) <- true;
        pred.(w) <- v; 
        Queue.add w q
      )
    )
    (vertex_neighbors g v)
  done;

  pred;;

(* Constructions de chemins de v1 à v2 *)
let dfs_path g v1 v2 =
  let pred = dfs g v1 in
  let rec make_path v = 
    if v = (-1) then []
    else v :: (make_path pred.(v))
  in
  List.rev (make_path v2);;

let bfs_path g v1 v2 =
  let pred = bfs g v1 in
  let rec make_path v = 
    if v = (-1) then []
    else v :: (make_path pred.(v))
  in
  List.rev (make_path v2);;

(* exemples *)
let g = g2;;
let v_init = 0
let pred_dfs = dfs g v_init;;
let pred_bfs = bfs g v_init;;

let g = g1;;
let v1 = 0 and v2 = 17;;
dfs_path g v1 v2;;
bfs_path g v1 v2;;