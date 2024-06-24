(* graphes pondérés non orientés *)
(* Graphes orientés pondérés, par listes d'adjacence *)

type wgraph = (int * float) list array;;
type edge = float * int * int;;
type heap_queue = { t: edge array; mutable idx: int };;

(* graphe *)
let empty_edge = (0.0, -1, -1);;

let g_create n = Array.make n [];;

let g_size g = Array.length g;;

let g_has_edge g u v = List.mem_assoc v g.(u);;

let g_weight g u v =
  assert (g_has_edge g u v);
  List.assoc v g.(u)
;;

let g_succ g u = g.(u);;

let g_add_edge g (d, u, v) =
  assert (not (g_has_edge g u v));
  g.(u) <- (v, d) :: g.(u);
  g.(v) <- (u, d) :: g.(v)
;;

let g_edges g =
  let lst = ref [] in
  for i = 0 to g_size g - 1 do
    List.iter (fun (j, d) -> 
      if j > i 
      then lst := (d, i, j) :: !lst) (g_succ g i)
  done;
  !lst
;;

(* file de priorité *)

let hq_create size = { t = Array.make size empty_edge; idx = 0 };;

let hq_size h = Array.length h.t;;

let hq_empty h = h.idx = 0;;

let hq_full h = h.idx = hq_size h;;

let swap t i j =
  let x = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- x
;;

let rec sift_up t i = match i with
  | 0 -> ()
  | _ -> let j = (i-1)/2 in
         if t.(i) < t.(j) then (
            swap t i j;
            sift_up t j 
         )
;;

let sift_down t k n =
  let rec aux = function
    | i when 2*i+1 >= n -> ()
    | i -> 
        let j = 
          if (2*i+2 = n) || (t.(2*i+1) < t.(2*i+2)) 
          then 2*i+1 else 2*i+2
        in
        if t.(i) < t.(j) 
        then (swap t i j; aux j)
  in aux k
;;

let hq_extract h = match hq_empty h with
  | true -> failwith "file vide"
  | _ ->  let x = h.t.(0) in
          h.t.(0) <- h.t.(h.idx-1);
          h.idx <- h.idx - 1;
          h.t.(h.idx) <- empty_edge;
          sift_down h.t 0 h.idx;
          x
;;

let hq_insert h x = match hq_full h with
  | true -> failwith "file pleine"
  | _ -> h.t.(h.idx) <- x;
         sift_up h.t h.idx;
         h.idx <- h.idx + 1
;;

let make_heap t =
  let n = Array.length t in 
  for i = 1 to n-1 do
    sift_up t i
  done
;;
(* ========================= *)
(* union-Find *)
(* structure union-find,
   avec union pondérée et 
   compression de chemins *)

type uf = { 
  link: int array; 
  rank: int array 
};;

let uf_create n =
  { link = Array.init n (fun i -> i);
    rank = Array.make n 0; 
};;

let rec uf_find uf i =
  let p = uf.link.(i) in
  if p = i then i else (
    let r = uf_find uf p in
    uf.link.(i) <- r;
    r
  )
;;

let uf_union uf i j =
  let ri = uf_find uf i in
  let rj = uf_find uf j in
  if ri <> rj then
    if uf.rank.(ri) < uf.rank.(rj) then
      uf.link.(ri) <- rj
    else (
      uf.link.(rj) <- ri;
      if uf.rank.(ri) = uf.rank.(rj) then
        uf.rank.(ri) <- uf.rank.(ri) + 1
    )
;;