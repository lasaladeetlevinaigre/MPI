(* graphes  *)
type vertex = int;;
type weight = float;;
type graph = (weight * vertex) list array;;
let succ (g:graph) u = g.(u);;
let size (g:graph) = Array.length g;;

(* file de priorité version mutable - pas efficace *)
type distance = weight;;
type 'a prioqueue = (distance * vertex) list ref;;

let create () = ref [];;

let is_empty q = (!q = []);;

let insert v weight q =
  if not (List.exists (fun (_, u) -> u = v) !q)
  then q := (weight, v) :: !q
  else 
    let rec update q' = match q' with
      | [] -> []
      | (w, u) :: lst when u = v -> (weight, v) :: (update lst)
      | (w, u) :: lst -> (w, u) :: (update lst)
    in
    q := update !q
;;

let extract q = match !q with
  | [] -> failwith "fonction extract -> file vide"
  | (weight, edge) :: q' ->
    let rec aux weight edge q' acc_q = match q' with
      | [] -> q := acc_q; (weight, edge)
      | (w, _) as c :: q'' when w > weight -> aux weight edge q'' (c :: acc_q)
      | (w, v) :: q'' -> aux w v q'' ((weight, edge) :: acc_q)
    in aux weight edge q' []
;;

(* algorithme de Dijkstra *)
let dijkstra g vertex =
  let n = size g in
  let dist = Array.make n infinity in
  let q = create () in
  let visited = Array.make n false in
  let pred = Array.make n (-1) in
  let add v d =
    dist.(v) <- d; insert v d q;
  in
  add vertex 0.;

  while not (is_empty q) do
    let dv, v = extract q in
    if not visited.(v) then (
      visited.(v) <- true;
      List.iter (
        fun (dvu, u) ->
          let d = dv +. dvu in
          if d < dist.(u) then (add u d; pred.(u) <- v)
        )
        (succ g v)
    );
  done;
  dist, pred
;;

let path g u v =
  let _, pred = dijkstra g u in
  let rec aux x = match x with
    | -1 -> []
    | _ -> x :: (aux pred.(x))
  in
  List.rev (aux v)
;;


let a = 0;;
let b = 1;;
let c = 2;;
let d = 3;;
let g = [|
  [ (3., b); (1., c) ];
  [ (1., d) ];
  [ (1., b); (5., d) ];
  [];
|];;

let a = 0;;
let b = 1;;
let c = 2;;
let d = 3;;
let e = 4;;
let g = [|
  [ (6., b); (3., c); (5., d) ];
  [ (6., a); (2., d) ];
  [ (3., a); (1., d); (11., e) ];
  [ (5., a); (2., b); (1., c); (4., e)];
  [ (11., c); (4., d) ];
|];;

dijkstra g a;;
path g a e;;