type 'a abralea = Vide | N of int * 'a abralea * 'a * 'a abralea

(* Question 1 *)
let a = N(6, N(2, Vide, 'a', N(1, Vide, 'g', Vide)), 'h',
             N(3, N(1, Vide, 'm', Vide), 'r',
                  N(1, Vide, 'z', Vide)))

(* Question 2 *)
let taille (a:'a abralea) :int = match a with
  |Vide -> 0
  |N(n,_,_,_) -> n

let _ = taille a

(* Question 3 *)
let rec hauteur (a:'a abralea) :int = match a with
  |Vide -> -1
  |N(_,g,_,d) -> 1 + max (hauteur g) (hauteur d)

let _ = hauteur a

(* Fonctions de rotation, à utiliser en boite noire *)
let rotation_droite = function
    | N(n, N(_, a, x, b), y, c) -> N(n, a, x, N(taille b + taille c + 1, b, y, c))
    | _                         -> failwith "Rotation impossible"

let rotation_gauche = function
    | N(n, a, x, N(_, b, y, c)) -> N(n, N(taille a + taille b + 1, a, x, b), y, c)
    | _                         -> failwith "Rotation impossible"

(* Question 4 *)
let rec insere_racine (x:'a) (a:'a abralea) :'a abralea = match a with
  |Vide -> N(1, Vide, x, Vide)
  |N(_,_,y,_) when x = y -> failwith "doublons interdits"
  |N(n,g,y,d) when x < y -> let t = insere_racine x g in rotation_droite (N(n+1,t,y,d))
  |N(n,g,y,d) -> let t = insere_racine x d in rotation_gauche (N(n+1,g,y,t))

let _ = insere_racine 'n' a

(* Question 6 *)
let rec insere (x:'a) (a:'a abralea) :'a abralea = match a with
  |Vide -> N(1, Vide, x, Vide)
  |N(_,_,y,_) when y = x -> failwith "doublons interdits"
  |N(n,g,y,d) -> match Random.int (n+1) with
                 |0 -> insere_racine x a
                 |k when x < y -> N(n+1, insere x g, y, d)
                 |k -> N(n+1, g, y, insere x d)

let _ = insere 'z' a
let _ = insere 'n' a

(* Question 7 *)
let rec fusion (a:'a abralea) (b:'a abralea) :'a abralea = match a, b with
  (* On suppose que toutes les étiquettes de a sont strictement inférieures à celles de b. *)
  |Vide, _ -> b
  |_, Vide -> a
  |N(nx,gx,x,dx), N(ny,gy,y,dy) when Random.int (nx+ny+1) < nx -> N(nx+ny, gx, x, fusion dx b)
  |N(nx,gx,x,dx), N(ny,gy,y,dy) -> N(nx+ny, fusion a gy, y, dy)

(* Fonction fournie *)
let rec supprime (x:'a) (a:'a abralea) :'a abralea =
  match a with
  |Vide -> failwith "Élément non trouvé"
  |N(n, g, y, d) -> if x = y then fusion g d
                    else if x < y then N(n - 1, supprime x g, y, d)
                    else N(n - 1, g, y, supprime x d)

let _ = supprime 'h' a

(* Question 9 *)
let _ =
    Random.self_init ();
    let b = ref Vide in
    for i = 0 to 99999 do
      b := insere i !b
    done;
    Printf.printf "hauteur : %d, taille : %d\n" (hauteur !b) (taille !b)
