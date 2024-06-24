type braun = E | N of int * braun * braun

(* Deux tas de Braun, qui sont en particulier des tas de Braun *)
let t1 = N(1, N(3, N(7,E,E), E), N(4,E,E))
let t2 = N(2, N(3,E,E), N(12,E,E))

(* Question 2 *)
let rec hauteur (a:braun) :int = match a with
  |E -> -1
  |N(_,g,_) -> hauteur g + 1

let _ = hauteur t1
let _ = hauteur t2

(* Question 3 *)
let minimum (a:braun) :int = match a with
  |E -> max_int
  |N(r,_,_) -> r

let _ = minimum t1
let _ = minimum t2
  
(* Question 4 *)
let rec inserer (a:braun) (x:int) = match a with
  |E -> N(x,E,E)
  |N(r,g,d) when x < r -> N(x, inserer d r, g)
  |N(r,g,d) -> N(r, inserer d x, g)

let _ = inserer t1 9

(* Question 5 *)
let rec extraire_min (a:braun) :int*braun = match a with
  |E -> failwith "extraction dans un arbre vide"
  |N(x,g,d) -> x, fusionner g d

let _ = extraire_min t1

(* Question 6 *)
let rec extraire_element (a:braun) :int*braun = match a with
  |E -> failwith "extraction dans un arbre vide"
  |N(r,E,E) -> r, E
  |N(r,g,d) -> let x, gg = extraire_element g in x, N(r,d,gg)

let _ = extraire_element t1

(* Question 7 *)
let rec remplacer_min (a:braun) (x:int) :braun = match a with
  |E -> failwith "pas de minimum à remplacer"
  |N(r,g,d) -> let min_gauche = minimum g and min_droit = minimum d in
               if x <= min min_gauche min_droit
               then N(x,g,d)
               else
                 if min_gauche <= min_droit
                 then N(min_gauche, remplacer_min g x, d)
                 else N(min_droit, g, remplacer_min d x)

let _ = remplacer_min t1 12

(** Entrée : deux tas de Braun t et t' tels que |t'| <= |t| <= |t'| + 1.
Sortie : un tas de Braun contenant l'union des éléments contenus dans t et t'. *)
let rec fusionner (g:braun) (d:braun) :braun = match g, d with
    |E, E |N(_,E,E), E -> g
    |N(rg, gg, dg), N(rd, gd, dd) ->
      if rg <= rd then N(rg, d, fusionner gg dg)
      else let x, g' = extraire_element g in
           N(rd, remplacer_min d x, g')
    |_ -> failwith "conditions sur les arbres non respectées dans fusionner"

let _ = fusionner t1 t2