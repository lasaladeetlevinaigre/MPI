let compress texte =
  let taille = String.length texte in
  let dico = Hashtbl.create 1024 in
  let max = ref 224 in
  for i = 0 to 223 do
    Hashtbl.add dico (String.make 1 (Char.chr (i+32))) (i+32)
  done;
  let rec aux n l =
    match n with
    | i when i >= taille -> l
    | _ -> 
      let rec aux2 t2 t i = match ((Hashtbl.mem dico t), i) with
        | (_, i) when i >= taille -> (t2, t, i)
        | (false, i) -> (t2, t, i)
        | (true, i) -> aux2 t (t ^ (String.make 1 texte.[i])) (i+1)
      in
      let (t2,t,i) = aux2 "" (String.make 1 (texte.[n])) (n+1) in
      if i < taille 
      then (
        Hashtbl.add dico t (!max);
        max := !max + 1;
        aux (i-1) ((Hashtbl.find dico t2) :: l)
      ) else (
        (Hashtbl.find dico t2) :: l
      )
  in
  List.rev (Char.code(texte.[taille-1]) :: (aux 0 []))  
;;
(* ajout artificiel du dernier caractere, car il ne passait pas *)

let decompress l =
  let dico = Hashtbl.create 300 in
  let max = ref 224 in
  for i = 0 to 223 do
    Hashtbl.add dico (i+32) (String.make 1 (Char.chr (i+32)));
  done;
  let rec aux l prec = match l with
    | [] -> ""
    | t :: q when t < !max -> 
        let car = Hashtbl.find dico t in  
        (* notons que la hashtbl.mem dico t dans le when ne donnait pas le bon resultat *) 
        if (prec <> "" ) 
        then (
          if String.length car > 1 
          then Hashtbl.add dico (!max) (prec ^ (String.make 1 (car.[0])))
          else Hashtbl.add dico (!max) (prec ^ car);
          max:= !max +1;
        );
        car^(aux q car)
    | t :: q -> 
        let car = prec ^ (String.make 1 (prec.[0])) in
        Hashtbl.add dico (!max) car;
        max := !max +1;
        car ^ (aux q car)
  in
  aux l ""
;;

let t ="que j'aime à faire connaître un nombre utile aux sages";;

let t2 = "lalalalalere";;
let h2 = compress t2;;
decompress h2;;
List.length h2;;
String.length t2;;

let t3 = "jj";;
let h3 = compress t3;;
decompress h3;;

let h = compress t ;;
List.length h;;
String.length t;;
decompress h;;
