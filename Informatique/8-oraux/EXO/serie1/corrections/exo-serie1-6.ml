
(******************************************************)
(* Concours commun INP                                *)
(* https://www.concours-commun-inp.fr                 *)
(* CC BY-NC-SA, Novembre 2023                         *)
(* https://creativecommons.org/licenses/by-nc-sa/4.0/ *)
(******************************************************)

type graphe = int list array

let g1 = [|[1;3];[];[0;1;3];[1]|]

let g2 = [|[];[0];[0;3;1];[1]|]

let g3 = [|[1;2;8];[6;7];[0;4;5;9];[];[3;7];[0;1;3];[4;5;9];[1;2;3;4];[3;5;6];[2]|]
(*Question 1*)
let est_sommet g a =  (0 <=a) && (a < Array.length g)


(*Question 2*)
let rec appartient liste a = match liste with
    | [] -> false
    | b::suite -> (b = a) || (appartient suite a)

(*Question 3*)
let rec est_chemin g liste = match liste with
   | [] -> true
   | [a] -> est_sommet g a  (*pas nécessaire :  *)
   | a::b::suite -> (appartient (g.(a)) b) && (est_chemin g (b::suite))

(*Question 4*)
 let est_chemin_simple_sans_issue g liste =
    let n = Array.length g in
    let visites = Array.make n false in
    let rec test_aux liste = match liste with
        | [] -> false
        | [a] -> [] = (List.filter (fun x -> not visites.(x)) (g.(a)))
        | a::b::suite ->
            begin
                visites.(a) <- true ;
                (appartient g.(a) b) && (not visites.(b))
                && (test_aux (b::suite))
            end
        in
        test_aux liste

(*Question 5*)
let genere_chemins_simples_sans_issue (g:graphe) =
    let taille = Array.length g in
    let liste_chemins = ref [] in (*garde en mémoire les chemins déjà trouvés*)
    let visites = Array.make taille false in (*garde en mémoire les sommets en cours de visite *)
    let chemin_courant_envers = ref [] in (*garde en mémoire le début d'un chemin*)
    let rec profondeur s = (*trouve tous les chemins simples sans issue commençant par s*)
        if not visites.(s) then
        begin
            visites.(s) <- true ;
            chemin_courant_envers := s::(!chemin_courant_envers) ;
            let voisins_libres = List.filter (fun x -> not visites.(x)) g.(s) in
            if voisins_libres = [] then
                begin
                    liste_chemins := (List.rev !chemin_courant_envers)::(!liste_chemins)
                end
            else
                begin
                    List.iter profondeur voisins_libres
                end ;
                visites.(s) <- false ;
                chemin_courant_envers := List.tl !chemin_courant_envers ;
        end
    in
    for i = 0 to (taille-1) do
        profondeur i
    done ; !liste_chemins 


