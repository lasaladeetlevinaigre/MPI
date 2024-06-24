let sub_string str idx len =
  let n = String.length str
  and s_str = ref ""
  and i = ref idx
  in
  let idx_max = min n (idx+len) in
  while !i < idx_max do
    s_str := !s_str ^ (Char.escaped str.[!i]);
    incr i
  done;
  !s_str

let sub_string1 str idx len =
  let n = String.length str in
  let idx_max = min n (idx + len) 
  in
  let rec aux acc_str = function
    | i when i = idx_max -> acc_str
    | i -> aux (acc_str ^ (Char.escaped str.[i])) (i+1)
  in 
  aux "" idx

let find_pattern p t =
  let n = String.length t 
  and m = String.length p
  and i = ref 0 
  and found = ref false in
  while !i <= n - m && not !found do
    if sub_string t !i m = p then found := true;
    incr i;
  done;
  !found

let prefix_array p =
  let m = String.length p in
  let pi = Array.make (m+1) 0 in
  for q = 2 to m do
    let k = ref pi.(q-1) in
    while !k > 0 && p.[!k] <> p.[q-1] do k := pi.(!k) done;
    if p.[!k] = p.[q-1] then k := !k + 1;
    pi.(q) <- !k;
  done;
  pi

let kmp text pattern =
  let n = String.length text
  and m = String.length pattern
  and pi = prefix_array pattern
  and pos = ref []
  and q = ref 0 in
  for i = 0 to n - 1 do
    while !q > 0 && pattern.[!q] <> text.[i] do q := pi.(!q) done;
    if pattern.[!q] = text.[i] then q := !q + 1;
    if !q = m then (
      pos := (i-m+1) :: !pos;
      q := pi.(!q);
    );
  done;
  List.rev !pos


(* let text = "abracadabra"
let pattern = "br"
 *)
let text = "abababbaabababbaabababba"
let pattern = "abababba"
let fp = find_pattern pattern text
let pref = prefix_array pattern
let fp_kmp = kmp text pattern
(* kmp "aabababaa" "ab" *)


type automate = {
  start: int;
  delta: int -> char -> int;
  success: int
}

let accepte a t =
  let q = ref a.start
  and n = String.length t in
  for i = 0 to n - 1 do
    q := a.delta !q t.[i];
  done;
  !q = a.success

let auto p =
  let m = String.length p
  and pi = prefix_array p in
  let delt q x =
    if q = m then m
    else (
      let q1 = ref q in
        while !q1 > 0 && p.[!q1] <> x do
          q1 := pi.(!q1);
        done;
        if p.[!q1] = x 
        then !q1 + 1 else 0
    ) 
  in {start=0; delta=delt; success=m}

let p = "bit"
let aut = auto p

let disp_auto_pattern p =
  let aut = auto p in
  let n_states = aut.success in
  let n = String.length p in
  for i = 0 to n-1 do
    for j = 0 to n_states do
      let c = p.[i] in
      Printf.printf "delta(%d, '%c') -> %d\n" j c (aut.delta j c)
    done;
  done

let _ = disp_auto_pattern p