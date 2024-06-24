type point = {x : int; y : int};;

let tab_points = [|
	{x = 0; y = 0};
	{x = 1; y = 4};
	{x = 1; y = 8};
	{x = 4; y = 1};
	{x = 4; y = 4};
	{x = 5; y = 9};
	{x = 5; y = 6};
	{x = 7; y = - 1};
	{x = 7; y = 2};
	{x = 8; y = 5};
	{x = 11; y = 6};
	{x = 13; y = 1}
|];;

let lexico p1 p2 = (p1.y < p2.y) || (p1.x < p2.x && p1.y = p2.y);;

let plus_bas_point p1 p2 = if lexico p1 p2 then p1 else p2;;

let plus_bas t =
  let p1 = ref t.(0) and n = Array.length t in
    for i = 1 to n - 1 do p1 := plus_bas_point !p1 t.(i) done;
    !p1
;;

let orient p1 p2 p3 = 
	match (p2.x - p1.x) * (p3.y - p1.y) - (p3.x - p1.x) * (p2.y - p1.y) with
    | det when det < 0 -> - 1
    | det when det > 0 -> 1
    | det -> 0
;;

let prochain p1 t =
  let p2 = ref t.(0) in
    if !p2 = p1 then p2 := t.(1);
    let n = Array.length t in
    for i = 0 to n - 1 do
      let p3 = t.(i) in
        if p3 <> p1 && p3 <> !p2 && orient p1 !p2 p3 < 0 then p2 := p3
    done;
    !p2
;;

let convex tab =
  let p0 = plus_bas tab in
    let p1 = ref (prochain p0 tab)
    and lst_env = ref [p0] in
    while not (!p1 = p0) do
      lst_env := !p1 :: !lst_env;
      p1 := prochain !p1 tab
    done;
    lst_env
;;

convex tab_points;;