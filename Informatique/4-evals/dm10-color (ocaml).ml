type graph = int list array;;
let succ g v = g.(v);;
let size g = Array.length g;;

let color3 g =
	let n = size g in
	let c = Array.make n 0 in 
	let rec color i =
		i = n || assign i 0 || assign i 1 || assign i 2 
	and assign i v =
		c.(i) <- v;
		List.for_all (fun j -> j >= i || c.(j) <> v) (succ g i) && color (i+1)
	in
	if color 0 then c else raise Not_found
;;

let g = [|
	[1; 3];
	[0; 2];
	[1; 3];
	[2; 0]
|];;

let g = [|
	[1];
	[0;2];
	[1; 0]
|];;

color3 g;;