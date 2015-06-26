module E = Equation
module U = Unification
module L = Lambda
module H = Hashtbl
open Set
module SS = Set.Make(String)

let rec get_index' list elem idx =
  match list with
    [] -> -1
    | x::_ when (x = elem) -> idx
    | x::xs -> get_index' xs elem (idx + 1)

let get_index list elem = get_index' list elem 0

let rec get_name a = 
let c = String.make 1 (Char.chr((Char.code 'a') + a mod 26)) in
	if (a / 26 = 0) then c else c^(get_name (a / 26))

let get_new_name free am = 
	while (let res = get_name(!am) in (get_index free res) <> -1) do
		am := (!am + 1)
  	done ;
	am := (!am + 1);
	get_name(!am)

(*-----------------------------------------------------------------------------------*)

let func a b = E.Func ("f", [a; b])

let get_types expr = 
	let am = ref 0 in
	let free = SS.elements (L.free_var expr) in
	let map = H.create 32 in
	let free_t = ref [] in
	let get_new _ = get_new_name free am in
	let process x = let n =  get_new () in 
		(H.add map x n ); (free_t := ((x, n)::!free_t)) in
	(List.iter process free) ;
	let rec get_types' = (function
		| L.Var a -> (E.Var (H.find map a), [])
		| L.Lambda (a, b) -> 
			let n = get_new () in
			H.add map a n;
			let (x, y) = get_types' b in
			H.remove map a;
			(func (E.Var n) x, y)
		| L.App (a, b) -> let (x, y) = get_types' a in
				let (m, n) = get_types' b in
				let l = get_new () in
				(E.Var l, ((x, func m (E.Var l)) :: y @ n)))
in let (q, w) = get_types' expr in
	(q, w, !free_t)

let rec find_sub e = function 
	| [] -> e
	| (a, b)::xs -> if a = e then b else find_sub e xs

let rec apply u = function
	| E.Func (f, a) -> E.Func (f, List.map (apply u) a)
  	| E.Var x -> find_sub (E.Var x) u

let _ = 
	let s = read_line() in
	let e = Parser.lambda_expr Lexer.token (Lexing.from_string s) in
	let (a, b, c) = get_types e in
	try 
		let u = U.unificate b in
		let t = apply u a in
		(print_endline (E.to_string_type t));
		List.iter (fun (a, b) -> print_endline (a^":"^b)) c
	with U.UFail -> print_endline "Не умеет типа"
