open Lambda
module L = Lambda
module H = Hashtbl
module M = Map.Make(struct type t = int let compare = compare end)

type expr = Var of int 
          | App of expr * expr 
          | Lambda of expr

let rec get_index' list elem idx =
  match list with
    [] -> -1
    | x::_ when (x = elem) -> idx
    | x::xs -> get_index' xs elem (idx + 1)

let get_index list elem = get_index' list elem 0

let rec get_elem list idx = if idx = 0 then List.hd list else get_elem (List.tl list) (idx - 1)

let convert l free =
  let names = H.create 4 in
  let rec convert' idx = function
    | L.Var a when (H.mem names a) -> Var (idx - H.find names a)
    | L.Var a                      -> 
   					free := List.append !free [a] ;
    					Var (-List.length !free)
    | L.Lambda (v, e)              -> begin
                                        H.add names v (idx + 1);
                                        let e' = convert' (idx + 1) e in
					H.remove names v;
					Lambda e'
			              end
    | L.App (a, b)                 -> App (convert' idx a, convert' idx b)
  in convert' 0 l

let rec to_string_debruijn = function
      | App (a, b) -> "("^(to_string_debruijn a)^" "^(to_string_debruijn  b)^")"
      | Lambda b -> "(\\."^(to_string_debruijn b)^")"
      | Var a -> string_of_int a

(* need to fix adding: need to add only to variables greater than depth of expr2 *)
let rec recount expr d = 
	(*print_endline ((to_string_debruijn expr)^ " " ^ (string_of_int d)^ " " ^ (string_of_int len));*)
	let rec recount' e l = match e with
		| Var a when (a >= l) -> Var (a + d)
		| Var a -> Var a
		| Lambda b -> Lambda (recount' b (l + 1))
		| App (a, b) -> App (recount' a l, recount' b l)
	in recount' expr 0

let rec decrement = function
	| Var a when (a >= 0) -> Var (a - 1)
	| Var a -> Var a
	| Lambda b -> Lambda (decrement b)
	| App (a, b) -> App (decrement a, decrement b)

let rec depth = function
	| Var _ -> 0
	| App (a, b) -> let r = depth a in
			let l = depth b in
			if (r > l) then r else l
	| Lambda a -> 1 + (depth a)

(* need to fix : need to dec only to variables greater than depth of expr *)
let rec sub_debruijn expr expr2 len = 
	let rec subd expr d = 
		match expr with 
		| Var a when (d < a) -> decrement expr
		| Var a when (d > a) -> expr
		| Var a -> recount expr2 d 
		| Lambda b -> Lambda (subd b (d + 1))
		| App (a, b) -> App (subd a d, subd b d)
	in subd expr 0 

let rec betareduct' expr idx = match expr with
	Var a -> expr
	| Lambda a -> Lambda (betareduct' a (idx + 1))
	| App (Lambda x, b) -> let r = betareduct' b idx in
				sub_debruijn x r (depth r)
	| App (a, b) -> let x = betareduct' a idx in
				if (x = a) then	
					App (a, betareduct' b idx)
				else
					App (x, b)
and betareduct expr = betareduct' expr 0

let rec get_name a = 
	let c = String.make 1 (Char.chr((Char.code 'a') + a mod 26)) in
	if (a / 26 = 0) then c else c^(get_name (a / 26))

let get_new_name list free idx am = 
	while (let res = get_name(!am) in (get_index free res) <> -1) do
		am := (!am + 1)
  	done ;
	am := (!am + 1);
	list := (M.add idx !am !list) ; get_name(!am)

let rec convert_back' expr list idx free am = match expr with
	Var a when (a < 0) -> L.Var (get_elem free (-a - 1))
	| Var a -> L.Var (get_name (M.find (idx - a) !list))
	| Lambda b -> let res = get_new_name list free (idx + 1) am in 
		L.Lambda (res, convert_back' b list (idx + 1) free am)
	| App (a, b) -> L.App (convert_back' a list idx free am, convert_back' b list idx free am)

let convert_back expr free = 
	let list = ref M.empty in
	let amount = ref 0 in
	convert_back' expr list 0 free amount

let rec normalize e = match betareduct e with
	| e' when (e' = e) -> e'
	| e' ->  (*print_endline (to_string_lambda(convert_back e' [])); print_endline(to_string_debruijn(e')); *) normalize e'
