open Lambda
module L = Lambda
module H = Hashtbl
module M = Map.Make(struct type t = int let compare = compare end)

let rec get_index' list elem idx =
  match list with
    [] -> -1
    | x::_ when (x = elem) -> idx
    | x::xs -> get_index' xs elem (idx + 1)

let get_index list elem = get_index' list elem 0

let rec get_elem list idx = if idx = 0 then List.hd list else get_elem (List.tl list) (idx - 1)

let rec get_name a = 
	let c = String.make 1 (Char.chr((Char.code 'a') + a mod 26)) in
	if (a / 26 = 0) then c else c^(get_name (a / 26))

let get_new_name list free idx am = 
	while (let res = get_name(!am) in (get_index free res) <> -1) do
		am := (!am + 1)
  	done ;
	am := (!am + 1);
	list := (M.add idx !am !list) ; get_name(!am)

(*--------------------------------------------------------------------------------------------*)

type expr = Var of int 
          | App of expr * expr 
          | Lambda of expr
	  | Ref of expr ref
	  | Lazy of ((expr->expr) list)*expr


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
      | Lazy (list, b) -> "Lazy("^(to_string_debruijn b)^" + "^(string_of_int (List.length list))^")"
      | Var a -> string_of_int a
      | Ref r ->"Ref "^  (to_string_debruijn !r)

let rec recount d expr = 
	let rec recount' e l = match e with
		| Var a when (a >= l) -> Var (a + d)
		| Var a -> Var a
		| Lambda b -> Lambda (recount' b (l + 1))
		| App (a, b) -> App (recount' a l, recount' b l)
		| Ref r -> recount' !r l
		| Lazy _ -> failwith "No lazy in recount"
	in recount' expr 0

let rec sub_debruijn expr expr2 = 
	let rec subd e2 d expr = 
		match expr with 
		| Var a when (d < a) -> Var (if a>= 0 then a - 1 else a)
		| Var a when (d > a) -> expr
		| Var a -> Lazy ([recount d], e2)
		| Ref r -> subd e2 d !r
		| Lazy (list, e) -> Lazy ((List.append list [subd e2 d]), e)
		| Lambda b -> Lambda (subd e2 (d + 1) b)
		| App (a, b) -> App (subd e2 d a, subd e2 d b)
	in subd expr2 0 expr

let rec apply e = function
	| [] -> e
	| x::xs -> let a = x e in apply a xs

let rec apply_all = function
	| Var a -> Var a
	| Lambda a -> Lambda (apply_all a)
	| Ref r -> apply_all !r
	| Lazy (list, e) -> apply (apply_all e) list 
	| App (a, b) -> App (apply_all a, apply_all b)

let rec check_lazy_lambda = function
	| Lazy (list, Ref r) -> check_lazy_lambda !r
	| Lambda _ -> true
	| _ -> false
		
let rec betareduct' expr idx = match expr with
	| Var a -> (false, expr)
	| Lambda a -> let (y, x) = betareduct' a (idx + 2) in (y, Lambda x)
	| App (Lambda x, b) -> let (_, a) = betareduct' b idx in
				(true, sub_debruijn x (Ref (ref a)))
	| Ref r -> let (x, y) = betareduct' !r idx in 
		if x then (r := y ; (x, expr)) else (x, expr)
	| Lazy (list, e) -> (let (a, y) = betareduct' e idx in 
						if a then (true, Lazy (list, y))
						else (true, apply e list))
	| App (a, b) -> let func = (
	let (y, x) = betareduct' a idx in
		if (y) then	
			(true, App (x, b))
		else
			let (y, x) = betareduct' b idx in (y, App (a, x))) 
	in if (check_lazy_lambda a) then (true, App ((apply_all a), b)) else func
and betareduct expr = betareduct' expr 0

let rec convert_back' expr list idx free am = match expr with
	Var a when (a < 0) -> L.Var (get_elem free (-a - 1))
	| Var a -> L.Var (get_name (M.find (idx - a) !list))
	| Lambda b -> let res = get_new_name list free (idx + 1) am in 
		L.Lambda (res, convert_back' b list (idx + 1) free am)
	| App (a, b) -> L.App (convert_back' a list idx free am, convert_back' b list idx free am)
	| _ -> failwith "No ref or lazy in convert_back"

let convert_back expr free = 
	let list = ref M.empty in
	let amount = ref 0 in
	convert_back' expr list 0 free amount

let rec normalize e = match betareduct e with
	| (false, e') -> e'
	| (true, e') -> normalize e'
