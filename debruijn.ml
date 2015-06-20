open Lambda
module L = Lambda
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


let rec convert' expr list idx free =
  match expr with
    L.Var a -> let idx1 = get_index list a in
      		if idx1 <> -1 then
        		Var idx1
      		else
        		(let idx2 = get_index !free a in
          			if idx2 <> -1 then
            				Var (-1* idx2)
          			else
            				let len = List.length !free in
            					free := (List.append !free [a]) ; Var (-1 * (len + 1))
        		)

  | L.Lambda (a, b) -> Lambda (convert' b (List.append list [a]) (idx + 1) free)
  | L.App (a, b)    -> App (convert' a list idx free, convert' b list idx free)

(* free must be reference*)
let convert l free = convert' l [] 0 free 

let rec to_string_debruijn = function
        App (a, b) -> "("^(to_string_debruijn a)^" "^(to_string_debruijn  b)^")"
      | Lambda b -> "(\\."^(to_string_debruijn b)^")"
      | Var a -> string_of_int a

let rec recount expr d = match expr with
	Var a when (a >= 0) -> Var (a + d)
	| Var a -> Var a
	| Lambda b -> Lambda (recount b d)
	| App (a, b) -> App (recount a d, recount b d)

let rec decrement = function
	Var a when (a >= 0) -> Var (a - 1)
	| Var a -> Var a
	| Lambda b -> Lambda (decrement b)
	| App (a, b) -> App (decrement a, decrement b)

let rec sub_debruijn expr idx expr2 d = match expr with 
	Var a when (idx <> a) ->  decrement expr
	| Var a -> recount expr2 d
	| Lambda b -> Lambda (sub_debruijn b idx expr2 (d + 1))
	| App (a, b) -> App (sub_debruijn a idx expr2 d, sub_debruijn b idx expr2 d)


let rec betareduct' expr idx = match expr with
	Var a -> expr
	| App (Lambda x, b) -> betareduct' (sub_debruijn x idx b 0) idx
	| Lambda a -> Lambda (betareduct' a (idx + 1))
	| App (a, b) -> ( let expr2 = betareduct' a idx in
			  match expr2 with
				Lambda x -> betareduct' (App (expr2, b)) idx
				| _ -> App (expr2, betareduct' b idx)
			)

let betareduct expr = betareduct' expr 0

let rec get_elem list idx = if idx = 0 then List.hd list else get_elem (List.tl list) (idx - 1)

let rec get_name a = 
	let c = String.make 1 (Char.chr((Char.code 'a') + a mod 26)) in
	if (a / 26 = 0) then c else c^(get_name (a / 26))

let get_new_name list free idx am = 
	while (let res = get_name(!am) in (get_index free res) <> -1) do
		am := (!am + 1)
  	done ;
	list := M.add idx !am !list ; get_name(!am)

let rec convert_back' expr list idx free am = match expr with
	Var a when (a < 0) -> L.Var (get_elem free (-1*a - 1))
	| Var a -> L.Var (get_name (M.find a !list))
	| Lambda b -> let res = get_new_name list free idx am in 
		L.Lambda (res, convert_back' b list (idx + 1) free am)
	| App (a, b) -> L.App (convert_back' a list idx free am, convert_back' b list idx free am)

let convert_back expr free = 
	let list = ref M.empty in
	let amount = ref 0 in
	convert_back' expr list 0 free amount


