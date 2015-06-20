open Set
module SS = Set.Make(String)

type lambda = Var of string
		| App of lambda * lambda
		| Lambda of string * lambda

let rec to_string_lambda = function
        App (a, b) -> "("^(to_string_lambda a)^" "^(to_string_lambda b)^")"
      | Lambda (a, b) -> "(\\"^a^"."^(to_string_lambda b)^")"
      | Var a-> a

let print_set s = SS.iter print_endline s

let rec free_var = function
       App (a, b)    -> SS.union (free_var a) (free_var b)
     | Lambda (a, b) -> SS.remove a (free_var b)
     | Var a         -> SS.singleton a

let app_union_subst x y =
   match x with
      None -> y
    | Some a -> x

let rec check_if_free expr v vars = 
  match expr with
    Var a ->  None
  | App (a,b) -> app_union_subst (check_if_free a v vars) (check_if_free b v vars)
  | Lambda (a, b) -> if SS.exists (fun x -> x = v) (free_var b) then
      (if SS.exists (fun x -> x = a) vars then Some a else None)
      else None

let rec substitute expr v expr2 = 
  match expr with
    Var a when (a <> v) ->  expr
  | Var a -> expr2
  | Lambda (a, b) when (a <> v) -> Lambda (a, substitute b v expr2)
  | Lambda (a, b) -> expr
  | App (a, b)    -> App (substitute a v expr2, substitute b v expr2)
