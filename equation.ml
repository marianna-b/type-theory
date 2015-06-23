type equation = Var of string
		| Func of string * equation list

let rec to_string_term e =
   let rec to_string_list_term = function
     | [] -> ""
     | x::[] -> to_string_term x
     | x::xs -> (to_string_term x)^", "^(to_string_list_term xs)
in match e with
     | Var a-> a
     | Func (a, l) -> a ^"("^(to_string_list_term l)^")"

let to_string_eq (a, b) = (to_string_term a)^" = "^(to_string_term b)

let print_eq_list list = print_string (String.concat "\n" (List.map to_string_eq list))
