open Equation

let rec get_eq_from_list a b = match (a, b) with
     | ([], []) -> []
     | (x::xs, y::ys) -> List.append [(x, y)] (get_eq_from_list xs ys)
     | _ -> failwith "Length check failed"

let rec exists_in a list = match list with
     | [] -> false
     | x::xs -> match x with
                       | Var y when (a = y) -> true
                       | Func (y, z) -> if (exists_in a z) then true else (exists_in a xs)
                       | _ -> exists_in a xs

let rec sub x t a = match a with 
                     | (Var y) when (y = x) -> t
                     | Func (y, z) -> (Func (y,  List.map (sub x t) z))
                     | _-> a

let rec replace x t list = match list with
     | [] -> []
     | (a, b)::xs -> (sub x t a, sub x t b)::(replace x t xs)

let check = function
     | (Func (a, b), Func (c, d)) when (a = c && (List.length b) = (List.length d)) -> true
     | (Func (a, b), Func (c, d)) -> false
     | (Var a, Func (b, c)) when (exists_in a c) -> false
     | _ -> true
exception UFail
let rec unificate' e1 e2 = match e2 with
     | [] -> e1
     | x::xs -> if check x then 
                    (
                     match x with
                       | (Var a, Var b) when (a = b) -> unificate' e1 xs
                       | (Var a, Var b) when a > b -> unificate' (List.append (List.filter ((<>) x) e1) [(Var b, Var a)]) xs
                       | (Var a, Var b) -> unificate' (List.append (List.filter ((<>) x) e1) [x]) xs
                       | (Var a, Func (b, c)) -> (
                                                  let e1' = (replace a (Func (b, c)) e1) in
                                                      if e1' <> e1 then List.append e1' e2
                                                      else unificate' (List.append e1 [x]) xs
                                                  )
                       | (Func (a, b), Var c) ->
                               unificate' (List.filter ((<>) x) e1) ((Var c, Func (a, b))::xs)
                       | (Func (a, b), Func (c, d)) -> List.append (get_eq_from_list b d) (List.append e1 xs)
                    )
                else raise UFail


let rec unificate e = (*print_eq_list e ; print_endline "" ; print_endline "" ;*) match (unificate' [] e) with
     | e' when e = e' -> List.sort_uniq compare e
     | e' -> unificate e'
