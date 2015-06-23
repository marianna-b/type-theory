module E = Equation

let rec to_string_term e =
let rec to_string_list_term = function
      | [] -> ""
      | x::[] -> to_string_term x
      | x::xs -> (to_string_term x)^", "^(to_string_list_term xs)
in
match e with
      | E.Var a-> a
      | E.Func (a, l) -> a ^"("^(to_string_list_term l)^")"

let to_string_eq (a, b) = (to_string_term a)^" = "^(to_string_term b)

let _ =
        let s = read_line () in
        let (a, b) = Parser.equation Lexer.token (Lexing.from_string s) in
        print_endline (to_string_eq (a, b))
