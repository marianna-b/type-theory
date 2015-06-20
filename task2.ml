open Set

module SS = Set.Make(String)

let print_set s = SS.iter print_endline s

let rec free_var = function
       Lambda.App (a, b)    -> SS.union (free_var a) (free_var b)
     | Lambda.Lambda (a, b) -> SS.remove a (free_var b)
     | Lambda.Var a         -> SS.singleton a

let _ =
       let s = read_line() in
       let e = Parser.lambda_expr Lexer.token (Lexing.from_string s) in
       print_set (free_var e)
