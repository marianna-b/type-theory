let rec toStringLambda = function
        Lambda.App (a, b) -> "("^(toStringLambda a)^" "^(toStringLambda b)^")"
      | Lambda.Lambda (a, b) -> "(\\"^a^"."^(toStringLambda b)^")"
      | Lambda.Var a-> a

let _ =
       let s = read_line() in
       let e = Parser.lambda_expr Lexer.token (Lexing.from_string s) in
       print_string (toStringLambda e)
