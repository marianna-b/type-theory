open Lambda

let _ =
       let s = read_line() in
       let e = Parser.lambda_expr Lexer.token (Lexing.from_string s) in
       print_string (to_string_lambda e)
