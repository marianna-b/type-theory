open Lambda



let _ =
       let s = read_line() in
       let (e, v, a) = Parser.lambda_assign Lexer.token (Lexing.from_string s) in
       let vars = free_var a in
       match (check_if_free e v vars) with
       None -> print_string (to_string_lambda(substitute e v a))
     | Some x -> print_string ("Нет свободы для подставновки для переменной " ^ x)
