open Lambda
open Debruijn
module D = Debruijn

let _ =
       	let s = read_line() in
       	let e = Parser.lambda_expr Lexer.token (Lexing.from_string s) in
	let vars = ref [] in
	let reduct = betareduct(convert e vars) in
	let res = convert_back reduct !vars in
	print_string (to_string_lambda res)
