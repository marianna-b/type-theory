open Lambda
open Debruijn
module D = Debruijn

let _ =
	let s = read_line() in
	let e = Parser.lambda_expr Lexer.token (Lexing.from_string s) in
	let vars = ref [] in
        let conv = convert e vars in
	(*let str0 = to_string_debruijn conv in*)
	(*print_endline (str0);*)
	(*let str00 = to_string_lambda (convert_back conv !vars) in*)
	let reduct = normalize conv in
	(*let str1 = to_string_debruijn reduct in*)
	let res = convert_back reduct !vars in
	let str2 = to_string_lambda res in
	(*print_endline (str1);*)
	print_endline (str2)
(* List.iter (Printf.printf "%s ") !vars ; *)
