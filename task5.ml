open Equation
open Unification

let print_eq e = print_endline (to_string_eq e)

let _ =
let rec read_all _ = try
      let s = input_line stdin in
      let eq = Parser.equation Lexer.token (Lexing.from_string s) in
      eq :: read_all() with End_of_file -> [] in
    let r = read_all () in
    try List.iter print_eq (unificate r) with UFail -> print_endline "No solution"
