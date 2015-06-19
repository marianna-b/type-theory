{
  open Parser
}

let whitespace = [' ' '\t' '\r']
let variable   = ['a'-'z']['a'-'z' '0'-'9']*

rule token = parse
             | whitespace       { token lexbuf }
             | variable as var  { TVar var }
             | '\n'             { TEoln }
             | '('              { TOpenPar }
             | ')'              { TClosePar }
             | '\\'             { TSlash }
             | '.'              { TDot }
             | eof              { TEOF }
