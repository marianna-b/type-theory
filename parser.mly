%{
    module L = Lambda
    module E = Equation
%}

%token <string> TVar
%token TOpenPar TClosePar TSlash TDot TEoln TEOF
%token TOpenBr TCloseBr TAss
%token TEq TComma
%start lambda_expr
%start lambda_assign
%start equation
%type <Lambda.lambda> lambda_expr
%type <Lambda.lambda * string * Lambda.lambda> lambda_assign
%type <Equation.equation * Equation.equation> equation
%%

lambda_assign: expr TOpenBr TVar TAss expr TCloseBr { ($1, $3, $5) }
                ;

lambda_expr: expr TEoln TEOF { $1 }
         | expr TEOF { $1 }
         ;

equation: term TEq term TEoln TEOF { ($1, $3) }
          | term TEq term TEOF { ($1, $3) }
          ;

term: TVar TOpenPar termlist TClosePar { E.Func ($1, $3) }
        | TVar { E.Var $1 }
        ;

termlist: termlist TComma term { List.append $1 [$3] }
          | term { [$1] }
          ;

expr: lambda         { $1 }
      | apply lambda { L.App ($1, $2) }
      | apply        { $1 }
      ;

lambda: TSlash TVar TDot expr { L.Lambda ($2, $4) }
      ;

apply: apply atom { L.App ($1, $2) }
     | atom       { $1 }
     ;

atom: TOpenPar expr TClosePar { $2 }
    | TVar                    { L.Var $1 }
    ;
