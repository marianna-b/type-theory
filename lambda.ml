type lambda = Var of string
		| App of lambda * lambda
		| Lambda of string * lambda
