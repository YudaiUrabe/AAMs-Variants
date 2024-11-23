(* Object Langugae -untyped lambda calculus(CbV)- *)

type var = string
and Lambda = var * term

and term = 
  | TmVar of var
  | TmAbs of Lambda
  | TmApp of term * term

(* SYNTAX of CEK machine *)

(* configuration_ triple 
 (Control string
an expression currently being evaluated)
*)
and Σ = term * Env * Cont


and d = Clo of Lambda * Env

(* Environment
a map that contains the addresses of all variables in the lexical scope *)

(* and Env = TmVar -> val *)

(* Continuation
evaluation context
*)

type Cont = 
  | Done
  | Ar of term * Env * Cont
  | Fn of Lambda * Env * Cont






(* (type operator) *)



(* syntactic sugar *)



(* transition relation as a partial function *)



(* injection function *)



(* Continuation
program’s execution context *)



(* evaluation function *)

    (* auxiliary function *)


(* isFinal *)
