(* Object Langugae -untyped lambda calculus(CbV)- *)

type var = string
and lambda = var * term
and term = 
  | TmVar of var
  | TmAbs of lambda
  | TmApp of term * term

(* SYNTAX of CEK machine *)

(* configuration
 triple of a control string(an expression), an environment and continuation)
*)
type config = term * env * cont

(* Closure is pair of a value and environment 
*)
and d = Clo of lambda * env

(* Environment
is implemented as finite maps from variables to closures.
This looks similar to (but different from) substitution in λ-Calculus
*)
and env = (var * d) list

(* Continuation
represent evaluation context.
E ::= [] | E[([] term)] | E[(value [])]
*)

and cont = 
  | Done (* hole *)
  | Ar of term * env * cont
  | Fn of lambda * env * cont






(* type operator *)

type :-> k v = Data.Map.Map k v


(* syntactic sugar *)




(* SEMANTICS of CEK machine *)

(* transition relation for the CEK machine 
as a partial function *)


let step 

variables　

application

abstraction


val step : config -> config = <fun>


(* injection function 
the initial machine state for a closed expression e *)

let inject 

val inject : term -> config = <fun>




    (* auxiliary function for evaluation function *)
collect

(* evaluation function *)
evaluate

(* isFinal *)
isFinal






