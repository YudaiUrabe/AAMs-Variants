(* Object Langugae -untyped lambda calculus(CbV)- *)






type var = string
and Lambda = var * term

and term = 
  | TmVar of var
  | TmAbs of Lambda
  | TmApp of term * term



(* SYNTAX of CEK machine *)

(* configuration
 triple of a control string(an expression), an environment and continuation)
*)
and Σ = term * Env * Cont

(* Closure is pair of a value and environment 
useful to implement lambda abstraction *)

and d = Clo of Lambda * Env

(* Environment
is implemented as finite maps from variables to closures.
This may be an alternative to substitution in λ-Calculus
*)

and Env = TmVar * d

(* Continuation
represent evaluation context.
E ::= [] | E[([] term)] | E[(value [])]
*)


type Cont = 
  | Done (* hole *)
  | Ar of term * Env * Cont
  | Fn of Lambda * Env * Cont


(* tests *)




(* type operator *)

type :-> k v = Data.Map.Map k v


(* syntactic sugar *)

~~~


(* SEMANTICS of CEK machine *)

(* transition relation for the CEK machine 
as a partial function *)


let step 

variables　

application

abstraction


val step : Σ -> Σ = <fun>


(* injection function 
the initial machine state for a closed expression e *)

let inject 

val inject : term -> Σ = <fun>




    (* auxiliary function for evaluation function *)
collect

(* evaluation function *)
evaluate

(* isFinal *)
isFinal

