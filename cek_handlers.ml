(* Object Langugae -fine-graing CbV lambda calculus- *)
(* Reference: "Liberating Effects with Rows and Handlers" *)

type var = string
and lambda = var * term
and term = 
  | TmVar of var
  | TmAbs of lambda

(* computations *)

(* handlers *)


(* SYNTAX of CEK machine with handlers *)

(* configuration*)
(* type config = term * env * cont *)

(* Environment *)
(* and env = (var * addr) list *)


(* Closure *)
(* and d = Clo of env * handler)

(* Continuation*)
(* and cont = 
  | Done (* hole *)
  | Ar of term * env * cont
  | Fn of lambda * env * cont *)





(* SEMANTICS of this machine *)



(* Identity continuation *)

(* transition relation 
M-INT, M-APP, M-APPCONT, 
M-LET,M-HANDLER,
M-RETCONT, M-RETHANDLER, M-RETTOP,
M-OP, M-OP-HANDLER, M-OP-FORWARD 
*)




(* Value interpretation *)

