(* Reference: "Liberating Effects with Rows and Handlers",
"Foundations for Programming and Implementing Effect Handlers"(2021) *)

module StringMap = Map.Make(String)

(* Object Langugae -Fine-grain call-by-value lambda calculus- *)
(* ref. An Abstract Machine Semantics for Handlers, Mar 2017, p.6 *)
type var = string
and label = string
and var_cont = string

and lambda = var * comp
and termvalue = 
  | TmVar of var
  | TmAbs of lambda

(* computations *)
and comp = 
  | TmApp of termvalue * termvalue
  | Return of termvalue
  | Let of var * comp * comp
  | Do of label * termvalue
  | Handling of comp * handler

(* handlers *)
and handler = 
  | ReturnClause of var * comp
  | OperationClause of label * var * var_cont * comp * handler




(* SYNTAX of CEK machine with handlers *)
(* ref. An Abstract Machine Semantics for Handlers, Mar 2017, p.18 *)

(* configuration *)
type config = 
  | Config1 of comp * val_env * cont
  | Config2 of comp * val_env * cont * cont' (* Augmented the configuration space of CEK *)

(* Values *)
and amvalue = 
  | AMValClo of val_env * lambda
  | AMValCont of cont

(* Value environments *)
and val_env = amvalue StringMap.t

(* (Captured) Continuations *)
and cont = 
  | ContNil
  | ContCons of cont_frame * cont

(* Continuations frames *)
and cont_frame = pure_cont * chi

(* Pure continuations *)
and pure_cont = 
  | PureContNil
  | PureContCons of pure_cont_frame * pure_cont

(* Pure continuations frames *)
and pure_cont_frame = val_env * var * comp


(* Hanlder Closures *)
and chi = val_env * handler

