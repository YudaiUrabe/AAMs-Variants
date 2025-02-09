(* Reference: 

*)

module StringMap = Map.Make(String)
module AddrMap = Map.Make(Int)

(* Object Langugae -Fine-grain call-by-value lambda calculus- *)
(* ref. An Abstract Machine Semantics for Handlers, Mar 2017, p.6 *)
type var = string
and label = string
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
  | Handle of comp * handler

(* handlers *)
and handler = 
  | ReturnClause of var * comp
  | OperationClause of label * var * string * comp * handler

(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries




(* SYNTAX of time-stamped CESK* machine with handlers*)
(* configuration *)
type config = 







(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries


(* SEMANTICS of time-stamped CESK* machine *)

(* alloc function *)

(* tick function *)


(* transition relation for the time-stamped CESK* machine with handlers *)


(* injection function *)


(* auxiliary functions for evaluation function *)
(* isFinal *)


(* collect *)


(* evaluation function *)
let evaluate (e: term): config list =
  collect step isFinal(inject e)

(* test *)

