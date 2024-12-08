(* Object Langugae -fine-graing CbV lambda calculus- *)
(* Reference: "Liberating Effects with Rows and Handlers" *)

module StringMap = Map.Make(String)

(* Object Langugae -Fine-grain call-by-value lambda calculus- *)
(* ref. An Abstract Machine Semantics for Handlers, Mar 2017, p.6 *)
type var = string
and label = string
and lambda = var * term
and term = 
  | TmVar of var
  | TmAbs of lambda

(* computations *)
and comp = 
  | TmApp of term * term
  | Return of term
  | Let of var * comp * comp
  | Do of label * term
  | Handle of comp * handler

(* handlers *)
and handler = 
  | ReturnClause of var * comp
  | OperationClause of label * var * string * comp * handler

(* SYNTAX of CEK machine with handlers *)
(* ref. An Abstract Machine Semantics for Handlers, Mar 2017, p.18 *)

(* configuration
*)
type config = 
  | term * env * cont
  | term * env * cont * cont' (* Augmented the configuration space of CEK *)

(* Closure is pair of a value and environment 
*)
(* and d = Clo of lambda * env *)

(* Environment
*)
and env = d StringMap.t






(* Continuation*)

and cont = 
  | Done (* hole *)
  | Ar of term * env * cont
  | Fn of lambda * env * cont

(* Hanlder Closures　*)






  (* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries



(* SEMANTICS of this machine *)

(* IdentityContinuation *)
ilet identityContinuation ~~~~~~

(* injection function M-INIT *)
let inject (e:term) : config =
  (e, StringMap.empty, identityContinuation)

(* transition function *)
(* ref."Liberating Effects with Rows and Handlers", FIg.9 *)
let step (sigma: config): config = 
    match sigma with
    |    (* M-APP *)

(* M-APPCONT *)

(* M-LET *)

(* M-HANDLE *)


(* M-RETHANDLER *)


(* M-OP *)

(* M-OP-HANDLE *)


(* M-OP-FORWARD *)




(*　test *)



