(* Object Langugae -fine-graing CbV lambda calculus- *)
(* Reference: "Liberating Effects with Rows and Handlers" *)

module StringMap = Map.Make(String)

(* Object Langugae -Fine-grain call-by-value lambda calculus- *)
type var = string
and lambda = var * term
and term = 
  | TmVar of var
  | TmAbs of lambda

(* computations *)
and comp = 
  | TmApp of term * term
  | Return of term
  | Let of var * comp * comp
  | Do of string * term
  | Handle of comp * handler

(* handlers *)
and handler = 
  | ReturnClause of var * comp
  | OperationalClaus of string * var * string * comp * handler


(* SYNTAX of CEK machine with handlers *)

(* configuration
*)
type config = term * env * cont


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
(* type map = string StringMap.t *)

(* syntactic sugar *)
(* 
let (==>) x y = (x, y)  (* tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries
 *)




(* SEMANTICS of this machine *)

(* transition function 
M-INT, M-APP, M-APPCONT, 
M-LET,M-HANDLER,
M-RETCONT, M-RETHANDLER, M-RETTOP,
M-OP, M-OP-HANDLER, M-OP-FORWARD 
*)


(*
let step (sigma: config): config = 
  match sigma with
  | (TmVar x, rho, kappa) ->
    let Clo(lam, rho') = List.assoc x rho in (TmAbs lam, rho', kappa)
  | (TmApp (f,e), rho, kappa) ->
    (f, rho, Ar(e, rho, kappa))
  | (TmAbs lam, rho, Ar(e, rho', kappa)) ->
    (e, rho', Fn(lam, rho, kappa)) 
  | (TmAbs lam, rho, Fn((x, e) , rho', kappa)) -> 
     (e,rho'//[x ==> Clo(lam, rho)], kappa)
  | _ -> failwith "Invalid configuration"
*)



(*

(* injection function 
the initial machine state for a closed expression e *)
let inject (e:term) : config =
  (e, StringMap.empty, Done)

(* auxiliary functions for evaluation function *)
(* isFinal *)
let isFinal (sigma_state: config) : bool =
  match sigma_state with
    |(TmAbs _, rho, Done) -> true
    | _ -> false

(* collect *)
let rec collect (f: config -> config) (isFinal: config-> bool)(sigma_collect: config): config list =
  if isFinal sigma_collect then
    [sigma_collect]
  else
    sigma_collect :: collect f isFinal (f sigma_collect)

(* evaluation function *)
let evaluate (e: term): config list =
  collect step isFinal(inject e)

(*　test *)


*)
