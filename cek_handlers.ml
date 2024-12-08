(* Object Langugae -fine-graing CbV lambda calculus- *)
(* Reference: "Liberating Effects with Rows and Handlers",
"Foundations for Programming and Implementing Effect Handlers"(2021) *)

module StringMap = Map.Make(String)

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





(* SYNTAX of CEK machine with handlers *)
(* ref. An Abstract Machine Semantics for Handlers, Mar 2017, p.18 *)

(* configuration *)
type config = 
  | comp * val_env * cont
  | comp * val_env * cont * cont' (* Augmented the configuration space of CEK *)










(* Values *)
and cekvalue = 

(* Value environments *)
and val_env =

(* Closure *)
and d = Clo of lambda * env
















(* Continuation*)

and cont = 
  | Done (* hole *)
  | Ar of term * env * cont
  | Fn of lambda * env * cont





and cont' =














(* Hanlder Closures　*)
and chi = Clo of 








  (* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries















(* SEMANTICS of this machine *)

(* IdentityContinuation *)
let idCont = [([], (StringMap.empty, ))]


(* injection function M-INIT 
map a computation term into an machine conficuration
*)
let inject (m:comp) : config =
  (m, StringMap.empty, idCont)





(* Interpretation function for values *)
let initerpret_value (tv: termvalue)(rho: val_env): cekvalue =
  match tv with
  | (TmVar x, rho) -> 
  | (TmAbs lam, rho) -> 












(* transition function *)
(* ref."Liberating Effects with Rows and Handlers", FIg.9 *)
let step (sigma: config): config = 
    match sigma with
    | (TmApp(v,w), rho, kappa) -> 
      () (* M-APP *)

    | (TmApp(v,w), rho, kappa) ->
      () (* M-APPCONT *)

    | (Let(x,M,N), rho, ) ->
      () (* M-LET *)
    | (Handle(M, H), rho, kappa) ->
      (M, rho, ) (* M-HANDLE *)






    | () ->
      () (* M-RETCONT *)
    | () ->
      () (* M-RETHANDLER *)
    | () ->
      () (* M-RETTOP *)


    | (Do(l, V), rho, kappa) ->
      (Do(l, V), rho, kappa, []) (* M-OP *)
    | (Do(l, tv), rho, , kappa') ->
      ()  (* M-OP-HANDLE *) 
    | (Do(l, tv), rho, , kappa') ->
      ()  (* M-OP-FORWARD *) 












(*　test *)



