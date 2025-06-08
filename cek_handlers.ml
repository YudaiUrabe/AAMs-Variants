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


and cont_frame = pure_cont * chi

and pure_cont = 
  | PureContNil
  | PureContCons of pure_cont_frame * pure_cont

and pure_cont_frame = val_env * var * comp

(* Hanlder Closures *)
and chi = val_env * handler



(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)   (* create a tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries
(* Extend a map with a list of key-value pairs in entries.
   Usage: map // entries *)






(* SEMANTICS of this machine *)

(* Identity Continuation *)
let idCont = [([], (StringMap.empty, ReturnClause(x, Return (TmVar x))))]

(* injection function M-INIT
map a computation term into an machine configuration
*)
let inject (m:comp) : config =
  (m, StringMap.empty, idCont)

(* Interpretation function for values *)
let interpret_value (tv: termvalue)(rho: val_env): amvalue =
  match tv with
  | TmVar x -> (
    match StringMap.find_opt x rho with
    | Some v -> v  (* Get the value of x under the environment rho *)
    | None -> failwith ("Unbound variable: " ^ x)  (* Error if the variable is not found in the environment *)
  )
  | TmAbs lam -> AMValClo(rho, lam)





(* transition function *)
(* ref."Liberating Effects with Rows and Handlers", FIg.9 *)
let step (sigma: config): config = 
    match sigma with
    | (TmApp(v,w), rho, kappa) -> 
      let v' = interpret_value v rho in
      let w' = interpret_value w rho in
        (match v' with
        | Clo(rho', (x, m)) -> 
          (m, StringMap.add x w' rho', kappa)  (* M-APP *)
        | Cont(kappa') ->
          (Return(w), rho, kappa' @ kappa)(* M-APPCONT *)
          ) 
    | (Let(x,m,n), rho, (s,chi)::kappa) ->
        (m, rho, ((rho, x, n) :: s, chi) :: kappa) (* M-LET *)
    | (Handle(m, h), rho, kappa) ->
        (m, rho, ([],(rho, h))::kappa) (* M-HANDLE *)

    | (Return v, rho, ((rho', x, n)::s, chi)::kappa) ->
         (n, StringMap.add x (interpret_value v rho) rho', (s,chi)::kappa) (* M-RETCONT *)
    | (Return v, rho, ([],(rho', h))::kappa) ->
        (match h with 
          | {return x -> m} -> 
            (m, StringMap.add x (interpret_value v rho) rho', kappa)  (* M-RETHANDLER *)
          | _ -> failwith "Handle error: invalid handler"
        )
        (* 
        | (Return v, rho, []) ->
        interpret_value v rho (* M-RETTOP *)
        *)
    | (Do(l, v), rho, kappa) ->
       (Do(l, v), rho, kappa, []) (* M-OP *)
    | (Do(l, tv), rho, (s, (rho', h))::kappa, kappa') ->
      match h with 
      | {l x k -> m} -> 
           let updated_rho = StringMap.add x (interpret_value v rho) rho' in
        (m, updated_rho, kappa) (* M-OP-HANDLE *)
      | _ -> 
          (Do(l, v), rho, kappa, kappa' @ [(s, (rho', h))]) (* M-OP-FORWARD *)







(* auxiliary functions for evaluation function *)
(* isFinal 
A state is final when it has no next step.
This function checks if the term is a value and the continuation is empty.
*)
let isFinal (s: config) : bool =
  match s with
    |(Return _, _rho, Done) -> true
    | _ -> false



(* collect *)
let rec collect (f: config -> config) (isFinal: config-> bool)(state: config): config list =
  if isFinal state then
    [state]
  else
    state :: collect f isFinal (f state)

(* evaluation function *)
(* Create an initial state from the term e using "inject",
then apply "step" repeatedly until the final state is reached, saving all intermediate states in a list.*)
let evaluate (m: comp): config list =
  collect step isFinal(inject m)


(* 

(* tests *)
(* auxiliary function for the tests *)

(* to string *)
let rec string_of_term (m: comp): string  =
  match m with    
  | TmVar x -> x
  | TmAbs(x, body) -> "(λ" ^ x ^ "." ^ string_of_term body ^ ")"
  | TmApp(v1, v2) -> "(" ^ string_of_term v1 ^ " " ^ string_of_term v2 ^ ")"
  | Return(v) ->
  | Let(x,m,n) -> 
  | Do(l, v) -> 
  | Handling(m,h) -> 
