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

and cont' = 
  | ContNil'
  | ContCons' of cont_frame * cont'


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





(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)   (* create a tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries
(* Extend a map with a list of key-value pairs in entries.
   Usage: map // entries *)


(* SEMANTICS of this machine *)
(* Interpretation function for values *)
let interpret_value (tv: termvalue)(rho: val_env): amvalue =
  match tv with
  | TmVar x -> (
    match StringMap.find_opt x rho with
    | Some v -> v  (* Get the value of x under the environment rho *)
    | None -> failwith ("Unbound variable: " ^ x)  (* Error if the variable is not found in the environment *)
  )
  | TmAbs lam -> AMValClo(rho, lam)


(* k1 ++ k2: Concatenates two continuations by chaining k2 to the end of k1, like @ for OCaml lists *)
  let rec cont_append (k1 : cont) (k2 : cont) : cont =
    match k1 with
    | ContNil -> k2
    | ContCons (frame, rest) -> ContCons (frame, cont_append rest k2)

  let rec cont'_append (k1 : cont') (k2 : cont') : cont' =
    match k1 with
    | ContNil' -> k2
    | ContCons' (frame, rest) -> ContCons' (frame, cont'_append rest k2)

(* Convert cont' to cont *)
let rec cont'_to_cont (k : cont') : cont =
  match k with
  | ContNil' -> ContNil
  | ContCons'(frame, rest) -> ContCons(frame, cont'_to_cont rest)


(* injection function M-INIT
map a computation term into an machine configuration
*)
let inject (m:comp) : config =
  Config1(m, StringMap.empty, ContNil)

(* transition function *)
(* ref."Liberating Effects with Rows and Handlers", FIg.9 *)
let step (sigma: config): config = 
  match sigma with
    |Config1(m, rho, kappa) -> 
      (match m with
      | TmApp(v,w)->
        let v' = interpret_value v rho in
        let w' = interpret_value w rho in
        (match v' with
        | AMValClo(rho', (x, m)) -> 
          Config1(m, StringMap.add x w' rho', kappa)  (* M-APP *)
        | AMValCont(kappa') ->
          Config1(Return(w), rho, cont_append kappa' kappa)(* M-APPCONT(M-RESUME) *)
        )   
      |Let(x,m1,m2)->
        (match kappa with
          | ContCons((s,chi),kappa')->
            Config1(m1, rho, ContCons((PureContCons((rho, x, m2) , s), chi) , kappa')) (* M-LET *)
          | ContNil -> 
              failwith "Let without continuation"
        )
      | Handling(m1, h) ->
            Config1(m1, rho, ContCons((PureContNil,(rho, h)), kappa)) (* M-HANDLE *)
      | Return(v) ->
        (match kappa with
          | ContCons((PureContCons((rho', x, m2), s), chi), kappa') ->
            Config1(m2, StringMap.add x (interpret_value v rho) rho', ContCons((s,chi), kappa')) (* M-RETCONT(M-PURE-CONT) *)
          | ContCons((PureContNil,(rho', h)), kappa')->
            (match h with 
              | ReturnClause(x, m_ret) -> 
                  Config1(m_ret, StringMap.add x (interpret_value v rho) rho', kappa')  (* M-RETHANDLER(M-GET-CONT) *)
              | OperationClause _ -> 
                  failwith "Handler must be ReturnClause"
            )
        )

      | Do(l, v) ->
        Config2(Do(l, v), rho, kappa, ContNil') (* M-OP(M-Doδ) *)
      )
    | Config2(m, rho, kappa, kappa') ->      
      (match m with
      | Do(l, v) ->  
        (match kappa with 
        | ContCons((s, (rho', h)), kappa)->
          (match h with 
            | OperationClause(l', x, k, m, h') when l = l'-> 

                let v_val = interpret_value v rho in
                let cont_val = AMValCont (cont_append (cont'_to_cont kappa') (ContCons((s, (rho', h)),ContNil))) in
                let updated_rho = StringMap.add x v_val (StringMap.add k cont_val rho') in
                Config1(m, updated_rho, kappa) (* M-OP-HANDLE(M-Do†) *)
            | OperationClause _ ->
                Config2(Do(l, v), rho, kappa, cont'_append kappa' (ContCons'((s, (rho', h)), ContNil'))) (* M-OP-FORWARD(1)(M-FORWARD) *)
            | ReturnClause _ ->
                Config2(Do(l, v), rho, kappa, cont'_append kappa' (ContCons'((s, (rho', h)), ContNil'))) (* M-OP-FORWARD(2)(M-FORWARD) *)
          )
        | ContNil->
           failwith "Unexpected ContNil in Config2"
        )  
      | _ -> failwith "Invalid computation in Config2"
      )












(* auxiliary functions for evaluation function *)
(* isFinal 
A state is final when it has no next step.
This function checks if the term is a value and the continuation is empty.
*)
let isFinal (s: config) : bool =
  match s with
    |Config1(Return _, _, ContNil) -> true
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


