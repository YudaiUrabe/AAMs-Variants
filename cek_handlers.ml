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



(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries




(* SYNTAX of CEK machine with handlers *)
(* ref. An Abstract Machine Semantics for Handlers, Mar 2017, p.18 *)

(* configuration *)
type config = 
  | comp * val_env * cont
  | comp * val_env * cont * cont' (* Augmented the configuration space of CEK *)

(* Function Closures *)
and d = Clo of val_env * lambda


(* Values *)
and cekvalue = 
  | d
  | cont

(* Value environments *)
and val_env =
  | StringMap.empty
  | val_env // [x ==> cekvalue]


(* (Captured) Continuations *)
and cont = 
  | Done
  | cont_frame :: cont

and cont_frame = (pure_cont, chi)

and pure_cont = 
  | Done
  | pure_cont_frame :: pure_cont

and pure_cont_frame = (val_env, x, comp)


(* Hanlder Closures *)
and chi = (val_env, handler)






















(* SEMANTICS of this machine *)

(* Identity Continuation *)
let idCont = [([], (StringMap.empty, ReturnClause(x, Return (TmVar x))))]


(* injection function M-INIT 
map a computation term into an machine configuration
*)
let inject (m:comp) : config =
  (m, StringMap.empty, idCont)




(* Interpretation function for values *)
let interpret_value (tv: termvalue)(rho: val_env): cekvalue =
  match tv with
  | TmVar x -> (
    match StringMap.find_opt x rho with
    | Some v -> v  (* Get the value of x under the environment rho *)
    | None -> failwith ("Unbound variable: " ^ x)  (* Error if the variable is not found in the environment *)
  )
  | TmAbs lam -> Clo(rho, lam)



(* transition function *)
(* ref."Liberating Effects with Rows and Handlers", FIg.9 *)
let step (sigma: config): config = 
    match sigma with
    | (TmApp(v,w), rho, kappa) -> 
      let v' = interpret_value v rho in
      let w' = interpret_value w rho in
        (match v' with
        | Clo(rho', (x, M)) -> 
          (M, StringMap.add x w' rho', kappa)  (* M-APP *)
        | _ -> failwith "Application error: not a closure")
    | (TmApp(v,w), rho, kappa) ->
      let kappa' = interpret_value v rho in 
        (Return(w), rho, kappa' @ kappa) (* M-APPCONT *)
    


    | (Let(x,M,N), rho, (s,chi)::kappa) ->
        (M, rho, ((rho, x, N) :: s, chi) :: kappa) (* M-LET *)
    | (Handle(M, H), rho, kappa) ->
        (M, rho, ([],(rho, H))::kappa) (* M-HANDLE *)

    | (Return V, rho, ((rho', x, N)::s, chi)::kappa) ->
         (N, StringMap.add x (interpret_value V rho) rho', (s,chi)::kappa) (* M-RETCONT *)
    | (Return V, rho, ([],(rho', H))::kappa) ->
        (match H with 
          | {return x -> M} -> 
            (M, StringMap.add x (interpret_value V rho) rho', kappa)  (* M-RETHANDLER *)
          | _ -> failwith "Handle error: invalid handler"
        )
    | (Return V, rho, []) ->
        interpret_value V rho (* M-RETTOP *)



    | (Do(l, V), rho, kappa) ->
       (Do(l, V), rho, kappa, []) (* M-OP *)

    | (Do(l, tv), rho, (s, (rho', H))::kappa, kappa') ->
      match H with 
      | {l x k -> M} -> 
           let updated_rho = StringMap.add x (interpret_value V rho) rho' in
        (M, updated_rho, kappa) (* M-OP-HANDLE *)
      | _ -> 
          (Do(l, V), rho, kappa, kappa' @ [(s, (rho', H))]) (* M-OP-FORWARD *)



(* isFinal *)
let isFinal (sigma_state: config) : bool =
  match sigma_state with
    |(Return _, rho, Done) -> true
    | _ -> false



(* collect *)
let rec collect (f: config -> config) (isFinal: config-> bool)(sigma_collect: config): config list =
  if isFinal sigma_collect then
    [sigma_collect]
  else
    sigma_collect :: collect f isFinal (f sigma_collect)


(* evaluation function *)
let evaluate (M: comp): config list =
  collect step isFinal(inject M)


(*　test *)
let term_test =

let result = evaluate term_test

(* auxiliary function *)
let rec string_of_term (M: comp) =
  match M with
  |~~~~~

(* output *)
let () = 
  List.iter (fun (term_test, _, _) -> 
    Printf.printf "State: %s\n" (string_of_term term_test)
  ) result


