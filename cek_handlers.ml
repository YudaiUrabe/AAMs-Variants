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
  | Handling of comp * handler

(* handlers *)
and handler = 
  | ReturnClause of var * comp
  | OperationClause of label * var * string * comp * handler









(* SYNTAX of CEK machine with handlers *)
(* ref. An Abstract Machine Semantics for Handlers, Mar 2017, p.18 *)

(* configuration *)
type config = 
  | Config1 of comp * val_env * cont
  | Config2 of comp * val_env * cont * cont' (* Augmented the configuration space of CEK *)

(* Values *)
and amvalue = 
  | FuncClo of val_env * lambda
  | AMvalCont of cont

(* Value environments *)
and val_env = amvalue StringMap.t


(* (Captured) Continuations *)
and cont = 
  | Done
  | cont_frame :: cont
and cont_frame = pure_cont * chi

and pure_cont = 
  | Done
  | pure_cont_frame :: pure_cont

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
let interpret_value (tv: termvalue)(rho: val_env): AMvalue =
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
        | Cont(kappa') ->
          (Return(w), rho, kappa' @ kappa) (* M-APPCONT *)
        | _ -> failwith "Application error: not a closure or continuation")

    

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
let evaluate (M: comp): config list =
  collect step isFinal(inject M)




(* tests *)
(* auxiliary function for the tests *)

(* to string *)
let rec string_of_term (M: comp): string  =
  match t with
  | TmVar x -> x
  | TmNum n -> string_of_int n
  | TmAbs(x, body) -> "(λ" ^ x ^ "." ^ string_of_term body ^ ")"
  | TmApp(e1, e2) -> "(" ^ string_of_term e1 ^ " " ^ string_of_term e2 ^ ")"
  | TmAdd(e1, e2) -> "(" ^ string_of_term e1 ^ " + " ^ string_of_term e2 ^ ")"
  | TmMul(e1, e2) -> "(" ^ string_of_term e1 ^ " * " ^ string_of_term e2 ^ ")"

let string_of_state (s: state) : string =
  match s with
  | (TmNum n, _, Done) -> string_of_int n
  | (TmAbs(_, _) as abs, _, Done) -> string_of_term abs
  | _ -> "<non-final state>"



  (* test1 
  (λa.a)(λb.b) -> (λb.b) *)
  let term_test1 = TmApp (TmAbs ("a", TmVar "a"), TmAbs ("b", TmVar "b"))

(* test2
suc = λnsz.s(nsz)
1 = λsz.sz
suc 1 -> λsz.s(sz) = 2
 *)
 let term_test2 = TmApp(
                    TmAbs("n", TmAbs ("s",TmAbs ("z", TmApp (TmVar "s", TmApp(TmApp(TmVar "n", TmVar "s"), TmVar "z"))))),
                    TmAbs ("s", TmAbs ("z", TmApp (TmVar "s", TmVar "z"))))

(* test3
(λx.λy.x) (λz.z) (λw.w) ->* λz.z
*)
 let term_test3 = TmApp(
  TmApp(TmAbs("x", TmAbs("y", TmVar "x")), TmAbs("z", TmVar "z")), 
  TmAbs("w", TmVar "w"))

(* test4

*)



  (* output *)
  let () =
  let result1 = evaluate term_test1 in
  let result2 = evaluate term_test2 in
  let result3 = evaluate(term_test3) in
  let result4 = evaluate(term_test4) in
  print_endline ("test1 result: " ^ string_of_state result1);
  print_endline ("test2 result: " ^ string_of_state result2);
  print_endline ("test3 result: " ^ string_of_state result3);
  print_endline ("test4 result: " ^ string_of_state result4)
  

    (* check the correctness *)
    print_endline "Testing term_test1...";
    assert~~~~~~~~




