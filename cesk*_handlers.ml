(* Reference: "Liberating Effects with Rows and Handlers",
"Foundations for Programming and Implementing Effect Handlers"(2021) *)

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




(* SYNTAX of CESK* machine with handlers *)
(* configuration *)
type config = 
  | comp * val_env * store * cont
  | comp * val_env * store * cont * cont' (* Augmented the configuration space *)

(* Function Closures *)
and d = Clo of val_env * lambda

(* Values *)
and AMvalue = 
  | d
  | cont

(* Value environments *)
and val_env = AMvalue StringMap.t


and storable = 
  | AMVal of AMvalue
  | PureCont of pure_cont


(* store*)
 and store =  storable AddrMap.t




(* (Captured) Continuations *)
and cont = 
  | Done
  | cont_frame :: addr


and cont_frame = (pure_cont, chi)

and pure_cont = 
  | Done
  | pure_cont_frame :: addr


and pure_cont_frame = (val_env, x, comp)

(* Hanlder Closures *)
and chi = (val_env, handler)

(* Address *)
and addr = int











(* SEMANTICS of this machine *)

(* Identity Continuation *)
let idCont = 

(* injection function M-INIT 
map a computation term into an machine configuration
*)
let inject (m:comp) : config =
  (m, StringMap.empty, AddrMap.empty, 0)



(* Interpretation function for values *)




(* transition function *)
(* ref."Liberating Effects with Rows and Handlers", FIg.9 *)
let step (sigma: config): config = 
    match sigma with
    | (TmApp(v,w), rho, kappa) -> 

 (* M-APP *)
 (* M-APPCONT *)
 (* M-LET *)
 (* M-HANDLE *)
 (* M-RETCONT *)
 (* M-RETHANDLER *)
(* M-RETTOP *)
  (* M-OP *)
  (* M-OP-HANDLE *)
 (* M-OP-FORWARD *)
(* ~~~ *)







(* isFinal *)
let isFinal (sigma_state: config) : bool =
  match sigma_state with
    |(Return _, rho, s, Done) -> true
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