(* Reference: "Systematic abstraction of abstract machines" §4*)

module StringMap = Map.Make(String)

type var = string
and lambda = var * term
and term = 
  | TmVar of var
  | TmAbs of lambda
  | TmApp of term * term



(* call-by-name Krivine Machine *)

(* SYNTAX of Krivine machine *)

(* configuration*)
type config = term * env * store * cont
  
and storable = 
  | Thunk of term * env
  | Clo of lambda * env

(* store *)
and store =  storable StringMap.t

(* Continuation *)
and cont = 
  | Done
  | C1 of addr * cont
  | C2 of addr * cont

(* Environment *)
and env = addr StringMap.t
  
(* Address *)
and addr = int

(* tests *)






(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries











(* SEMANTICS of Krivine's machine *)

(* transition relation for the Krivine's machine *)
let step (sigma: config): config = 
  match sigma with
  | (TmVar x, rho, s, kappa) ->

  | (TmVar x, rho, s, kappa) ->

| (TmApp (e0,e1), rho, s, kappa) ->

| (TmAbs lam, rho, s, C1(a, kappa)) ->

| (TmAbs (x, e), rho, s, C2(a, kappa)) -> 

| _ -> failwith "Invalid configuration"









(* alloc function *)
let alloc (sigma: store): addr =
  ~~~~~~

(* injection function *)
let inject (e:term) : config =
  (e, rho0, sigma0, Done)
~~~~~~

(* collect *)
let rec collect (f: config -> config) (isFinal: config-> bool)(sigma_collect: config): config list =
  if isFinal sigma_collect then
    [sigma_collect]
  else
    sigma_collect :: collect f isFinal (f sigma_collect)

(* isFinal *)
let isFinal (sigma_state: config) : bool =
  match sigma_state with
    |(TmAbs _, _, _, Done) -> true
    | _ -> false


(* evaluation function *)
let evaluate (e: term): config list =
  collect step isFinal(inject e)
  
(*　test *)

