(* Reference: "Systematic abstraction of abstract machines" §2.8 *)

module StringMap = Map.Make(String)

(* Object Langugae -untyped lambda calculus(CbV)- *)
type var = string
and lambda = var * term
and term = 
  | TmVar of var
  | TmAbs of lambda
  | TmApp of term * term



(* time-stamped CESK* machine *)

(* SYNTAX of time-stamped CESK* machine *)

(* configuration*)
type config = term * env * store * cont * time

and storable = 
  | Clo of lambda * env
  | Cont of cont


(* Environment *)
and env = addr StringMap.t

(* store *)
and store =  storable StringMap.t


(* Continuation *)
and cont = 
  | Done
  | Ar of term * env * addr
  | Fn of lambda * env * addr

(* Address *)
and addr = int

(* Time *)
and time = int

(* tests *)


(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries






(* SEMANTICS of time-stamped CESK* machine *)

(* alloc function *)
let alloc (sigma: config): addr =
  ~~~~~~
  
(* tick function *)
let tick (sigma: config): time =
  ~~~~~~

(* transition relation for the time-stamped CESK* machine *)

(* I need to fix all patterns *)

let step (sigma: config): config = 
  match sigma with
  | (TmVar x, rho, sigma, kappa, t) ->
    (* I will fix here soon! *)
    let Clo(lam, rho') = List.assoc x rho in (TmAbs lam, rho', sigma, kappa, t')

| (TmApp (f,e), rho, sigma, kappa, t) ->
    (f, rho, sigma', kappa', t')
    (* I will add some here. *)

| (TmAbs lam, rho, sigma, Ar(e, rho', a', t)) ->
    (e, rho', sigma, Fn(lam, rho, a', t')) 

| (TmAbs lam, rho, sigma, Fn((x, e) , rho', a), t) -> 
    (e,rho'//[x ==> Clo(lam, rho)], kappa, t')   (* I will fix here soon. *)

| _ -> failwith "Invalid configuration"





