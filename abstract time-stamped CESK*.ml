(* Reference: "Systematic abstraction of abstract machines" §2.9 *)

module StringMap = Map.Make(String)
module AddrMap = Map.Make(Int)

(* Object Langugae -untyped lambda calculus(CbV)- *)
type var = string
and lambda = var * term
and term = 
  | TmVar of var
  | TmAbs of lambda
  | TmApp of term * term


(* SYNTAX of abstract time-stamped CESK* machine *)

(* configuration*)
type config = term * env * store * cont * time

and storable = 
  | Clo of lambda * env
  | Cont of cont


(* Environment *)
and env = addr StringMap.t

(* store *)
and store =  


(* Continuation *)
and cont = 
  | Done
  | Ar of term * env * addr
  | Fn of lambda * env * addr

(* Time *)
and time = int


(* Address *)
and addr = int



(* type P *)



(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries


(* SEMANTICS of abstract time-stamped CESK* machine *)


(* Lattice *)




(* alloc function *)

(* tick function *)

(* transition relation *)
let step (sigma: config): ~~~~ = 
  match sigma with
  | (TmVar x, rho, s, kappa, t) ->

  | (TmApp (f,e), rho, s, kappa, t) ->

  | (TmAbs lam, rho, s, Ar(e, rho', a'), t) ->

  | (TmAbs lam, rho, s, Fn((x, e) , rho', a), t) -> 

  | _ -> failwith "Invalid configuration"






(* aval *)

(* explore *)

(* ∈ *)

(* search *)

(* injection function *)
let inject (e:term) : config =
  (e, StringMap.empty, AddrMap.empty, Done, 0)

(* test *)

