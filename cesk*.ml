(* Reference: "Systematic abstraction of abstract machines" §2.6 *)

module StringMap = Map.Make(String)

(* Object Langugae -untyped lambda calculus(CbV)- *)

type var = string
and lambda = var * term
and term = 
  | TmVar of var
  | TmAbs of lambda
  | TmApp of term * term



(* CESK* machine
eliminate recursion from continuation in CESK machine
*)


(* SYNTAX of CESK* machine *)

(* configuration*)
type config = term * env * store * cont

(* Continuation *)
and cont = 
  | Done
  | Ar of term * env * addr
  | Fn of lambda * env * addr

and storable = 
  | Clo of lambda * env
  | Cont of cont

(* Environment *)
and env = addr StringMap.t

(* store *)
 and store =  storable StringMap.t


(* Address *)
and addr = int

(* tests *)


(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries




(* SEMANTICS of CESK* machine *)

(* transition relation for the CESK machine *)
let step (sigma: config): config = 
  match sigma with
  | (TmVar x, rho, sigma, kappa) ->
    (* I will fix here soon! *)
    let Clo(lam, rho') = List.assoc x rho in (TmAbs lam, rho', sigma, kappa)

| (TmApp (f,e), rho, sigma, kappa) ->
    (f, rho, sigma', kappa')
    (* I will add some here. *)

| (TmAbs lam, rho, sigma, Ar(e, rho', a')) ->
    (e, rho', sigma, Fn(lam, rho, a')) 

| (TmAbs lam, rho, sigma, Fn((x, e) , rho', a)) -> 
    (e,rho'//[x ==> Clo(lam, rho)], kappa)   (* I will fix here soon. *)

| _ -> failwith "Invalid configuration"

(* alloc function *)
let alloc (sigma: store): addr =
~~~~~~



(*　test *)