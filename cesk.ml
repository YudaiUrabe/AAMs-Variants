(* Reference: "Systematic abstraction of abstract machines" §2.4*)

module StringMap = Map.Make(String)
module AddrMap = Map.Make(Int)

type var = string
and lambda = var * term
and term = 
  | TmVar of var
  | TmAbs of lambda
  | TmApp of term * term

(* CESK Machine *)
(* Adding a store to CEK machine and use it allocate variable bindings,
 thereby eliminating recursion from the environment in CEK machine*)

(* SYNTAX of CESK machine *)

(* configuration*)
type config = term * env * store * cont

(* Environment *)
and env = addr StringMap.t

and storable = Clo of lambda * env

(* store
 is a finite map from address to storable values *)
 and store =  storable AddrMap.t

(* Continuation *)
and cont = 
  | Done
  | Ar of term * env * cont
  | Fn of lambda * env * cont

(* Address *)
and addr = int

(* tests *)



(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries








(* SEMANTICS of CESK machine *)

(* alloc function *)
(* Extract all keys from s, find the maximum key in the list,
 and return the maximum key plus one as the new address. *)
 let alloc (s: store): addr =
  let keys = AddrMap.fold (fun key _ acc -> key :: acc) s [] in
  let max_key = List.fold_left max 0 keys in
  max_key + 1

(* transition relation for the CESK machine *)
let step (sigma: config): config = 
  match sigma with
  | (TmVar x, rho, sigma_store, kappa) ->
    let addr = StringMap.find x rho in (* (ρ!x) *)
    let Clo(lam, rho') = AddrMap.find addr sigma_store in (TmAbs lam, rho', sigma_store, kappa)

  | (TmApp (f,e), rho, sigma_store, kappa) ->
      (f, rho, sigma_store, Ar(e, rho, kappa))

  | (TmAbs lam, rho, sigma_store, Ar(e, rho', kappa)) ->
      (e, rho', sigma_store, Fn(lam, rho, kappa)) 


  | (TmAbs lam, rho, sigma_store, Fn((x, e) , rho', kappa)) -> 
      let a' = alloc sigma_store in (e, rho'//[x ==> a'], sigma_store//[a' ==> Clo(lam, rho)], kappa)
  | _ -> failwith "Invalid configuration"


(* injection function 
combines the term with the empty env, store and continuation*)
let inject (e:term) : config =
  let (rho0, s0) = (StringMap.empty, StringMap.empty) in (e, rho0, s0, Done)

(* isFinal *)
let isFinal (state: config) : bool =
  match state with
    |(TmAbs _, _, _, Done) -> true
    | _ -> false

