(* Reference: "Systematic abstraction of abstract machines" §2.8 *)

module StringMap = Map.Make(String)
module AddrMap = Map.Make(Int)

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
and store =  storable AddrMap.t

(* Continuation *)
and cont = 
  | Done
  | Ar of term * env * addr
  | Fn of lambda * env * addr

(* Address *)
and addr = int

(* Time *)
and time = int



(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries






(* SEMANTICS of time-stamped CESK* machine *)

(* alloc function *)
let alloc (sigma: config): addr =
    let (_, _, s, _, _) = sigma in
    let keys = AddrMap.fold (fun key _ acc -> key :: acc) s [] in
    let max_key = List.fold_left max 0 keys in
    max_key + 1
  
(* tick function *)
let tick (sigma: config): time =
  let (_, _, _, _, t) = sigma in
  t + 1


(* transition relation for the time-stamped CESK* machine *)
let step (sigma: config): config = 
  match sigma with
  | (TmVar x, rho, s, kappa, t) ->
    let Clo(lam, rho') =  StringMap.find x rho in
    let t' = tick sigma in
      (TmAbs lam, rho', s, kappa, t')

  | (TmApp (f,e), rho, s, kappa, t) ->
    let a' = alloc sigma in
    let s' = s//[a' ==> Cont kappa] in
    let kappa' = Ar(e, rho, a') in
    let t' = tick sigma in
      (f, rho, s', kappa', t')

  | (TmAbs lam, rho, s, Ar(e, rho', a'), t) ->
    let t' = tick sigma in
      (e, rho', s, Fn(lam, rho, a'), t') 

  | (TmAbs lam, rho, s, Fn((x, e) , rho', a), t) -> 
    let Cont kappa = StringMap.find a s in
    let a' = alloc sigma in
    let t' = tick sigma in
      (e, rho'//[x ==> a'], s//[a' ==> Clo(lam, rho)], kappa, t')

  | _ -> failwith "Invalid configuration"



(* injection function *)
let inject (e:term) : config =
  (e, StringMap.empty, AddrMap.empty, Done, 0)


(* auxiliary functions for evaluation function *)
(* isFinal *)


(* collect *)


(* evaluation function *)
let evaluate (e: term): config list =
  collect step isFinal(inject e)

(* test *)
