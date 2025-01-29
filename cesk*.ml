(* Reference: "Systematic abstraction of abstract machines" §2.6 *)

module StringMap = Map.Make(String)
module AddrMap = Map.Make(Int)

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
 and store =  storable AddrMap.t

(* Address *)
and addr = int

(* tests *)




(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries






(* SEMANTICS of CESK* machine *)

(* transition relation for the CESK* machine *)
let step (sigma: config): config = 
  match sigma with
  | (TmVar x, rho, s, kappa) ->
    let Clo(lam, rho') = StringMap.find x rho in
     (TmAbs lam, rho', s, kappa)

  | (TmApp (f,e), rho, s, kappa) ->
      let a' = alloc s in
      let s' = s//[a' ==> Cont kappa] in
      let kappa' = Ar(e, rho, a') in
        (f, rho, s', kappa')

  | (TmAbs lam, rho, s, Ar(e, rho', a')) ->
      (e, rho', s, Fn(lam, rho, a')) 

  | (TmAbs lam, rho, s, Fn((x, e) , rho', a)) -> 
    let Cont kappa = StringMap.find a s in
    let a' = alloc s in 
      (e, rho'//[x ==> a'], s//[a' ==> Clo(lam, rho)], kappa)

  | _ -> failwith "Invalid configuration"


(* alloc function *)
let alloc (s: store): addr =
  let keys = AddrMap.fold (fun key _ acc -> key :: acc) s [] in
  let max_key = List.fold_left max 0 keys in
  max_key + 1
