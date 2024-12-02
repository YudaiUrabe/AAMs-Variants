(* Reference: "Systematic abstraction of abstract machines" §2.4*)

module StringMap = Map.Make(String)

type var = string
and lambda = var * term
and term = 
  | TmVar of var
  | TmAbs of lambda
  | TmApp of term * term

(* CESK Machine *)
(* Adding a store to CEK machine and use it allocate variable bindings, thereby eliminating recursion from the environment in CEK machine*)

(* SYNTAX of CESK machine *)

(* configuration*)
type config = term * env * store * cont

(* Environment *)
and env = addr StringMap.t
  
and storable = Clo of lambda * env
(* 閉包はないのかしら，いや呼称しないだけかもで，こいつがCEKにおける閉包の代わりな気がする *)

(* store
 is a finite map from address to storable values *)
 and store =  storable StringMap.t
(* これってちゃんとアドレスからのマップだと書けているか？ *)

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

(* transition relation for the CESK machine *)
let step (sigma: config): config = 
  match sigma with
  | (TmVar x, rho, sigma, kappa) ->
    (* I will fix here soon! *)
    let Clo(lam, rho') = List.assoc x rho in (TmAbs lam, rho', sigma, kappa)

| (TmApp (f,e), rho, sigma, kappa) ->
    (f, rho, sigma, Ar(e, rho, kappa))

| (TmAbs lam, rho, sigma, Ar(e, rho', kappa)) ->
    (e, rho', sigma, Fn(lam, rho, kappa)) 
(* 引数を評価するため継続を展開 *)

| (TmAbs lam, rho, sigma, Fn((x, e) , rho', kappa)) -> 
    (e,rho'//[x ==> Clo(lam, rho)], kappa)   (* I will fix here soon. *)
| _ -> failwith "Invalid configuration"

(* alloc function *)
let alloc (sigma: store): addr =
~~~~~~

(* injection function *)
let inject (e:term) : config =
  (e, rho0, sigma0, Done)
~~~~~~

(* isFinal *)
let isFinal (sigma_state: config) : bool =
  match sigma_state with
    |(TmAbs _, _, _, Done) -> true
    | _ -> false

(*　test *)


