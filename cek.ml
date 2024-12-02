(* Reference: "Systematic abstraction of abstract machines" §2.2*)

module StringMap = Map.Make(String)

(* Object Langugae -untyped lambda calculus(CbV)- *)

type var = string
and lambda = var * term
and term = 
  | TmVar of var
  | TmAbs of lambda
  | TmApp of term * term

(* SYNTAX of CEK machine *)

(* configuration
 triple of a control string(an expression), an environment and continuation)
*)
type config = term * env * cont

(* Closure is pair of a value and environment 
*)
and d = Clo of lambda * env

(* Environment
is implemented as finite maps from variables to closures.
*)
and env = d StringMap.t

(* Continuation
represent evaluation context.
E ::= [] | E[([] term)] | E[(value [])]
*)

and cont = 
  | Done (* hole *)
  | Ar of term * env * cont
  | Fn of lambda * env * cont

(* tests *)
let ex_env1 : env = StringMap.add "a"(Clo(("b", TmVar"b"),(StringMap.empty))) StringMap.empty

let ex_term : term = TmApp(TmVar "a", TmVar "b")

let test_clo : d = Clo (("c", TmVar "c"), ex_env1)

let test_cont : cont = Fn (("c", TmVar "c"), ex_env1, Done)

(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries





(* SEMANTICS of CEK machine *)

(* transition relation for the CEK machine 
as a partial function 
one-step
*)


let step (sigma: config): config = 
  match sigma with
  | (TmVar x, rho, kappa) ->
    let Clo(lam, rho') = List.assoc x rho in (TmAbs lam, rho', kappa)
(* I will fix here *)
| (TmApp (f,e), rho, kappa) ->
    (f, rho, Ar(e, rho, kappa))


| (TmAbs lam, rho, Ar(e, rho', kappa)) ->
    (e, rho', Fn(lam, rho, kappa)) 


| (TmAbs lam, rho, Fn((x, e) , rho', kappa)) -> 
    (e,rho'//[x ==> Clo(lam, rho)], kappa)

| _ -> failwith "Invalid configuration" (* exception*)





(* injection function 
the initial machine state for a closed expression e *)
let inject (e:term) : config =
  (e, StringMap.empty, Done)

(* auxiliary functions for evaluation function *)
(* isFinal *)
let isFinal (sigma_state: config) : bool =
  match sigma_state with
    |(TmAbs _, rho, Done) -> true
    | _ -> false

(* collect *)
let rec collect (f: config -> config) (isFinal: config-> bool)(sigma_collect: config): config list =
  if isFinal sigma_collect then
    [sigma_collect]
  else
    sigma_collect :: collect f isFinal (f sigma_collect)



(* evaluation function *)
let evaluate (e: term): config list =
  collect step isFinal(inject e)

(*　test *)
