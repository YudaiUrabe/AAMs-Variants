(* Reference: "Systematic abstraction of abstract machines" §2.2, TAPL §7*)

module StringMap = Map.Make(String)

(* Object Langugae -untyped lambda calculus(CbV)- *)
type var = string
and lambda = var * term
and term = 
  | TmVar of var
  | TmAbs of lambda
  | TmApp of term * term

(* CEK Machine *)
(* SYNTAX of CEK machine *)

(* configuration (state)
 triple of a control string(an expression), an environment and continuation)
*)
type config = term * env * cont

(* Closure
 is pair of a value and environment
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
      let Clo(lam, rho') = StringMap.find x rho in (TmAbs lam, rho', kappa)
  | (TmApp (f,e), rho, kappa) ->
      (f, rho, Ar(e, rho, kappa))
  | (TmAbs lam, rho, Ar(e, rho', kappa)) ->
      (e, rho', Fn(lam, rho, kappa)) 
  | (TmAbs lam, rho, Fn((x, e) , rho', kappa)) -> 
      (e,rho'//[x ==> Clo(lam, rho)], kappa)
  | _ ->
      failwith "Invalid configuration"

(* injection function 
the initial machine state for a closed expression e *)
let inject (e:term) : config =
  (e, StringMap.empty, Done)

(* auxiliary functions for evaluation function *)
(* isFinal 
A state is final when it has no next step.
So, this function checks if the continuation is empty.
*)
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
(* Create an initial state from the term e using "inject",
then apply "step" repeatedly until the final state is reached, saving all intermediate states in a list.*)
let evaluate (e: term): config list =
  collect step isFinal(inject e)




(*　test 
(λa.a)(λb.b) -> (λb.b)
*)
let term_test = TmApp (TmAbs ("a", TmVar "a"), TmAbs ("b", TmVar "b"))

let result = evaluate term_test

(* auxiliary functions for this test *)
let rec string_of_term (t: term) =
  match t with
  | TmVar x -> x
  | TmAbs (x, t) -> "λ" ^ x ^ "." ^ string_of_term t
  | TmApp (t1, t2) -> "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"

let () = 
  List.iter (fun (term_test, _, _) -> 
    Printf.printf "State: %s\n" (string_of_term term_test)
  ) result

