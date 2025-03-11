(* Reference: "Systematic abstraction of abstract machines" §2.2, TAPL §7 *)

module StringMap = Map.Make(String)

(* Object Langugae -untyped lambda calculus(CbV)- *)
type var = string
and lambda = var * term (* value *)
and term = 
  | TmVar of var
  | TmAbs of lambda
  | TmApp of term * term

(* CEK Machine *)
(* SYNTAX of CEK machine *)

(* configuration (state)
 triple of a control string(an expression), an environment and continuation
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

(* (one-step) transition relation for the CEK machine 
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
This function checks if the continuation is empty.
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





(*　test1 
(λa.a)(λb.b) -> (λb.b)
*)
let term_test = TmApp (TmAbs ("a", TmVar "a"), TmAbs ("b", TmVar "b"))

(* test2
suc = λnsz.s(nsz)
1 = λsz.sz
suc 1 -> λsz.s(sz) = 2
 *)
 let term_test2 = TmApp(
                    TmAbs("n", TmAbs ("s",TmAbs ("z", TmApp (TmVar "s", TmApp(TmApp(TmVar "n", TmVar "s"), TmVar "z"))))),
                    TmAbs ("s", TmAbs ("z", TmApp (TmVar "s", TmVar "z"))))
let result = evaluate term_test2

(* auxiliary function for this test *)
let rec string_of_term (t: term) =
  match t with
  | TmVar x -> x
  | TmAbs (x, t) -> "λ" ^ x ^ "." ^ string_of_term t
  | TmApp (t1, t2) -> "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
let rec string_of_env (env: env) =
  let bindings = StringMap.bindings env in
  let binding_to_string (var, Clo ((x, t), _)) =
      var ^ " ↦ λ" ^ x ^ "." ^ string_of_term t
    in
    "{" ^ String.concat ", " (List.map binding_to_string bindings) ^ "}"
let rec string_of_cont (cont: cont) =
  match cont with
  | Done -> "Done"
  | Ar (t, _, k) -> "Ar(" ^ string_of_term t ^ ", " ^ string_of_cont k ^ ")"
  | Fn ((x, t), _, k) -> "Fn(λ" ^ x ^ "." ^ string_of_term t ^ ", " ^ string_of_cont k ^ ")"
let string_of_state (t: term) (env: env) (cont: cont) =
  "{" ^
   string_of_term t ^ ", " ^
   string_of_env env ^ ", " ^
   string_of_cont cont ^
  "}"

(* output *)
let () = 
  List.iter (fun (term_test, env, cont) -> 
    Printf.printf "State: %s\n" (string_of_state term_test env cont)
  ) result
