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
 triple of an expression, an environment and continuation
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


(* SEMANTICS of the CEK machine *)

(* injection function 
the initial machine state for a closed expression e *)
let inject (e:term) : config =
  (e, StringMap.empty, Done)

(* (one-step) transition relation for the CEK machine 
*)
let step (sigma: config): config = 
  match sigma with
  | (TmVar x, rho, kappa) ->
      let Clo((x', e'), rho') = StringMap.find x rho in (TmAbs (x', e'), rho', kappa)
  | (TmApp (f,e), rho, kappa) ->
      (f, rho, Ar(e, rho, kappa))
  | (TmAbs v, rho, Ar(e, rho', kappa)) ->
      (e, rho', Fn(v, rho, kappa)) 
  | (TmAbs v, rho, Fn((x, e), rho', kappa)) ->
      (e, rho'//[x ==> Clo(v, rho)], kappa)
  | _ ->
      failwith "Invalid configuration"

      

(* auxiliary functions for evaluation function *)
(* isFinal 
A state is final when it has no next step.
This function checks if the term is a value and the continuation is empty.
*)
let isFinal (s: config) : bool =
  match s with
    |(TmAbs _, _, Done) -> true
    | _ -> false

(* collect *)
let rec collect (f: config -> config) (isFinal: config-> bool)(state: config): config list =
  if isFinal state then
    [state]
  else
    state :: collect f isFinal (f state)

(* evaluation function *)
(* Create an initial state from the term e using "inject",
then apply "step" repeatedly until the final state is reached, saving all intermediate states in a list.*)
let evaluate (e: term): config list =
  collect step isFinal(inject e)




(* tests *)
(* auxiliary function for the tests *)
let rec string_of_term (t: term) =
  match t with
  | TmVar x -> x
  | TmAbs (x, t) -> "λ" ^ x ^ "." ^ string_of_term t
  | TmApp (t1, t2) -> "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
let rec string_of_env (env: env) =
  let bindings = StringMap.bindings env in
  let binding_to_string (var, Clo ((x, t), clo_env)) =
      var ^ " ↦ (λ" ^ x ^ "." ^ string_of_term t ^ ", " ^ string_of_env clo_env ^ ")"
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


  (*　test1 
  eval ((λa.a)(λb.b)) = <λb.b, φ, mt>
  cf. Under standard call-by-value evaluation,
(λa.a)(λb.b) -> (λb.b)
*)
let term_test1 = TmApp (TmAbs ("a", TmVar "a"), TmAbs ("b", TmVar "b"))

(* test2
suc = λnsz.s(nsz)
1 = λsz.sz
eval (suc 1) = <λs.λz.(s ((n s) z)), [n ↦ (λs.λz.(s z), φ)], mt>
cf. Under standard call-by-value evaluation,
suc 1 -> λsz.s(sz) = 2
 *)
 let term_test2 = TmApp(
                    TmAbs("n", TmAbs ("s",TmAbs ("z", TmApp (TmVar "s", TmApp(TmApp(TmVar "n", TmVar "s"), TmVar "z"))))),
                    TmAbs ("s", TmAbs ("z", TmApp (TmVar "s", TmVar "z"))))


(* output *)
  let print_trace name result =
    Printf.printf "\n=== %s ===\n" name;
    List.iter (fun (term, env, cont) ->
      Printf.printf "State: %s\n" (string_of_state term env cont)
    ) result
  
  let () =
    let result1 = evaluate term_test1 in
    let result2 = evaluate term_test2 in
    print_trace "Test 1" result1;
    print_trace "Test 2" result2
  



(* check the correctness *)
let isFinal2 (s: config) : bool =
  match s with
    |(TmAbs _, _, Done) -> true
    | _ -> false
let rec run (s:config): config =
      if isFinal2 s then s
      else run (step s)
let evaluate2 (e: term): config =
  run(inject e)

  let () =
  let result1 = evaluate2 term_test1 in
  let result2 = evaluate2 term_test2 in
  (*(* print result2 *)
   Printf.printf "Result2: %s\n" (let (t, env, cont) = result2 in string_of_state t env cont); *)
  print_endline "\n Correctness";
  assert(result1 = (TmAbs ("b", TmVar "b"), StringMap.empty, Done));
  print_endline "test1 passed";
  assert(result2 =  (TmAbs ("s", TmAbs ("z", TmApp (TmVar "s", TmApp (TmApp (TmVar "n", TmVar "s"),TmVar "z")))),
  StringMap.empty//["n" ==> Clo (("s", TmAbs ("z", TmApp (TmVar "s", TmVar "z"))),StringMap.empty)],
  Done));
  print_endline "test2 passed"; 

