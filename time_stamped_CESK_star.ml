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

(* Environment *)
and env = addr StringMap.t

(* store *)
and store =  storable AddrMap.t

(* Continuation *)
and cont = 
  | Done
  | Ar of term * env * addr
  | Fn of lambda * env * addr

and storable = 
  | Clo of lambda * env
  | Cont of cont

(* Address *)
and addr = int

(* Time *)
and time = int



(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
(* updates on env *)
let (//) (env: env) entries = 
  List.fold_left(fun acc(key, value) -> StringMap.add key value acc) env entries
(* updates on store *)
let (///) (store: store) entries =
  List.fold_left (fun acc (key, value) -> AddrMap.add key value acc) store entries





(* SEMANTICS of time-stamped CESK* machine *)

(* injection function *)
let inject (e:term) : config =
  (e, StringMap.empty, AddrMap.empty, Done, 0)

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
    let a = StringMap.find x rho in
    let Clo(lam, rho') = AddrMap.find a s in
    let t' = tick sigma in
      (TmAbs lam, rho', s, kappa, t')

  | (TmApp (f,e), rho, s, kappa, t) ->
    let a' = alloc sigma in
    let kappa' = Ar(e, rho, a') in
    let t' = tick sigma in
      (f, rho, s///[a' ==> Cont kappa], kappa', t')

  | (TmAbs lam, rho, s, Ar(e, rho', a'), t) ->
    let t' = tick sigma in
      (e, rho', s, Fn(lam, rho, a'), t') 

  | (TmAbs lam, rho, s, Fn((x, e) , rho', b), t) -> 
    let Cont kappa = AddrMap.find b s in
    let a' = alloc sigma in
    let t' = tick sigma in
      (e, rho'//[x ==> a'], s///[a' ==> Clo(lam, rho)], kappa, t')

  | _ -> failwith "Invalid configuration"



(* auxiliary functions for evaluation function *)
(* isFinal *)
let isFinal (state: config) : bool =
  match state with
    |(TmAbs _, _, _, Done,_) -> true
    | _ -> false

(* collect *)
let rec collect (f: config -> config) (isFinal: config-> bool)(state: config): config list =
  if isFinal state then
    [state]
  else
    state :: collect f isFinal (f state)

(* evaluation function *)
let evaluate (e: term): config list =
  collect step isFinal(inject e)



(* tests *)
(* auxiliary functions for this test *)
let rec string_of_term (t: term): string =
  match t with
  | TmVar x -> x
  | TmAbs (x, t) -> "λ" ^ x ^ "." ^ string_of_term t
  | TmApp (t1, t2) -> "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
let rec string_of_env (env: env): string =
  let bindings = StringMap.bindings env in
  let binding_to_string (var, addr) =
      var ^ " ↦ " ^ string_of_int addr
    in
    "{" ^ String.concat ", " (List.map binding_to_string bindings) ^ "}"
  let rec string_of_cont (cont : cont) : string =
      match cont with
      | Done -> "Done"
      | Ar (t, rho, a) -> "Ar(" ^ string_of_term t ^ ", "^ string_of_env rho ^ ", " ^ string_of_int a ^ ")"
      | Fn ((x, t), rho, a) ->
          "Fn(λ" ^ x ^ "." ^ string_of_term t ^ ", "^ string_of_env rho ^ ", " ^ string_of_int a ^ ")"
  let string_of_store (store : store) : string =
    let bindings = AddrMap.bindings store in
    let binding_to_string (addr, storable) =
      match storable with
      | Clo ((x, t), clo_env) ->
        string_of_int addr ^ " ↦ (λ" ^ x ^ "." ^ string_of_term t ^ ", "
      ^ string_of_env clo_env ^ ")"
      | Cont k->
        string_of_int addr ^ " ↦ (" ^ string_of_cont k ^ ")"
    in
    "{" ^ String.concat ", " (List.map binding_to_string bindings) ^ "}"

let string_of_state (t: term) (env: env)(store: store) (cont: cont)(time: time): string =
  "{" ^
   string_of_term t ^ ", " ^
   string_of_env env ^ ", " ^
   string_of_store store ^ ", " ^
   string_of_cont cont ^ ", " ^
   string_of_int time ^
  "}"


  (* test1 
(λa.a)(λb.b) -> (λb.b)　*)
let term_test1 = TmApp (TmAbs ("a", TmVar "a"), TmAbs ("b", TmVar "b"))


(* test2
suc = λnsz.s(nsz)
1 = λsz.sz
suc 1 -> λsz.s(sz) = 2
 *)
 let term_test2 = TmApp(
                    TmAbs("n", TmAbs ("s",TmAbs ("z", TmApp (TmVar "s", TmApp(TmApp(TmVar "n", TmVar "s"), TmVar "z"))))),
                    TmAbs ("s", TmAbs ("z", TmApp (TmVar "s", TmVar "z"))))

(* test3
   (λx.λy.x) (λz.z) (λw.w) ->*  λz.z
*)
let term_test3 =
  TmApp
    ( TmApp (TmAbs ("x", TmAbs ("y", TmVar "x")), TmAbs ("z", TmVar "z")),
      TmAbs ("w", TmVar "w") )


(* output *)
  let print_trace name result =
    Printf.printf "\n=== %s ===\n" name;
    List.iter
      (fun (term, env, store, cont, time) ->
        Printf.printf "State: %s\n" (string_of_state term env store cont time))
      result
  
  let () =
    let result1 = evaluate term_test1 in
    let result2 = evaluate term_test2 in
    let result3 = evaluate term_test3 in
    print_trace "Test 1" result1;
    print_trace "Test 2" result2;
    print_trace "Test 3" result3
  

(* check the correctness *)

