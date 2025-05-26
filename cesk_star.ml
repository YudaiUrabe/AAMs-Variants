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





(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
(* upadates on env
Usage: env // entries
*)
let (//) (env: env) entries = 
  List.fold_left(fun acc(key, value) -> StringMap.add key value acc) env entries
(* upadates on store 
Usage: store // entries
*)
let (///) (store: store) entries =
  List.fold_left (fun acc (key, value) -> AddrMap.add key value acc) store entries




(* SEMANTICS of CESK* machine *)

(* injection function *)
let inject (e:term) : config =
  (e, StringMap.empty, AddrMap.empty, Done)

(* alloc function *)
let alloc (s: store): addr =
  let keys = List.map fst (AddrMap.bindings s) in    (* Extract all keys from s,*)
  let max_key = List.fold_left max 0 keys in         (* find the maximum key in the list,*)
  max_key + 1                                        (* and return the maximum key plus one as the new address. *)

(* transition relation for the CESK* machine *)
let step (sigma: config): config = 
  match sigma with
  | (TmVar x, rho, s, kappa) ->
    let addr = StringMap.find x rho in
    let(Clo(lam, rho')) = AddrMap.find addr s in
     (TmAbs lam, rho', s, kappa)
  | (TmApp (f,e), rho, s, kappa) ->
      let a' = alloc s in
      let s' = s///[a' ==> Cont kappa] in
      let kappa' = Ar(e, rho, a') in
        (f, rho, s', kappa')
  | (TmAbs v, rho, s, Ar(e, rho', a')) ->
      (e, rho', s, Fn(v, rho, a')) 
  | (TmAbs v, rho, s, Fn((x, e) , rho', b)) -> 
    let Cont kappa = AddrMap.find b s in
    let a' = alloc s in
      (e, rho'//[x ==> a'], s///[a' ==> Clo(v, rho)], kappa)
  | _ -> failwith "Invalid Configuration"


(* auxiliary functions for evaluation function *)  
(* isFinal *)
 let isFinal (s: config) : bool =
   match s with
     |(TmAbs _, _, _, Done) -> true
     | _ -> false
 
 (* collect *)
 let rec collect (f : config -> config) (isFinal : config -> bool)
 (state : config) : config list =
if isFinal state then [ state ] else state :: collect f isFinal (f state)


(* evaluation function *)
let evaluate (e: term): config list =
  collect step isFinal (inject e)


  (* tests *)
  (* test 
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


(* test3 *)



(* auxiliary function for the tests *)
let rec string_of_term (t: term) : string  =
  match t with
  | TmVar x -> x
  | TmAbs (x, t) -> "λ" ^ x ^ "." ^ string_of_term t
  | TmApp (t1, t2) -> "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"


  let rec string_of_env (env: env) : string  =
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

let string_of_state (t: term) (env: env) (store : store)(cont: cont) : string  =
  "{" ^
   string_of_term t ^ ", " ^
   string_of_env env ^ ", " ^
   string_of_store store ^ ", " ^
   string_of_cont cont ^
  "}"





(* output *)
let print_trace name result =
  Printf.printf "\n=== %s ===\n" name;
  List.iter
    (fun (term, env, store, cont) ->
      Printf.printf "State: %s\n" (string_of_state term env store cont))
    result

let () =
  let result1 = evaluate term_test1 in
  let result2 = evaluate term_test2 in
  (* let result3 = evaluate term_test3 in *)
  print_trace "Test 1" result1;
  print_trace "Test 2" result2
  (* print_trace "Test 3" result3 *)



(* check the correctness *)
