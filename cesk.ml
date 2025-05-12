(* Reference: "Systematic abstraction of abstract machines" §2.4*)

module StringMap = Map.Make (String)
module AddrMap = Map.Make (Int)

type var = string
and lambda = var * term
and term = TmVar of var | TmAbs of lambda | TmApp of term * term

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
and store = storable AddrMap.t

(* Continuation *)
and cont = Done | Ar of term * env * cont | Fn of lambda * env * cont

(* Address *)
and addr = int

(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let ( ==> ) x y = (x, y)

let ( // ) map entries =
  List.fold_left
    (fun acc (key, value) -> StringMap.add key value acc)
    map entries

(* /// operator for AddrMap *)
let ( /// ) map entries =
  List.fold_left (fun acc (key, value) -> AddrMap.add key value acc) map entries

(* SEMANTICS of CESK machine *)

(* injection function *)
let inject (e : term) : config = (e, StringMap.empty, AddrMap.empty, Done)

(* alloc function *)
let alloc (s : store) : addr =
  let keys = List.map fst (AddrMap.bindings s) in
  (* Extract all keys from s,*)
  (* If you want a more functional style, use fold to gather keys:
     let keys = AddrMap.fold (fun key _ acc -> key :: acc) s [] in *)
  let max_key = List.fold_left max 0 keys in
  (* find the maximum key in the list,*)
  max_key + 1 (* and return the maximum key plus one as the new address. *)

(* transition relation for the CESK machine *)
let step (sigma : config) : config =
  match sigma with
  | TmVar x, rho, s, kappa ->
      let addr = StringMap.find x rho in
      (* ρ(x) *)
      let (Clo (lam, rho')) = AddrMap.find addr s in
      (TmAbs lam, rho', s, kappa)
  | TmApp (f, e), rho, s, kappa -> (f, rho, s, Ar (e, rho, kappa))
  | TmAbs v, rho, s, Ar (e, rho', kappa) -> (e, rho', s, Fn (v, rho, kappa))
  | TmAbs v, rho, s, Fn ((x, e), rho', kappa) ->
      let a' = alloc s in
      (e, rho' // [ x ==> a' ], s /// [ a' ==> Clo ((x, e), rho) ], kappa)
  | _ -> failwith "Invalid configuration"

(* auxiliary functions for evaluation function *)
(* isFinal *)
let isFinal (s : config) : bool =
  match s with TmAbs _, _, _, Done -> true | _ -> false

(* collect *)
let rec collect (f : config -> config) (isFinal : config -> bool)
    (state : config) : config list =
  if isFinal state then [ state ] else state :: collect f isFinal (f state)

(* evaluation function *)
let evaluate (e : term) : config list = collect step isFinal (inject e)

(* tests *)
(* auxiliary function for the tests *)
let rec string_of_term (t : term) : string =
  match t with
  | TmVar x -> x
  | TmAbs (x, t) -> "λ" ^ x ^ "." ^ string_of_term t
  | TmApp (t1, t2) -> "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"

let string_of_env (env : env) : string =
  let bindings = StringMap.bindings env in
  let binding_to_string (var, addr) = var ^ " ↦ " ^ string_of_int addr in
  "{" ^ String.concat ", " (List.map binding_to_string bindings) ^ "}"

(* Addition for the CESK machine *)
let string_of_store (store : store) : string =
  let bindings = AddrMap.bindings store in
  let binding_to_string (addr, Clo ((x, t), clo_env)) =
    string_of_int addr ^ " ↦ (λ" ^ x ^ "." ^ string_of_term t ^ ", "
    ^ string_of_env clo_env ^ ")"
  in
  "{" ^ String.concat ", " (List.map binding_to_string bindings) ^ "}"

let rec string_of_cont (cont : cont) : string =
  match cont with
  | Done -> "Done"
  | Ar (t, _, k) -> "Ar(" ^ string_of_term t ^ ", " ^ string_of_cont k ^ ")"
  | Fn ((x, t), _, k) ->
      "Fn(λ" ^ x ^ "." ^ string_of_term t ^ ", " ^ string_of_cont k ^ ")"

let string_of_state (t : term) (env : env) (store : store) (cont : cont) :
    string =
  "{" ^ string_of_term t ^ ", " ^ string_of_env env ^ ", "
  ^ string_of_store store ^ ", " ^ string_of_cont cont ^ "}"

(* test1
     eval ((λa.a)(λb.b)) = <λb.b, φ,[1 ↦ (λb.b, φ)], mt>
     cf. Under standard call-by-value evaluation,
   (λa.a)(λb.b) -> (λb.b) *)

let term_test1 = TmApp (TmAbs ("a", TmVar "a"), TmAbs ("b", TmVar "b"))

(* test2
   suc = λnsz.s(nsz)
   1 = λsz.sz
   suc 1 ->* λsz.s(sz) = 2 *)

let term_test2 =
  TmApp
    ( TmAbs
        ( "n",
          TmAbs
            ( "s",
              TmAbs
                ( "z",
                  TmApp
                    (TmVar "s", TmApp (TmApp (TmVar "n", TmVar "s"), TmVar "z"))
                ) ) ),
      TmAbs ("s", TmAbs ("z", TmApp (TmVar "s", TmVar "z"))) )

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
    (fun (term, env, store, cont) ->
      Printf.printf "State: %s\n" (string_of_state term env store cont))
    result

let () =
  let result1 = evaluate term_test1 in
  let result2 = evaluate term_test2 in
  let result3 = evaluate term_test3 in
  print_trace "Test 1" result1;
  print_trace "Test 2" result2;
  print_trace "Test 3" result3

(* check the correctness *)
let isFinal2 (s : config) : bool =
  match s with TmAbs _, _, _, Done -> true | _ -> false

let rec run (s : config) : config = if isFinal2 s then s else run (step s)
let evaluate2 (e : term) : config = run (inject e)

let () =
  let result1 = evaluate2 term_test1 in
  (*(* print result2 *)
    Printf.printf "Result2: %s\n" (let (t, env, cont) = result2 in string_of_state t env cont); *)
  print_endline "\n Correctness";
  assert (
    result1
    = ( TmAbs ("b", TmVar "b"),
        StringMap.empty,
        AddrMap.empty /// [ 1 ==> Clo (("b", TmVar "b"), StringMap.empty) ],
        Done ));
  (* <λb.b, φ,[1 ↦ (λb.b, φ)], mt> *)
  print_endline "test1 passed"
