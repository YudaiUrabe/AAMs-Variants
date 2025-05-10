(* Reference: "Systematic abstraction of abstract machines" §2.4*)

module StringMap = Map.Make(String)
module AddrMap = Map.Make(Int)

type var = string
and lambda = var * term
and term = 
  | TmVar of var
  | TmAbs of lambda
  | TmApp of term * term

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
 and store =  storable AddrMap.t

(* Continuation *)
and cont = 
  | Done
  | Ar of term * env * cont
  | Fn of lambda * env * cont

(* Address *)
and addr = int


(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries
(* /// operator for AddrMap *)
let (///) map entries = List.fold_left(fun acc(key, value) -> AddrMap.add key value acc) map entries





(* SEMANTICS of CESK machine *)

(* alloc function *)
 let alloc (s: store): addr =
  let keys = List.map fst (AddrMap.bindings s) in                (* Extract all keys from s,*)
  (* let keys = AddrMap.fold (fun key _ acc -> key :: acc) s [] in  *)
  let max_key = List.fold_left max 0 keys in                     (* find the maximum key in the list,*)
  max_key + 1                                                    (* and return the maximum key plus one as the new address. *)

(* transition relation for the CESK machine *)
let step (sigma: config): config = 
  match sigma with
  | (TmVar x, rho, sigma_store, kappa) ->
    let addr = StringMap.find x rho in (* (ρ!x) *)
    let Clo(lam, rho') = AddrMap.find addr sigma_store in
     (TmAbs lam, rho', sigma_store, kappa)
  | (TmApp (f,e), rho, sigma_store, kappa) ->
      (f, rho, sigma_store, Ar(e, rho, kappa))
  | (TmAbs lam, rho, sigma_store, Ar(e, rho', kappa)) ->
      (e, rho', sigma_store, Fn(lam, rho, kappa)) 
  | (TmAbs lam, rho, sigma_store, Fn((x, e) , rho', kappa)) -> 
      let a' = alloc sigma_store 
      in (e, rho'//[x ==> a'], sigma_store///[a' ==> Clo(lam, rho)], kappa)
  | _ -> failwith "Invalid configuration"

(* injection function 
combines the term with the empty env, store and continuation*)
let inject (e:term) : config =
 (e, StringMap.empty, AddrMap.empty, Done)




(* isFinal *)
let isFinal (state: config) : bool =
  match state with
    |(TmAbs _, _, _, Done) -> true
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
let rec string_of_store (store: store) =
〜〜


let rec string_of_cont (cont: cont) =
  match cont with
  | Done -> "Done"
  | Ar (t, _, k) -> "Ar(" ^ string_of_term t ^ ", " ^ string_of_cont k ^ ")"
  | Fn ((x, t), _, k) -> "Fn(λ" ^ x ^ "." ^ string_of_term t ^ ", " ^ string_of_cont k ^ ")"
let string_of_state (t: term) (env: env) (store: store)(cont: cont) =
  "{" ^
   string_of_term t ^ ", " ^
   string_of_env env ^ ", " ^
   string_of_store store ^ ", " ^
   string_of_cont cont ^
  "}"  




let term_test1 = TmApp (TmAbs ("a", TmVar "a"), TmAbs ("b", TmVar "b"))



 let term_test2 = TmApp(
                    TmAbs("n", TmAbs ("s",TmAbs ("z", TmApp (TmVar "s", TmApp(TmApp(TmVar "n", TmVar "s"), TmVar "z"))))),
                    TmAbs ("s", TmAbs ("z", TmApp (TmVar "s", TmVar "z"))))



(* output *)
let () = 
  List.iter (fun (term_test, _, _, _) -> 
    Printf.printf "State: %s\n" (string_of_term term_test)
  ) result



let () =
let result1 = evaluate term_test1 in
let result2 = evaluate term_test2 in
print_trace "Test 1" result1;
print_trace "Test 2" result2

