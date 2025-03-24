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
(* upadates on env *)
let (//) (env: env) entries = 
  List.fold_left(fun acc(key, value) -> StringMap.add key value acc) env entries
(* upadates on store *)
let (///) (store: store) entries =
  List.fold_left (fun acc (key, value) -> AddrMap.add key value acc) store entries




(* SEMANTICS of CESK* machine *)

(* injection function *)
let inject (e:term) : config =
  (e, StringMap.empty, AddrMap.empty, Done)

(* alloc function *)
let alloc (s: store): addr =
  let keys = AddrMap.fold (fun key _ acc -> key :: acc) s [] in
  let max_key = List.fold_left max 0 keys in
  max_key + 1

(* transition relation for the CESK* machine *)
let step (sigma: config): config = 
  match sigma with
  | (TmVar x, rho, s, kappa) ->
    let a = StringMap.find x rho in
    let Clo(lam, rho') = AddrMap.find a s in
     (TmAbs lam, rho', s, kappa)
  | (TmApp (f,e), rho, s, kappa) ->
      let a' = alloc s in
      let kappa' = Ar(e, rho, a') in
        (f, rho, s///[a' ==> Cont kappa], kappa')
  | (TmAbs lam, rho, s, Ar(e, rho', a')) ->
      (e, rho', s, Fn(lam, rho, a')) 
  | (TmAbs lam, rho, s, Fn((x, e) , rho', b)) -> 
    let Cont kappa = AddrMap.find b s in
    let a' = alloc s in
      (e, rho'//[x ==> a'], s///[a' ==> Clo(lam, rho)], kappa)
  | _ -> failwith "Invalid configuration"

 
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
  collect step isFinal (inject e)

(* test 
(λa.a)(λb.b) -> (λb.b)　*)
let test1 = TmApp (TmAbs ("a", TmVar "a"), TmAbs ("b", TmVar "b"))

let result = evaluate test1

(* auxiliary functions for this test *)
let rec string_of_term (t: term) =
  match t with
  | TmVar x -> x
  | TmAbs (x, t) -> "λ" ^ x ^ "." ^ string_of_term t
  | TmApp (t1, t2) -> "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
let rec string_of_env (env: env) =
  let bindings = StringMap.bindings env in
  let binding_to_string (var, addr) =
      var ^ " ↦ " ^ string_of_int addr
    in
    "{" ^ String.concat ", " (List.map binding_to_string bindings) ^ "}"
let rec string_of_cont (cont: cont) =
  match cont with
  | Done -> "Done"
  | Ar (t, _, _) -> "Ar(" ^ string_of_term t ^ ", ...)"
  | Fn ((x, t), _, _) -> "Fn(λ" ^ x ^ "." ^ string_of_term t ^ ", ...)"
let string_of_state (t: term) (env: env) (cont: cont) =
  "{" ^
   string_of_term t ^ ", " ^
   string_of_env env ^ ", " ^
   string_of_cont cont ^
  "}"

(* output *)
let () = 
  List.iter (fun (test1, env, _, cont) -> 
    Printf.printf "State: %s\n" (string_of_state test1 env cont)
  ) result
