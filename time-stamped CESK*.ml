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

and storable = 
  | Clo of lambda * env
  | Cont of cont


(* Environment *)
and env = addr StringMap.t

(* store *)
and store =  storable AddrMap.t

(* Continuation *)
and cont = 
  | Done
  | Ar of term * env * addr
  | Fn of lambda * env * addr

(* Address *)
and addr = int

(* Time *)
and time = int







(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries






(* SEMANTICS of time-stamped CESK* machine *)

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
