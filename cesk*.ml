(* Object Langugae -untyped lambda calculus(CbV)- *)



(* CESK* machine
eliminate recursion from continuation in CESK machine
*)


(* SYNTAX of CESK* machine *)

(* configuration*)
type config = term * env * store * cont

(* Address *)
and addr = int


(* Environment *)
and env = (var * addr) list


(* Closure *)
and storable = 
  | Clo of lambda * env
  | Cont of cont

(* store is a finite map from address to storable values *)
and store =  (addr * storable) list

(* Continuation*)
and cont = 
  | Done
  | Ar of term * env * addr
  | Fn of lambda * env * addr



