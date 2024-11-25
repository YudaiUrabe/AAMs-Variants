(* Object Langugae -untyped lambda calculus(CbV)- *)



(* time-stamped CESK* machine *)

(* SYNTAX of time-stamped CESK* machine *)

(* configuration*)
type config = term * env * store * cont * time

(* Address *)
and addr = int

(* Time *)
and time = int

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




  (* tick function *)


  (* alloc function *)


