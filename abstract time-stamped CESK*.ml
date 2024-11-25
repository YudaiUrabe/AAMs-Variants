(* Object Langugae -untyped lambda calculus(CbV)- *)



(* 
goal: to construct a function, CEK_hat

abstract state: Σ_hat
abstract state transition: |->CEK_hat
abstraction map: α: Σ →  Σ_hat 
abstract evaluation function: CEK_hat(e)

*)





(* abstract time-stamped CESK* 
*)

(* SYNTAX of abstract time-stamped CESK* machine *)

(* configuration*)
type config = term * env * store * cont * time

(* Address *)
(* and addr = int *)

(* Time *)
(* and time = int *)

(* Environment *)
(* and env = (var * addr) list *)

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




  (* type P *)
