


(* CESK Machine *)
(* add a store to CEK machine and use it allocate variable bindings, thereby eliminating recursion from the environment in CEK machine*)


(* SYNTAX of CESK machine *)

(* configuration*)
type config = term * env * store * cont

(* Address *)
and addr = int
これでええかな

(* Environment *)
and env = (var * addr) list
変えなきゃかも

(* Closure *)
and storable = Clo of lambda * env

(* store is a finite map from address to storable values *)
and store =  (addr * storable) list

(* Continuation*)
and cont = 
  | Done (* hole *)
  | Ar of term * env * cont
  | Fn of lambda * env * cont


多分CEKにStoreの定義を要所要所丁寧に追加していくのかな

(* futher exploit the store to allocate continuations, whichc corresponds to a well-known implementation technique used in FL compilers (Shao&Appel, 1994) *)