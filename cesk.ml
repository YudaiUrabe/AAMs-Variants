(* Reference: "Systematic abstraction of abstract machines" *)




(* CESK Machine *)
(* add a store to CEK machine and use it allocate variable bindings, thereby eliminating recursion from the environment in CEK machine*)


(* SYNTAX of CESK machine *)

(* configuration*)
type config = term * env * store * cont

(* Environment *)
and env = (var * addr) list

(* Closure *)
and storable = Clo of lambda * env

(* store
 is a finite map from address to storable values *)
 and store =  (addr * storable) list

(* Continuation *)
and cont = 
  | Done
  | Ar of term * env * cont
  | Fn of lambda * env * cont

(* Address *)
and addr = int


(* tests *)
let ex_env : env = [("a", 2)]

let ex_term : term = TmApp(TmVar "a", TmVar "b")

let test_storable : storable = Clo (("c", TmVar "c"), ex_env)

let test_store : store = [(3, Clo (("d", TmVar "d"),[]))]

let test_cont : cont = Fn (("c", TmVar "c"), ex_env, Done)
