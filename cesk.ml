(* Reference: "Systematic abstraction of abstract machines" *)


ここにtermを用意．
OCamlは，関数をリストで表すみたいなやつ．

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

テストを通ったはいいが，テストの内容を吟味していない，，，





あれ，type operatorや糖衣はいらない？？？





多分CEKにStoreの定義を要所要所丁寧に追加していくのかな

(* futher exploit the store to allocate continuations, whichc corresponds to a well-known implementation technique used in FL compilers (Shao&Appel, 1994) *)




