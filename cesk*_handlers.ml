(* Reference: "Liberating Effects with Rows and Handlers",
"Foundations for Programming and Implementing Effect Handlers"(2021) *)

module StringMap = Map.Make(String)
module AddrMap = Map.Make(Int)

(* Object Langugae -Fine-grain call-by-value lambda calculus- *)
(* ref. An Abstract Machine Semantics for Handlers, Mar 2017, p.6 *)
type var = string
and label = string
and lambda = var * comp
and termvalue = 
  | TmVar of var
  | TmAbs of lambda

(* computations *)
and comp = 
  | TmApp of termvalue * termvalue
  | Return of termvalue
  | Let of var * comp * comp
  | Do of label * termvalue
  | Handle of comp * handler

(* handlers *)
and handler = 
  | ReturnClause of var * comp
  | OperationClause of label * var * string * comp * handler
(* 元のCEKに合わせて対象言語の構文定義も修正しよう 　どういう意図で書いたっけ？？*)


(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries




(* SYNTAX of CESK* machine with handlers *)
(* configuration *)
type config = 
  | comp * val_env * store * cont
  | comp * val_env * store * cont * cont' (* Augmented the configuration space *)

(* Function Closures *)
and d = Clo of val_env * lambda

(* Values *)
and AMvalue = 
  | d
  | cont

(* Value environments *)
and val_env = AMvalue StringMap.t
(* このtはちゃんと変数って表せてるんやろか *)

and storable = 
  | AMVal of AMvalue
  | PureCont of pure_cont
(* ここマジでちゃんとしないと *)

(* store*)
 and store =  storable AddrMap.t


 この下の継続周りが再帰しないようにアドレスに．．．

(* (Captured) Continuations *)
and cont = 
  | Done
  | cont_frame :: addr
(* 再帰しないようにアドレスに変えた *)

and cont_frame = (pure_cont, chi)

and pure_cont = 
  | Done
  | pure_cont_frame :: addr
(* 再帰しないようにアドレスに変えた *)

and pure_cont_frame = (val_env, x, comp)
(* 思ったんやけど，型フレーム？(contとか)で定義しているのと，それによく使われる変数（xとか）の区別をちゃんとつけていないと！ *)

(* Hanlder Closures *)
and chi = (val_env, handler)
(* ここのハンドラって，対象言語の定義の方だから，実装の方で工夫しないといけんでしょ．．． *)

(* Address *)
and addr = int











(* SEMANTICS of this machine *)

(* Identity Continuation *)
let idCont = 
(* これもアドレスに変えないといけないかも？ *)

(* injection function M-INIT 
map a computation term into an machine configuration
*)
let inject (m:comp) : config =
  (m, StringMap.empty, AddrMap.empty, 0)
(* 飛んでいくコンフィギュを変えよう  Storeと継続（特に前者）ほんまにこれでええか？？？ *)



(* Interpretation function for values *)




(* transition function *)
(* ref."Liberating Effects with Rows and Handlers", FIg.9 *)
let step (sigma: config): config = 
    match sigma with
    | (TmApp(v,w), rho, kappa) -> 

 (* M-APP *)
 (* M-APPCONT *)
 (* M-LET *)
 (* M-HANDLE *)
 (* M-RETCONT *)
 (* M-RETHANDLER *)
(* M-RETTOP *)
  (* M-OP *)
  (* M-OP-HANDLE *)
 (* M-OP-FORWARD *)
(* ~~~ *)







(* isFinal *)
let isFinal (sigma_state: config) : bool =
  match sigma_state with
    |(Return _, rho, s, Done) -> true
    | _ -> false
(* コンフィギュを直した *)

(* collect *)
let rec collect (f: config -> config) (isFinal: config-> bool)(sigma_collect: config): config list =
  if isFinal sigma_collect then
    [sigma_collect]
  else
    sigma_collect :: collect f isFinal (f sigma_collect)
(* 元のままでいいと思う *)

(* evaluation function *)
let evaluate (M: comp): config list =
  collect step isFinal(inject M)
(* これ，入力はコンピュテーションでいいのかな？inj関数もそうだしいいんじゃないか *)



(*　test *)
let term_test =

let result = evaluate term_test

(* auxiliary function *)
let rec string_of_term (M: comp) =
  match M with
  |~~~~~

(* output *)
let () = 
  List.iter (fun (term_test, _, _) -> 
    Printf.printf "State: %s\n" (string_of_term term_test)
  ) result
(* そのままで大丈夫か？ *)



次のアクションアイテム：　まずは紙の上で確実に理解する