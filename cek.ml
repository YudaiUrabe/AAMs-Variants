(* Reference: "Systematic abstraction of abstract machines" §2.2, TAPL §7 *)

module StringMap = Map.Make(String)

(* Object Langugae -untyped lambda calculus(CbV)- *)
type var = string
and lambda = var * term (* value *)
and term = 
  | TmVar of var
  | TmAbs of lambda
  | TmApp of term * term

(* CEK Machine *)
(* SYNTAX of CEK machine *)

(* configuration (state)
 triple of a control string(an expression), an environment and continuation
*)
type config = term * env * cont

(* Closure
 is pair of a value and environment
*)
and d = Clo of lambda * env

(* Environment
is implemented as finite maps from variables to closures.
*)
and env = d StringMap.t

(* Continuation
represent evaluation context.
E ::= [] | E[([] term)] | E[(value [])]
*)

and cont = 
  | Done (* hole *)
  | Ar of term * env * cont
  | Fn of lambda * env * cont

(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries




(* SEMANTICS of CEK machine *)

(* (one-step) transition relation for the CEK machine 
*)
let step (sigma: config): config = 
  match sigma with
  | (TmVar x, rho, kappa) ->
      let Clo(lam, rho') = StringMap.find x rho in (TmAbs lam, rho', kappa)
  (* ここの理解が結構怪しめかもしれない．．．ひとまずこの上の記述でよしとするか →いいんじゃない？2025/0122　　この下は，（評価文脈って値を考える時に重要だから？）項が値のときに場合分けしてるのかな　*)
  | (TmApp (f,e), rho, kappa) ->
      (f, rho, Ar(e, rho, kappa))
  | (TmAbs lam, rho, Ar(e, rho', kappa)) ->
      (e, rho', Fn(lam, rho, kappa)) 
  | (TmAbs lam, rho, Fn((x, e) , rho', kappa)) ->
      (e,rho'//[x ==> Clo(lam, rho)], kappa)
  | _ ->
      failwith "Invalid configuration"

(* injection function 
the initial machine state for a closed expression e *)
let inject (e:term) : config =
  (e, StringMap.empty, Done)

(* auxiliary functions for evaluation function *)
(* isFinal 
A state is final when it has no next step.
This function checks if the continuation is empty.
*)
let isFinal (sigma_state: config) : bool =
  match sigma_state with
    |(TmAbs _, rho, Done) -> true
    | _ -> false

(* collect *)
let rec collect (f: config -> config) (isFinal: config-> bool)(sigma_collect: config): config list =
  if isFinal sigma_collect then
    [sigma_collect]
  else
    sigma_collect :: collect f isFinal (f sigma_collect)

(* evaluation function *)
(* Create an initial state from the term e using "inject",
then apply "step" repeatedly until the final state is reached, saving all intermediate states in a list.*)
let evaluate (e: term): config list =
  collect step isFinal(inject e)





(*　test 
(λa.a)(λb.b) -> (λb.b)
*)
let term_test = TmApp (TmAbs ("a", TmVar "a"), TmAbs ("b", TmVar "b"))

let result = evaluate term_test

(* auxiliary functions for this test *)
let rec string_of_term (t: term) =
  match t with
  | TmVar x -> x
  | TmAbs (x, t) -> "λ" ^ x ^ "." ^ string_of_term t
  | TmApp (t1, t2) -> "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"

(* output *)
let () = 
  List.iter (fun (term_test, _, _) -> 
    Printf.printf "State: %s\n" (string_of_term term_test)
  ) result


(* 

  
次すること；
・継続のArとFnの理解（実装としては合っているとは思うけど）をして残る一つのコメントを消す？＋ステップ関数の最初の場合分けの実装の箇所の理解（，継続や環境を使った別のテストの追加？ーこれはまぁ別に不要かと）

メモ：
・collect関数は，ステップ関数，isFinal関数，状態を受け取って，その状態に対してステップ関数を噛ませまくって，その状態から到達しうる状態のリストを返す
・type ('k, 'v) map = ('k, 'v) StringMap.t　とJFPみたいに書いたら上手く通らなかった．
  これを使うにはDataをinput上でしておかなくてはいけないのかな
  mapではなくて，上の記号をJFPと同様に使いましょうね．→いや，やはり:->はOCamlでは別の意味に使いそうだから避けた方が良さそう．
・昨日書いたシンタックスシュガーを参考に，cek.cmiとかcek.cmoが作られてるのビミョいが，，，，
・  (* Environment
This looks similar to (but different from) substitution in λ-Calculus という余計なことはなるべく書かないようにするか
envのところは，StringMap.tは，型コンストラクタだから，こいつにdを渡している
*)

要チェック：
・storeとstateでタイポしてないか気をつけよ
・ローとガンマがごちゃごちゃになりかけてるけど，多分，まだ混じってないはず（ローは環境でしょ，ガンマってなんやっけ）
・シグマは，step関数で書いているから，バッティングしないように．
・ローも使っているけど，多分変えても変えなくても良さそうな気がする，多分ね


＜一応通った例＞
(* auxiliary functions for this test *)
let rec string_of_term (t: term) =
  match t with
  | TmVar x -> x
  | TmAbs (x, t) -> "λ" ^ x ^ "." ^ string_of_term t
  | TmApp (t1, t2) -> "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"

(* auxiliary function to display the environment *)
let string_of_env (rho: env): string =
  StringMap.fold (fun key (Clo((x, t), _)) acc ->
      acc ^ key ^ " -> Clo(" ^ x ^ ", " ^ string_of_term t ^ "); "
    ) rho ""

(* auxiliary function to display the continuation *)
let rec string_of_cont (kappa: cont): string =
  match kappa with
  | Done -> "Done"
  | Ar (t, rho, kappa') ->
      "Ar(" ^ string_of_term t ^ ", " ^ string_of_env rho ^ ", " ^ string_of_cont kappa' ^ ")"
  | Fn ((x, t), rho, kappa') ->
      "Fn((" ^ x ^ ", " ^ string_of_term t ^ "), " ^ string_of_env rho ^ ", " ^ string_of_cont kappa' ^ ")"

(* output configuration as string *)
let string_of_config (term, rho, kappa) =
  "Term: " ^ string_of_term term ^ "\n" ^
  "Env: {" ^ string_of_env rho ^ "}\n" ^
  "Cont: " ^ string_of_cont kappa ^ "\n"
(* (λa.λb.a)(λc.c) *)
  let term_test2 = TmApp (TmAbs ("a", TmAbs ("b", TmVar "a")), TmAbs ("c", TmVar "c"))
  let result2 = evaluate term_test2
  let () =
    Printf.printf "Test 2: (λa.λb.a)(λc.c)\n";
    List.iter (fun config -> 
      Printf.printf "State:\n%s\n" (string_of_config config)
    ) result2

(* updated output function *)
let () =
  List.iter (fun config -> 
    Printf.printf "State:\n%s\n" (string_of_config config)
  ) result2 *)