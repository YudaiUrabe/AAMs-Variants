(* Reference: "Liberating Effects with Rows and Handlers",
"Foundations for Programming and Implementing Effect Handlers"(2021) *)

module StringMap = Map.Make(String)

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
(* handlerのところの実装これでいいのかな？集合だし，集合どうしの演算とかあるよね 
 | EmptyHandler    ハンドラが空の場合  を実装しなくちゃダメかもしれない
 *)
(* 継続が単にstringとして実装されているけど，ちょっと良くないんじゃない？
 and continuation = var * comp みたいに？
 みたいに？　でも，エフェクトハンドラの実装の仕方をまず見てみようか．
 抽象機会で使う継続と，対照言語の継続って別物だから，下でcont型を使うから異なるのならバッティングしないようにしないと 
*)


(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries




(* SYNTAX of CEK machine with handlers *)
(* ref. An Abstract Machine Semantics for Handlers, Mar 2017, p.18 *)

(* configuration *)
type config = 
  | comp * val_env * cont
  | comp * val_env * cont * cont' (* Augmented the configuration space of CEK *)
  (* なんでエラー出るかな？Compをちゃんと定義実はできていないってこと？ *)

(* Value environments *)
and val_env =
  | StringMap.empty
  | val_env // [x ==> cekvalue]
(* なんでval＿envは色がつかないんだ？ 他にも色がついてくれないところが以下にあるよね．*)

(* Function Closures *)
and d = Clo of val_env * lambda
(* これってここでも，閉包と呼称するの？　通常のCEKって，環境とラムダ抽象の順番が逆だよね　　あれ，下のフレームやハンドラみたいに，タプルで書くんじゃない？ *)

(* Values *)
and cekvalue = 
  | d
  | cont
(* このバリューは上の対象言語のではなく！，抽象機械のでしょう！（なんで，JFPとかでは２種類のvalueがなかったんやろか，対象言語がfinge-graindからかしら）
こいつって大文字から始めなくても良いのかな？対象言語のバリューはそうしているけど *)


(* (Captured) Continuations *)
and cont = 
  | Done
  | cont_frame :: cont
  (* ArとかFnとかとはどう違う方針で定義，実装しているんだろうね， 
これが，
E ::= [] | E[([] term)] | E[(value [])]みたいな評価文脈と同様な存在
*)
and cont_frame = (pure_cont, chi)

and pure_cont = 
  | Done
  | pure_cont_frame :: pure_cont

and pure_cont_frame = (val_env, x, comp)
(* 思ったんやけど，型フレーム？(contとか)で定義しているのと，それによく使われる変数（xとか）の区別をちゃんとつけていないと！ *)

(* Hanlder Closures *)
and chi = (val_env, handler)
(* ここのハンドラって，対象言語の定義の方だから，実装の方で工夫しないといけんでしょ．．． *)





















(* SEMANTICS of this machine *)

(* IdentityContinuation *)
let idCont = [([], (StringMap.empty, ReturnClause 恒等ハンドラ))]
(* あの括弧ってどうやって実装する？ *)

(* injection function M-INIT 
map a computation term into an machine configuration
*)
let inject (m:comp) : config =
  (m, StringMap.empty, idCont)




(* Interpretation function for values *)
let interpret_value (tv: termvalue)(rho: val_env): cekvalue =
  match tv with
  | TmVar x -> (
    match StringMap.find_opt x rho with 下の二行はまだ良いとして，ここの行が完全には理解できていない
    | Some v -> v  (* Get the value of x under the environment rho *)
    | None -> failwith ("Unbound variable: " ^ x)  (* Error if the variable is not found in the environment *)
  )
  | TmAbs lam -> Clo(rho, lam)
(* ここは対象言語における値から，抽象機械における値に飛ばしているんね　　あれ，意外と，ここの定義って，テキトーだったりする？ *)



(* transition function *)
(* ref."Liberating Effects with Rows and Handlers", FIg.9 *)
let step (sigma: config): config = 
    match sigma with
    | (TmApp(v,w), rho, kappa) -> 
      let v' = interpret_value v rho in
      let w' = interpret_value w rho in
        (match v' with
        | Clo(rho', (x, M)) -> 
          (M, StringMap.add x w' rho'ここをちゃんと, kappa)  (* M-APP *)
        | _ -> failwith "Application error: not a closure")
      (* 　ここの分岐って，interpretation funcに応じて分かれているのかな *)   
    | (TmApp(v,w), rho, kappa) ->
      let kappa' = interpret_value v rho in 
        (Return(w), rho, kappa' @ kappa) (* M-APPCONT *)
      あと，この規則は特にその行なっている意味をちゃんと理解しないと
    
    | (Let(x,M,N), rho, (s,chi)::kappa) ->
        (M, rho, ((rho, x, N) :: s, chi) :: kappa) (* M-LET *)
    | (Handle(M, H), rho, kappa) ->
        (M, rho, ([],(rho, H))::kappa) (* M-HANDLE *)

    | (Return V, rho, ((rho', x, N)::s, chi)::kappa) ->
         (N, StringMap.add x (interpret_value V rho) rho'ちゃんと, (s,chi)::kappa) (* M-RETCONT *)
    | (Return V, rho, ([],(rho', H))::kappa) ->
        (match H with これでちゃんと実装できていると言える？
          | {return x -> M} -> 
            (M, StringMap.add x (interpret_value V rho) rho'ちゃんと, kappa)  (* M-RETHANDLER *)
          | _ -> failwith "Handle error: invalid handler"
        )
    | (Return V, rho, []) ->
        interpret_value V rho (* M-RETTOP *)



    | (Do(l, V), rho, kappa) ->
       (Do(l, V), rho, kappa, []) (* M-OP *)

    | (Do(l, tv), rho, (s, (rho', H))::kappa, kappa') ->
      match H with 
      | {l x k -> M} -> 
           let updated_rho = StringMap.add x (interpret_value V rho) rho' in
        (M, updated_rho, kappa) (* M-OP-HANDLE *)
      | _ -> 
          (Do(l, V), rho, kappa, kappa' @ [(s, (rho', H))]) (* M-OP-FORWARD *)
          なんか最後に， :: kappaをつけなきゃまずい？ あと，普通に二つをまとめたのもあまり良くないのかも？
      (* ハンドラが操作lに対して定義を持っている場合と持っていない場合で場合分けしてる *)




次のアクション：
・構文：ほとんど実装としては合っていそうな気がするから，日本語で書いたコメントの箇所を見直す＋継続のフレームとかマシンの定義を理解する（特にハンドラ周りかな．なんか直感的な意味も理解してないと，繊維関数を理解できない気がする．）
・意味論・遷移関数のところCEKと見比べながらCESKと並行して完成させようか．
・この下のメモを参考に修正を続けるか

      対象言語のハンドラ，恒等ハンドラ，　
      抽象機械の環境ローで値を求めるやつ
      条件文をちゃんと表示

      APPCONTの必要性 ＋＋のやつ

      条件分岐があるのは，前件が同じだから，H(l) 分岐しやすいか，意外と．
ローの代入？のやつをちゃんとしないといかんでしょうに





isFinal, collect, evalって抽象インタプリタでは使わない？から，
実装はしなくていい（もしくは，紙の上である程度できてから実装）ような気がする，，，紙の上では変形していく際に考えるのは良いことだろうけど．








(*　test *)

cont'はどこで定義するのやら．
OCaml では、++ はリストの連結を表す @ 演算子に対応します　だって．． これあってんのかな？
条件文のところの実装を確認　あれって前件同じなんだから，合わせて実装できないのかしら
    interpret_valueをところどころタイポしていた
M-FORWARDを見ると，kappaとkappa'は使い分けていて，かつ，リストと，単項のリスト？の演算は別？　ちょっとここ違和感あるけどテキトー描いているk

  (* M-APPCONT: Return W and continue the computation with the continuation κ' appended to κ *)
            (* M-LET: Handle the let construct *)
  (* M-HANDLE: Handle the handler *)
(* M-OP *)この規則は，四つの場所を持つコンフィギュを返す，この下の二つで，継続を進めるため？？？
(* M-RETTOP: トップレベルでの返り値処理 *)
 (* M-RETCONT: 継続が (rho', x, N) の場合、N の評価を続ける *)　この説明怪しい気がする，だって継続それと違うもん
(* M-RETHANDLER: ハンドラが return x -> M の場合、M を評価 *)













(* ガチメモ 
(Captured) Continuations 
Here, the structure of continuations is enriched.

*)