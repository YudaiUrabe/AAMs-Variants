(* Reference: "Systematic abstraction of abstract machines" §2.4*)

module StringMap = Map.Make(String)
module AddrMap = Map.Make(Int)

type var = string
and lambda = var * term
and term = 
  | TmVar of var
  | TmAbs of lambda
  | TmApp of term * term

(* CESK Machine *)
(* Adding a store to CEK machine and use it allocate variable bindings,
 thereby eliminating recursion from the environment in CEK machine*)

(* SYNTAX of CESK machine *)

(* configuration*)
type config = term * env * store * cont

(* Environment *)
and env = addr StringMap.t

and storable = Clo of lambda * env

(* store
 is a finite map from address to storable values *)
 and store =  storable AddrMap.t

(* Continuation *)
and cont = 
  | Done
  | Ar of term * env * cont
  | Fn of lambda * env * cont

(* Address *)
and addr = int




(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries
(* /// operator for AddrMap *)
let (///) map entries = List.fold_left(fun acc(key, value) -> AddrMap.add key value acc) map entries







(* SEMANTICS of CESK machine *)

(* alloc function *)
(* Extract all keys from s, find the maximum key in the list,
 and return the maximum key plus one as the new address. *)
 let alloc (s: store): addr =
  let keys = AddrMap.fold (fun key _ acc -> key :: acc) s [] in
  let max_key = List.fold_left max 0 keys in
  max_key + 1
(* ここのアロケーション関数の実装面の理解まだ足りていないような．．． *)

(* transition relation for the CESK machine *)
let step (sigma: config): config = 
  match sigma with
  | (TmVar x, rho, sigma_store, kappa) ->
    let addr = StringMap.find x rho in (* (ρ!x) *)
    let Clo(lam, rho') = AddrMap.find addr sigma_store in
     (TmAbs lam, rho', sigma_store, kappa)
(* この上の実装，多分あっているんだが，OCamlの話かもだけど，なんでこれでうまくいくんだろうね．Clo inみたいなのってどうやって処理するんや？ *)
  | (TmApp (f,e), rho, sigma_store, kappa) ->
      (f, rho, sigma_store, Ar(e, rho, kappa))
  | (TmAbs lam, rho, sigma_store, Ar(e, rho', kappa)) ->
      (e, rho', sigma_store, Fn(lam, rho, kappa)) 
(* 引数を評価するため継続を展開??? *)
  | (TmAbs lam, rho, sigma_store, Fn((x, e) , rho', kappa)) -> 
      let a' = alloc sigma_store 
      in (e, rho'//[x ==> a'], sigma_store///[a' ==> Clo(lam, rho)], kappa)
  | _ -> failwith "Invalid configuration"

(* injection function 
combines the term with the empty env, store and continuation*)
let inject (e:term) : config =
 (e, StringMap.empty, AddrMap.empty, Done)

(* isFinal *)
let isFinal (state: config) : bool =
  match state with
    |(TmAbs _, _, _, Done) -> true
    | _ -> false

(* collect *)
let rec collect (f: config -> config) (isFinal: config-> bool)(sigma_collect: config): config list =
  if isFinal sigma_collect then
    [sigma_collect]
  else
    sigma_collect :: collect f isFinal (f sigma_collect)

(* evaluation function *)
let evaluate (e: term): config list =
  collect step isFinal(inject e)


(* test 
(λa.a)(λb.b) -> (λb.b)　*)
let term_test = TmApp (TmAbs ("a", TmVar "a"), TmAbs ("b", TmVar "b"))

let result = evaluate term_test

let rec string_of_term (t: term) =
  match t with
  | TmVar x -> x
  | TmAbs (x, t) -> "λ" ^ x ^ "." ^ string_of_term t
  | TmApp (t1, t2) -> "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"

(* output *)
let () = 
  List.iter (fun (term_test, _, _, _) -> 
    Printf.printf "State: %s\n" (string_of_term term_test)
  ) result


  
  次開いた時にテストを通した方が良いでしょう2025/01/22
→　2025/01/29
  （状態遷移関数が肝だろうね）
  ステップの特にcloが関係するところを治す，alloc funcの確認　？
  一応テストを通した．　次は，理解を深めて上の日本語を消し去る（あと，この下のコメントも片づける）



  

(*

  (* 元々はこうなっていたけど変えた，let inject (e:term) : config =
    let (rho0, s0) = (StringMap.empty, StringMap.empty) in (e, rho0, s0, Done) *)

      List.iter (fun (term_test, _, _, _) -> の箇所で，組の個数をちゃんと変えた．

CESKでは環境と閉包が相互再起ではない？

ストリングマップは，連想配列として動作する？？？
　こいつを理解したら，実装も進みそうな予感がしている．

Farg(term, env, cont)は，関数を受け取り，その関数を，termをenvのもとで評価した結果に適用し，
その結果を継続計算contに渡す継続計算を表す　　みたい

envは，  クロージャの代わりにアドレスに飛ばすんやね

なんで，injectのところの二つ目のs０に赤線が引かれるんやろね．

あれ，ここではStorableが値なのかしら？

StringMap.find x rhoとかAddrMap.find addr sigma_storeとかAddrMap.findみたいなやつの使い方がいまいちピンときていないのかも．
    (* ここって，String Mapでいいんかいな？あと，Clo＝の右辺のやつ絶対うまく実装できていない気がする．．．StringMap.find x rho でええのか？ *)と思っていた


allocの実装に自信がないので，AAMや抽象機械の仕組みを理解した上で再度確認する．
Prof. G.Weiはevalやcollectを書いていたけど，ここでは不要なのかしら？
allocは，sから全てのキーを取り出して，そのリストから最大のキーを求め，その最大のキーに１を加えたものをアドレスとして返す
  こいつが，コレクトやevalの代わりかしら？　こいつが何をしているのがちゃんと理解せよ！！！

(* 変数の処理：環境からアドレスを取得し、ストアからそのアドレスを参照する *)
(* アプリケーションの処理：関数部分の項をそのまま次の評価に渡し、引数を評価するために新しい継続を作成 *)
(* ラムダ式の処理：ラムダ式が関数として適用されるのを待つ *)
    (* 関数適用を行う場合、束縛の更新 *)

*)

(* 閉包はないのかしら，いや呼称しないだけかもで，storableがCEKにおける閉包の代わりな気がする *)