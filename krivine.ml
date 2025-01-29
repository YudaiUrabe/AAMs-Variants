(* Reference: "Systematic abstraction of abstract machines" §4, https://github.com/Kraks/playground/blob/master/haskell/aam/krivine.hs *)

module StringMap = Map.Make(String)
module AddrMap = Map.Make(Int)

type var = string
and lambda = var * term
and term = 
  | TmVar of var
  | TmAbs of lambda
  | TmApp of term * term


(* call-by-name Krivine Machine *)
(* SYNTAX of Krivine machine *)

(* configuration *)
type config = term * env * store * cont
  
(* Environment *)
and env = addr StringMap.t

and storable = 
  | Thunk of term * env
  | Clo of lambda * env

(* store *)
and store =  storable StringMap.t

(* Continuation *)
and cont = 
  | Done
  | C1 of addr * cont
  | C2 of addr * cont
  
(* Address *)
and addr = int


(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries









(* SEMANTICS of Krivine's machine *)

(* transition relation for the Krivine's machine *)
let step (sigma: config): config = 
  match sigma with
  | (TmVar x, rho, s, kappa) ->
    (match StringMap.find (StringMap.find x rho) s with
    | Thunk (e, rho') -> 
      (e, rho', s, C1 (StringMap.find x rho, kappa))
    | Clo (lam, rho') -> 
      (TmAbs lam, rho', s, kappa)) (* ここってTmAbsってlamの前に書かなあかんの？*)
    (* match内matchって，括弧をつけなくてはならないの？？？　rho(x)はStringMap.find x rho で表している． *)
  | (TmApp (e0,e1), rho, s, kappa) ->
    let a = StringMap.cardinal s in
    let s' = s // [a ==> Thunk(e1, rho)] in
    (e0, rho, s', C2 (a, kappa))
(* この上の実装自信ない．．．StringMap.cardinal ってなんだよ． e0 とe1ってこれであってる？*)
  | (TmAbs lam, rho, s, C1(a, kappa)) ->
    (lam, rho, s // [a ==> Clo(lam, rho)], kappa)
  | (TmAbs (x, e), rho, s, C2(a, kappa)) -> 
    (e,rho // [x ==> a], s, kappa)
  | _ -> failwith "Invalid configuration"

(* alloc function *)
 let alloc (s: store): addr =
  let keys = AddrMap.fold (fun key _ acc -> key :: acc) s [] in
  let max_key = List.fold_left max 0 keys in
  max_key + 1
(* CESKと全く同じ実装だが，元々の方が自信がないからな．．． *)

(* injection function *)
let inject (e:term) : config =
    (e, StringMap.empty, AddrMap.empty, Done)


(* collect *)
let rec collect (f: config -> config) (isFinal: config-> bool)(sigma_collect: config): config list =
  if isFinal sigma_collect then
    [sigma_collect]
  else
    sigma_collect :: collect f isFinal (f sigma_collect)

(* isFinal *)
let isFinal (sigma_state: config) : bool =
  match sigma_state with
    |(TmAbs _, _, _, Done) -> true
    | _ -> false


(* evaluation function *)
let evaluate (e: term): config list =
  collect step isFinal(inject e)

  


(*　test *)


(* 
次のアクションアイテム2025/01/22：　遷移関数ほとんどオッケーだと思っているけど，遷移関数以外にもところどころ赤線が出てしまっているよね（多分，型の問題な気がするから，今回のコンフィギュとか各要素を見ながら見直したら解決しそうな気もする．）．この赤線をなくすのがお仕事かな．あとは，日本語を全部消せるようにして，かつ，テストを通して確認する．

この機会ってロジック寄りなの？？？ウィキによると，The Krivine machine explains how to compute a recursive function.だってよ

type k :-> v = Data.Map.Map k vに相当するものが必要かもしれん？構文定義にか？？どこで使ったやつやろか　→　いらんのっちゃう？環境の定義のことだろう多分．

*)
