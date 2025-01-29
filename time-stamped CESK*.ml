(* Reference: "Systematic abstraction of abstract machines" §2.8 *)

module StringMap = Map.Make(String)
module AddrMap = Map.Make(Int)

(* Object Langugae -untyped lambda calculus(CbV)- *)
type var = string
and lambda = var * term
and term = 
  | TmVar of var
  | TmAbs of lambda
  | TmApp of term * term

(* time-stamped CESK* machine *)
今回は，time-stamp machine stateみたい
なんでtimeなんて入れたんだ〜〜〜〜〜？


(* SYNTAX of time-stamped CESK* machine *)

(* configuration*)
type config = term * env * store * cont * time

and storable = 
  | Clo of lambda * env
  | Cont of cont
(* 一つ前のやつでこう書いている；この最後の行ってOCamlのコードとして正しいかしら． 少し気になっただけかもだが，Cloってこんなふうにofで定義してたっけ．*)

(* Environment *)
and env = addr StringMap.t

(* store *)
and store =  storable AddrMap.t

(* Continuation *)
and cont = 
  | Done
  | Ar of term * env * addr
  | Fn of lambda * env * addr

(* Address *)
and addr = int

(* Time *)
and time = int




(* tests *)




(* type operator *)
type map = string StringMap.t

(* syntactic sugar *)
let (==>) x y = (x, y)  (* tuple *)
let (//) map entries = List.fold_left(fun acc(key, value) -> StringMap.add key value acc) map entries






(* SEMANTICS of time-stamped CESK* machine *)

(* alloc function *)
let alloc (sigma: config): addr =
    let (_, _, s, _, _) = sigma in
    let keys = AddrMap.fold (fun key _ acc -> key :: acc) s [] in
    let max_key = List.fold_left max 0 keys in
    max_key + 1
ここの実装自信がない, configようにちゃんと実装せんと
  (* sから全てのキーを取り出して，そのリストから最大のキーを求め，その最大のキーに１を加えたものをアドレスとして返す *)
  コレクトやevalの代わりかしら？
  


(* tick function *)
let tick (sigma: config): time =
  let (_, _, _, _, t) = sigma in
  t + 1
(* ちょい自信がない．．． *)


  




(* transition relation for the time-stamped CESK* machine *)

let step (sigma: config): config = 
  match sigma with
  | (TmVar x, rho, s, kappa, t) ->
    let Clo(lam, rho') =  StringMap.find x rho in
        (* CESKと同じだが，ここって，String Mapでいいんかいな？あと，Clo＝の右辺のやつ絶対うまく実装できていない気がする．．．StringMap.find x rho でええのか？ *)
    let t' = tick sigma in
    (* これ ς@はシグマで代用して本当にいいの？？？*)
      (TmAbs lam, rho', s, kappa, t')

  | (TmApp (f,e), rho, s, kappa, t) ->
    let a' = alloc sigma in
    (* ここもς@はシグマで代用して本当にいいの？ *)
    let s' = s//[a' ==> Cont kappa] in
    let kappa' = Ar(e, rho, a') in
    let t' = tick sigma in
      (f, rho, s', kappa', t')

  | (TmAbs lam, rho, s, Ar(e, rho', a'), t) ->
    let t' = tick sigma in
      (e, rho', s, Fn(lam, rho, a'), t') 

  | (TmAbs lam, rho, s, Fn((x, e) , rho', a), t) -> 
    let Cont kappa = StringMap.find a s in
    let a' = alloc sigma in
    let t' = tick sigma in
      (e, rho'//[x ==> a'], s//[a' ==> Clo(lam, rho)], kappa, t')

  | _ -> failwith "Invalid configuration"


(* ほんまに，実装の際に，ς@を無視してしまっていいのか？このステップ関数の，シグマで置き換えてるところを全部見直すべきかも *)


