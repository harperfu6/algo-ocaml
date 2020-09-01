(*
 * 単純に考えると64×63×62...=178462987637760通りとなってしまうが，
 * 同じ行に重複しないので，
 * 列を用意してあげてあとは行の位置が重複しないようにしていけば8×7×..=40320通りとなる
 *   1  2  3  4  5  6  7  8    <--- 列の位置
 *   ---------------------------
 *  [1, 7, 5, 8, 2, 4, 6, 3]   <--- 要素が行の位置を表す 
 *)

(*盤面の表示*)
let rec print_board = function
    [] -> print_newline ()
  | x::xs -> print_int x; print_string " "; print_board xs

(*要素の削除*)
let rec remove n xs =
  List.filter (fun x -> x <> n) xs

(*衝突の検出*)
let attack x xs =
  let rec attack_sub x n = function
      [] -> true
    (*斜めの判定*)
    (*列が遠ざかる(ys)と斜め判定とする行(n)も増える*)
    | y::ys -> if x = y + n || x = y - n then false
               else attack_sub x (n+1) ys
  in
    attack_sub x 1 xs

(*8クイーンの条件を満たしているか安全確認*)
let rec safe = function
    [] -> true
  | x::xs -> if attack x xs then safe xs else false

(*単純な生成検定法*)
(*バックトラック*)
let rec queen f nums board =
  if nums = [] then
    if safe board then f board else ()
  else
    List.iter (fun x -> queen f (remove x nums) (x::board)) nums

(*バックトラックはクイーンの数が増えると非効率であることがわかる*)
(*解を生成する途中でattackをかけて満たさないものは途中で止める->枝刈り*)
let rec queen_fast f nums board =
  if nums = [] then f board
  else List.iter (fun x ->
    if attack x board then queen_fast f (remove x nums) (x::board) else ()) nums


(*クイーンの数を増やすN-Queen Problemとする*)
(*計算時間の違いを計測*)

(*整数列の生成*)
let rec iota n m =
  if n > m then []
  else n :: iota (n+1) m

let test_queen f n =
  let c = ref 0 in (*代入するための参照*)
  let s = Sys.time () in
  f (fun _ -> c := !c + 1) (iota 1 n) []; (*queenを回す際に解を表示(print_board)するのではなく個数を計上する*)
  print_float (Sys.time () -. s);
  !c (*解の個数の表示*)
