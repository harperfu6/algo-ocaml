(*OCamlは異なるデータ型をリストに格納でいないので，データ型を定義する*)
type term = Plus | Minus | Num of int

(*
  1 + 2 + 3 - 4 + 5 + 6 + 78 + 9
  => [Num 1; Plus; Num 2; Plus; Num 3; Minus; Num 4; Plus;
      Num 5; Plus; Num 6; Plus; Num 78; Plus; Num 9]
 *)

(*式を生成する*)
(*可能性のあるパターンをリストの逆順に生成する*)
(*
[Num 1] => [Num 2, Plus, Num 1]
        => [Num 2, Minus, Num 1]
        => [Num 12]
 *)

(*式の計算*)
let calc_expr expr =
  let rec calc_expr_sub expr a =
    match expr with
      [] -> a
    | Plus :: Num x :: xs -> calc_expr_sub xs (a + x)
    | Minus :: Num x :: xs -> calc_expr_sub xs (a - x)
    | _ -> raise (Failure "calc_expr_sub")
  in
    match expr with
        Num x :: xs -> calc_expr_sub xs x
      | _ -> raise (Failure "calc_expr")

(*式の表示*)
let rec print_expr = function
    [] -> print_string " = 100\n"
  | Num x :: xs -> print_int x; print_expr xs
  | Plus :: xs -> print_string " + "; print_expr xs
  | Minus :: xs -> print_string " - "; print_expr xs

(*式の組み立て*)
let rec make_expr n expr =
  if n = 10 then
    let expr1 = List.rev expr in
    if calc_expr expr1 = 100 then print_expr expr1 else ()
  else
    match expr with
      Num x::xs ->
        make_expr (n+1) (Num n::Plus::expr);
        make_expr (n+1) (Num n::Minus::expr);
        make_expr (n+1) (Num (x*10+n)::xs);
    | _ -> raise (Failure "make_expr")

