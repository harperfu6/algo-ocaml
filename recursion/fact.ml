(*末尾再帰でないパターン*)
lec rec fact n =
  if n = 0 then 1 else n * fact (n-1)

(*末尾再帰*)
(*末尾再帰では関数の最後に再帰計算するのではなく，最後(末尾)に計算結果をそのまま返す*)
(*関数の最後に計算を残していないのでスタックしない*)
let fact n =
  let rec facti (n, a) =
    if n = 0 then a else facti (n-1, a*n)
  in
    facti (n,1)
