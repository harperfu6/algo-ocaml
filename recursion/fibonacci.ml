(*末尾再帰しない場合*)
let rec fibonacci n =
  if n < 2 then n
  else fibonacci (n-1) + fibonacci (n-2)

(*末尾再帰したもの*)
let fibonacci n =
  let rec fibo (n, a1, a2) =
    if n = 0 then a1 else fibo (n-1, a1+a2, a1)
  in
    fibo (n, 1, 0)
