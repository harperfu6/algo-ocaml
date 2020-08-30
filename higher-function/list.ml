let rec make_list x n =
  if n = 0 then []
  else x :: make_list x (n-1)
(*パターンマッチング*)
let rec make_list x n =
  match n with
    0 -> []
  | n -> x :: make_list x (n-1) 

let rec iota n m =
  if n > m then []
  else n :: iota (n+1) m

let rec take xs n =
  if n = 0 then []
  else List.hd xs :: take (List.tl xs) (n-1)
(*パターンマッチング*)
let rec take xs n =
  match (n, xs) with
    (0, _) | (_, []) -> []
  | (_, y::ys) -> y :: take ys (n-1)

let rec drop xs n =
  if n = 0 then xs
  else drop (List.tl xs) (n-1)
(*パターンマッチング*)
let rec drop xs n =
  match (n, xs) with
    (0, _) -> xs
    (_, []) -> []
  | (_, _::ys) -> drop ys (n-1)

let rec zip xs ys =
  if xs = [] || ys = [] then []
  else (List.hd xs, List.hd ys) :: zip (List.tl xs) (List.tl ys)
(*パターンマッチング*)
let rec zip xs ys =
  match (xs, ys) with
    ([], _) | (_, []) -> []
  | (x:xsl, y::ysl) -> (x, y) :: zip xsl ysl

let rec unzip xs =
  if xs = [] then ([],[])
  else let (y, z) = List.hd xs in
       let (ys, zs) = unzip (List.tl xs) in (y::ys, z::zs)
(*パターンマッチング*)
let rec unzip xs =
  match xs with
    [] -> ([],[])
  | (x,y)::zs -> let (xs, ys) = unzip zs in (x::xs, y::ys)


