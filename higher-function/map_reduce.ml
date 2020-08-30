let rec tabulate fn s e =
  if s > e then []
  else fn s :: tabulate fn (s+1) e

let rec foreach fn = function
    [] -> ()
  | x::xs -> begin
              fn x;
              foreach fn xs
             end

let rec take_while pred = function
    x::xs when pred x -> x :: take_while pred xs
  | _ -> []

let rec drop_while pred = function
    x::xs when pred x -> drop_while pred xs
  | xs -> xs 

let rec scan_left fn a = function
  [] -> [a]
  | x::xs -> a :: scan_left fn (fn a x) xs

let rec scan_right fn a = function
    [] -> [a]
  | x::xs -> let ys = scan_right fn a xs in fn x (List.hd ys) :: ys



