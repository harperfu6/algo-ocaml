let remove_string c xs =
  List.filter (fun s -> s.[0] <> c) xs

let remove_number n xs =
  List.filter (fun m -> m mod n <> 0) xs

let rec rev_append xs ys =
  match xs with
    [] -> ys
  | x::xsl -> rev_append xsl (x::ys)

let rec any pred = function
    [] -> false
  | x::xs when not (pred x) -> any pred xs 
  | _ -> true

let rec every pred = function
    [] -> true
  | x::xs when pred x -> every pred xs
  | _ -> false
