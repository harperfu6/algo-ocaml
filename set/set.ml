(*シグネチャの定義*)
module type ITEMTYPE =
  sig
    type t (*要素の型*)
    val compare : t -> t -> int
  end

(*ファンクタの定義*)
module MakeSet(Item: ITEMTYPE) :
  sig
    type set (*集合の型*)
    val create : unit -> set
    val member : Item.t -> set -> bool
    val insert : Item.t -> set -> set
    val delete : Item.t -> set -> set
    val union : set -> set -> set
    val intersection : set -> set -> set
    val difference : set -> set -> set
    val is_subset : set -> set -> bool
    val is_equal : set -> set -> bool
    val iter : (Item.t -> unit) -> set -> unit
    val set_of_list : Item.t list -> set
    val list_of_set : set -> Item.t list
  end
= struct
    type set = S of Item.t list

    let create () = S []

    (*モジュール内で使用する関数: リストの中からxと等しい要素があればtrue*)
    let rec mem_eq n = function
        [] -> false
      | x::xs -> if Item.compare n x = 0 then true else mem_eq n xs

    let member n (S ls) = mem_eq n ls

    let insert n (S ls) =
      if mem_eq n ls then S ls else S (n::ls)

    let delete n (S ls) =
      if mem_eq n ls
      then S (List.filter (fun x -> Item.compare n x <> 0) ls)
      else raise Not_found

    (*部分集合判定*)
    (*ls1の要素が全てls2に含まれていたらls1はls2の部分集合である*)
    let is_subset (S ls1) (S ls2) =
      let rec iter = function
          [] -> true
        | x::xs -> if mem_eq x ls2 then iter xs else false
      in
        iter ls1

    (*同値判定*)
    let is_equal s1 s2 =
      is_subset s1 s2 && is_subset s2 s1

    (*和集合*)
    let union (S ls1) (S ls2) =
      let rec _union = function
          [] -> ls2
        | x::xs when mem_eq x ls2 -> _union xs
        | x::xs -> x::(_union xs)
      in
        S (_union ls1)

    (*積集合*)
    let intersection (S ls1) (S ls2) =
      let rec _inter = function
          [] -> []
        | x::xs when mem_eq x ls2 -> x::_inter xs
        | _::xs -> _inter xs
      in
        S (_inter ls1)

    (*集合の差分*)
    let difference (S ls1) (S ls2) =
      let rec _diff = function
          [] -> []
        | x::xs when mem_eq x ls2 -> _diff xs
        | x::xs -> x::(_diff xs)
      in
        S (_diff ls1)


    let iter f (S ls) = List.iter (fun x -> f x) ls

    let set_of_list ls =
      List.fold_right (fun x y -> insert x y) ls (S [])

    let list_of_set (S ls) = ls
  end
