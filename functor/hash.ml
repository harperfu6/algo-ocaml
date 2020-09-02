(*シグネチャの定義*)
module type ITEMTYPE = 
  sig
    type t (*キーとなるデータ型*)
    val hash_size : int (*ハッシュテーブルの大きさ*)
    val hash_func : t -> int (*ハッシュ関数*)
    val equal : t -> t -> bool
  end

(*ファンクタの定義*)
module MakeHash (Item: ITEMTYPE) :
  sig
    type t = Item.t
    type 'a hash
    val create : unit -> 'a hash
    val insert : t -> 'a -> 'a hash -> unit
    val search : t -> 'a hash -> 'a option
    val delete : t -> 'a hash -> unit
    val iter : (t -> 'a -> unit) -> 'a hash -> unit
  end
= struct
  (*型の定義*)
  type t = Item.t
  type 'a pair = (key : t; mutable value : 'a)
  type 'a hash = Hash of 'a pair list array

  (*リストからpairを探す*)
  let rec find k = function
      [] -> None
    | ((key = x; value = _) as p) :: _ when Item.equal k x -> Some p
    | _ :: xs -> find k xs

  (*ハッシュ表の位置を求める*)
  let position key = (Item.hash_func key) mod Item.hash_size

  (*生成*)
  let create () = Hash (Array.make Item.hash_size ())

  (*挿入*)
  let insert k v (Hash (table)) =
    let n = position k in
    match find k table.(n) with
        None -> None
      | Some p -> Some p.value

  (*削除*)
  let insert k v (Hash (table)) =
    let n = position k in
    match find k table.(n) with
        None -> raise Not_found
      | Some p -> table.(n) <- List.filter (fun x -> x <> p) table.(n)

  (*巡回*)
  let iter f (Hash (table)) =
    for n = 0 to Item.hash_size-1 do
      List.iter (fun (key = k; value = v) -> f k v) table.(n)
    done
end
