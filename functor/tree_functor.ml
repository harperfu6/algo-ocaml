(*ファンクタ版*)
(*シグネチャ=このシグネチャを使うファンクタに渡すモジュールが満たすべき仕様*)
module type ItemType = sig
  type t
  val compare : t -> t -> int (* <は-1, =は0, >は1とする*)
end


(*下記ファンクたの使い方*)
(*整数型を格納する二分木*)
(*モジュールの定義: module 名前 = struct ... end*)
(*# module IntTree = MakeTree(struct type t = int let compare x y = x - y end);;*)

(*二分木の定義*)
(*ファンクタのMakeTreeの定義*)
module MakeTree(Item: ItemType) = struct (*モジュールItemの型はItemTypeで上記シグネチャで定義*)
  (*ex: Node(13, 左の木, 右の木))*)
  (*type 'a tree = Nil | Node of 'a * 'a tree * 'a tree*)
  type tree = Nil | Node of Item.t * tree * tree (*型はシグネチャ定義されたもの*)

  (*空の木*)
  let create = Nil
  
  (*データの探索*)
  let rec search x = function
      Nil -> None
    (*| Node (y, _, _) when x = y -> true*)
    (*| Node (y, left, _) when x < y -> search x left*)
    (*比較演算はシグネチャ定義されたもの*)
    | Node (y, _, _) when Item.compare x y = 0 -> Some y
    | Node (y, left, _) when Item.compare x y < 0 -> search x left
    | Node (y, _, right) -> search x right
  
  (*データの挿入*)
  let rec insert x = function
      Nil -> Node (x, Nil, Nil)
    (*| (Node (y, _, _)) as node when x = y -> node (*同じノードがあればそれを返す*)*)
    (*| Node (y, left, right) when x < y -> Node (y, (insert x left), right)*)
    | (Node (y, _, _)) as node when Item.compare x y = 0 -> node (*同じノードがあればそれを返す*)
    | Node (y, left, right) when Item.compare x y < 0 -> Node (y, (insert x left), right)
    | Node (y, left, right) -> Node (y, left, (insert x right))
  
  (*データの削除*)
  (*最小値を求める*)
  let rec search_min = function
      Nil -> raise (Failure "search_min")
    | Node (x, Nil, _) -> x (*左木がnilの場合はそのノードは最小値*)
    | Node (_, left, _) -> search_min left
  
  (*最小値を削除する*)
  let rec delete_min = function
      Nil -> raise (Failure "delete_min")
    | Node (x, Nil, right) -> right (*左木がnilの場合は右木を返すことでノードが削除される*)
    | Node (x, left, right) -> Node (x, (delete_min left), right)
  
  (*データの削除*)
  let rec delete x = function
      Nil -> raise Not_found
    | Node (y, left, right) ->
        if Item.compare x y = 0 then
          if left = Nil then right
          else if right = Nil then left
          else
            let min_data = search_min right in
            Node (min_data, left, (delete_min right))
        else if Item.compare x y < 0 then
          Node (y, (delete x left), right)
        else
          Node (y, left, (delete x right))
  
  (*二分木の巡回*)
  let rec iter f = function
      Nil -> ()
    | Node (x, left, right) -> iter f left; f x; iter f right (*左の子 < 節のデータ < 右の子*)
end
