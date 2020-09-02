(*二分木の定義*)
(*ex: Node(13, 左の木, 右の木))*)
type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

(*データの探索*)
let rec search x = function
    Nil -> false
  | Node (y, _, _) when x = y -> true
  | Node (y, left, _) when x < y -> search x left
  | Node (y, _, right) -> search x right

(*データの挿入*)
let rec insert x = function
    Nil -> Node (x, Nil, Nil)
  | (Node (y, _, _)) as node when x = y -> node (*同じノードがあればそれを返す*)
  | Node (y, left, right) when x < y -> Node (y, (insert x left), right)
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
      if x = y then
        if left = Nil then right
        else if right = Nil then left
        else
          let min_data = search_min right in
          Node (min_data, left, (delete_min right))
      else if x < y then
        Node (y, (delete x left), right)
      else
        Node (y, left, (delete x right))

(*二分木の巡回*)
let rec iter f = function
    Nil -> ()
  | Node (x, left, right) -> iter f left; f x; iter f right (*左の子 < 節のデータ < 右の子*)
