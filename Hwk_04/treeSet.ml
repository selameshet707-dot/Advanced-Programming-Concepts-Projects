
module TreeSetImplM : Sets.TreeSetS = struct

  type 'a t =
    | Empty
    | Node of 'a * 'a t * 'a t

  (* [empty] represents an empty tree.

     Requires: None

     Example: [empty = Empty]
  *)
  let empty : 'a t = Empty


  (* [elem x t] checks if x is in tree t.

     Requires: None

     Example: [elem 5 (Node (5, Empty, Empty)) = true]
  *)
  let rec elem (x : 'a) (t : 'a t) : bool =
    match t with
    | Empty -> false
    | Node (v, left, right) ->
        if x = v then true
        else if x < v then elem x left
        else elem x right


  (* [insert x t] adds x to tree t if it is
     not already in the tree.

     Requires: None

     Example: [insert 5 Empty =
               Node (5, Empty, Empty)]
  *)
  let rec insert (x : 'a) (t : 'a t) : 'a t =
    match t with
    | Empty -> Node (x, Empty, Empty)
    | Node (v, left, right) ->
        if x = v then t
        else if x < v then
          Node (v, insert x left, right)
        else
          Node (v, left, insert x right)


  (* [min t] finds the smallest value in tree t.

     Requires: None

     Example: [min Empty = None]
  *)
  let rec min (t : 'a t) : 'a option =
    match t with
    | Empty -> None
    | Node (v, Empty, _) -> Some v
    | Node (_, left, _) -> min left


  (* [max t] finds the largest value in tree t.

     Requires: None

     Example: [max Empty = None]
  *)
  let rec max (t : 'a t) : 'a option =
    match t with
    | Empty -> None
    | Node (v, _, Empty) -> Some v
    | Node (_, _, right) -> max right


  (* [is_bst t] checks if t follows the binary
     search tree ordering rules.

     Requires: None

     Example: [is_bst Empty = true]
  *)
  let rec is_bst (t : 'a t) : bool =
    match t with
    | Empty -> true
    | Node (v, left, right) ->
        let left_ok =
          match max left with
          | None -> true
          | Some x -> x < v
        in
        let right_ok =
          match min right with
          | None -> true
          | Some x -> x > v
        in
        left_ok && right_ok &&
        is_bst left && is_bst right


  (* [size t] counts the number of values in tree t.

     Requires: None

     Example: [size Empty = 0]
  *)
  let rec size (t : 'a t) : int =
    match t with
    | Empty -> 0
    | Node (_, left, right) ->
        1 + size left + size right


  (* [height t] finds the height of tree t.

     Requires: None

     Example: [height Empty = 0]
  *)

  let rec height (t : 'a t) : int =
    match t with
    | Empty -> 0
    | Node (_, left, right) ->
        let left_height = height left in
        let right_height = height right in
        if left_height > right_height then
          1 + left_height
        else
          1 + right_height
end


module TreeSetM : Sets.SetS = TreeSetImplM
