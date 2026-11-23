
module TreeSetM : Sets.SetS = struct

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

end
