type 'a lb_tree =
  | Leaf of 'a
  | Branch of 'a lb_tree * 'a * 'a lb_tree

let lb_t1 = Leaf 4
let lb_t2 = Branch (Leaf 2, 3, Leaf 4)

let lb_t3 =
  Branch (
    Branch (Leaf 1, 4, Leaf 2),
    5,
    Branch (
      Branch (Leaf 2, 1, Leaf 3),
      3,
      Leaf 1
    )
  )

let lb_t4 =
  Branch (
    Branch (
      Branch (Leaf 2, 2, Leaf 3),
      3,
      Leaf 6
    ),
    5,
    Branch (Leaf 9, 4, Leaf 5)
  )

let rec lb_product (t : int lb_tree) : int =
  match t with
  | Leaf n -> n
  | Branch (left, n, right) ->
      lb_product left * n * lb_product right

let rec lb_square_all (t : int lb_tree) : int lb_tree =
  match t with
  | Leaf n -> Leaf (n * n)
  | Branch (left, n, right) ->
      Branch (
        lb_square_all left,
        n * n,
        lb_square_all right
      )

let rec lb_max_val (t : int lb_tree) : int =
  match t with
  | Leaf n -> n
  | Branch (left, n, right) ->
      max n (max (lb_max_val left) (lb_max_val right))

let rec lb_depth (t : 'a lb_tree) : int =
  match t with
  | Leaf _ -> 1
  | Branch (left, _, right) ->
      1 + max (lb_depth left) (lb_depth right)

let rec lb_path_range (t : 'a lb_tree) : int * int =
  match t with
  | Leaf _ -> (1, 1)
  | Branch (left, _, right) ->
      let (left_min, left_max) = lb_path_range left in
      let (right_min, right_max) = lb_path_range right in
      (1 + min left_min right_min,
       1 + max left_max right_max)
