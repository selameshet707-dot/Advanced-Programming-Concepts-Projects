(* mastery.ml *)

type 'a lb_tree =
  | Leaf of 'a
  | Branch of 'a lb_tree * 'a * 'a lb_tree

let lb_t1 : int lb_tree = Leaf 4
let lb_t2 : int lb_tree = Leaf 2
let lb_t3 : int lb_tree = Branch (Leaf 2, 3, Leaf 4)
let lb_t4 : string lb_tree = Branch (Leaf "2", "3", Leaf "4")

let lb_t5 : int lb_tree =
  Branch (
    Branch (Leaf 1, 4, Leaf 2),
    5,
    Branch (
      Branch (Leaf 2, 1, Leaf 3),
      3,
      Leaf 1
    )
  )

let lb_t6 : string lb_tree =
  Branch (
    Branch (Leaf "1", "4", Leaf "2"),
    "5",
    Branch (
      Branch (Leaf "2", "1", Leaf "3"),
      "3",
      Leaf "1"
    )
  )

let rec list_zip_exn
    (xs : 'a list)
    (ys : 'b list) : ('a * 'b) list =
  match (xs, ys) with
  | ([], []) -> []
  | (x :: xt, y :: yt) ->
      (x, y) :: list_zip_exn xt yt
  | _ ->
      raise (Invalid_argument "lists not the same size")

let rec list_zip_opt
    (xs : 'a list)
    (ys : 'b list) : ('a * 'b) list option =
  match (xs, ys) with
  | ([], []) -> Some []
  | (x :: xt, y :: yt) ->
      (match list_zip_opt xt yt with
       | None -> None
       | Some rest ->
           Some ((x, y) :: rest))
  | _ -> None

let rec list_unzip
    (pairs : ('a * 'b) list) : 'a list * 'b list =
  match pairs with
  | [] -> ([], [])
  | (x, y) :: rest ->
      let (xs, ys) = list_unzip rest in
      (x :: xs, y :: ys)

let rec lb_tree_zip_exn
    (t1 : 'a lb_tree)
    (t2 : 'b lb_tree) : ('a * 'b) lb_tree =
  match (t1, t2) with
  | (Leaf x, Leaf y) ->
      Leaf (x, y)
  | (Branch (l1, x, r1), Branch (l2, y, r2)) ->
      Branch (
        lb_tree_zip_exn l1 l2,
        (x, y),
        lb_tree_zip_exn r1 r2
      )
  | _ ->
      raise (Invalid_argument "trees not the same shape")

let rec lb_tree_zip_opt
    (t1 : 'a lb_tree)
    (t2 : 'b lb_tree) : ('a * 'b) lb_tree option =
  match (t1, t2) with
  | (Leaf x, Leaf y) ->
      Some (Leaf (x, y))
  | (Branch (l1, x, r1), Branch (l2, y, r2)) ->
      (match (
         lb_tree_zip_opt l1 l2,
         lb_tree_zip_opt r1 r2
       ) with
       | (Some left, Some right) ->
           Some (Branch (left, (x, y), right))
       | _ -> None)
  | _ -> None

let rec lb_tree_unzip
    (t : ('a * 'b) lb_tree) : 'a lb_tree * 'b lb_tree =
  match t with
  | Leaf (x, y) ->
      (Leaf x, Leaf y)
  | Branch (left, (x, y), right) ->
      let (left1, left2) = lb_tree_unzip left in
      let (right1, right2) = lb_tree_unzip right in
      (Branch (left1, x, right1),
       Branch (left2, y, right2))