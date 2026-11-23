
let squared_list : int list -> int list =
  List.map (fun x -> x * x)

let list_evens : int list -> int list =
  List.filter (fun x -> x mod 2 = 0)

let list_product : int list -> int =
fun lst -> List.fold_right ( * ) lst 1

type 'a ltree =
  | Leaf of 'a
  | Fork of 'a ltree * 'a ltree

let rec ltree_reduce :
    ('a -> 'b) -> ('b -> 'b -> 'b) -> 'a ltree -> 'b =
  fun f_leaf f_fork tree ->
    match tree with
    | Leaf x -> f_leaf x
    | Fork (left, right) ->
        f_fork
          (ltree_reduce f_leaf f_fork left)
          (ltree_reduce f_leaf f_fork right)

let ltree_map : ('a -> 'b) -> 'a ltree -> 'b ltree =
  fun f tree ->
    ltree_reduce
      (fun x -> Leaf (f x))
      (fun left right -> Fork (left, right))
      tree

let squared_ltree : int ltree -> int ltree =
  ltree_map (fun x -> x * x)

let ltree_product : int ltree -> int =
  ltree_reduce (fun x -> x) (fun left right -> left * right)

  
let _ =
  assert (squared_list [] = []);
  assert (squared_list [1; 2; 3] = [1; 4; 9]);
  assert (list_evens [] = []);
  assert (list_evens [1; 2; 3] = [2]);
  assert (list_product [] = 1);
  assert (list_product [1; 2; 3] = 6);

  assert (
    ltree_reduce (fun v -> v * 2) (fun l r -> l + r)
      (Fork (Fork (Leaf 1, Leaf 2), Leaf 3))
    = 12
  );

  assert (
    ltree_reduce (fun v -> v) (fun l r -> l ^ " " ^ r)
      (Fork (Leaf "Hello", Leaf "world"))
    = "Hello world"
  );

  assert (ltree_map (fun x -> x * 2) (Leaf 5) = Leaf 10);

  assert (
    ltree_map (fun x -> x * 2)
      (Fork (Fork (Leaf 1, Leaf 2), Leaf 3))
    = Fork (Fork (Leaf 2, Leaf 4), Leaf 6)
  );

  assert (
    ltree_map Char.uppercase_ascii
      (Fork (Fork (Leaf 'a', Leaf '3'), Leaf 'g'))
    = Fork (Fork (Leaf 'A', Leaf '3'), Leaf 'G')
  );

  assert (squared_ltree (Leaf 5) = Leaf 25);

  assert (
    squared_ltree
      (Fork (Fork (Leaf 1, Leaf 2), Fork (Leaf 3, Leaf 4)))
    = Fork (Fork (Leaf 1, Leaf 4), Fork (Leaf 9, Leaf 16))
  );

  assert (ltree_product (Leaf 5) = 5);

  assert (
    ltree_product
      (Fork (Fork (Leaf 1, Leaf 2), Fork (Leaf 3, Leaf 4)))
    = 24
  )
