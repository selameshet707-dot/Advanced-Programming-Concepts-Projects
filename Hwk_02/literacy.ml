(* literacy.ml *)

type 'a zom_tree =
  | Zero
  | One of 'a
  | Many of 'a zom_tree * 'a zom_tree

let zom_t0 = Zero
let zom_t1 = One 4
let zom_t2 = Many (One 2, One 4)

let zom_t3 =
  Many (
    Many (One 1, One 2),
    Many (
      Many (One 2, One 3),
      Zero
    )
  )

let zom_t4 =
  Many (
    Many (
      Many (One 2, One 3),
      One 6
    ),
    Many (One 9, Zero)
  )

let max_int_string_pairs
    (pairs : (string * int) list) : (string * int) list =
  let rec find_max
      (remaining : (string * int) list)
      (best : int option)
      (result : (string * int) list)
      : (string * int) list =
    match remaining with
    | [] -> result
    | (name, n) :: rest ->
        match best with
        | None -> find_max rest (Some n) [(name, n)]
        | Some m when n > m ->
            find_max rest (Some n) [(name, n)]
        | Some m when n = m ->
            find_max rest best ((name, n) :: result)
        | Some _ ->
            find_max rest best result
  in
  find_max pairs None []

let rec zom_product (t : int zom_tree) : int =
  match t with
  | Zero -> 1
  | One n -> n
  | Many (left, right) ->
      zom_product left * zom_product right

let rec zom_square_all (t : int zom_tree) : int zom_tree =
  match t with
  | Zero -> Zero
  | One n -> One (n * n)
  | Many (left, right) ->
      Many (
        zom_square_all left,
        zom_square_all right
      )

let rec zom_max_val (t : int zom_tree) : int option =
  match t with
  | Zero -> None
  | One n -> Some n
  | Many (left, right) ->
      match (zom_max_val left, zom_max_val right) with
      | (None, None) -> None
      | (Some n, None) -> Some n
      | (None, Some n) -> Some n
      | (Some n, Some m) -> Some (max n m)

let rec zom_depth (t : 'a zom_tree) : int =
  match t with
  | Zero -> 0
  | One _ -> 1
  | Many (left, right) ->
      1 + max (zom_depth left) (zom_depth right)

let rec zom_path_range (t : 'a zom_tree) : int * int =
  match t with
  | Zero -> (0, 0)
  | One _ -> (1, 1)
  | Many (left, right) ->
      let (left_min, left_max) = zom_path_range left in
      let (right_min, right_max) = zom_path_range right in
      (1 + min left_min right_min,
       1 + max left_max right_max)