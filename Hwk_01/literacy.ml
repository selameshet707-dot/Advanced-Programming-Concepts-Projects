let rec first_digit (n : int) : int =
  if n < 0 then
    first_digit (-n)
  else if n < 10 then
    n
  else
    first_digit (n / 10)

let rec product_to (n : int) : int =
  if n < 0 then
    raise (Failure "invalid input to product_to")
  else if n = 0 then
    0
  else if n = 1 then
    1
  else
    n * product_to (n - 1)

let is_prime (n : int) : bool =
  if n < 0 then
    raise (Failure "invalid input to is_prime")
  else if n < 2 then
    false
  else
    let rec check (d : int) : bool =
      if d = n then
        true
      else if n mod d = 0 then
        false
      else
        check (d + 1)
    in
    check 2

let rec last_digits (nums : int list) : int list =
  match nums with
  | [] -> []
  | x :: rest ->
      (abs x mod 10) :: last_digits rest

let rec all_primes (nums : int list) : int list =
  match nums with
  | [] -> []
  | x :: rest ->
      if is_prime x then
        x :: all_primes rest
      else
        all_primes rest

let rec sum_pairs (pairs : (int * int) list) : int =
  match pairs with
  | [] -> 0
  | (a, b) :: rest ->
      a + b + sum_pairs rest

let rec max_pairs (pairs : (int * int) list) : int =
  match pairs with
  | [] -> raise (Failure "invalid input to max_pairs")
  | (a, b) :: rest ->
      let rec find_max (current : int) (remaining : (int * int) list) : int =
        match remaining with
        | [] -> current
        | (x, y) :: other ->
            let biggest = if x > current then x else current in
            let biggest = if y > biggest then y else biggest in
            find_max biggest other
      in
      let biggest = if a > b then a else b in
      find_max biggest rest