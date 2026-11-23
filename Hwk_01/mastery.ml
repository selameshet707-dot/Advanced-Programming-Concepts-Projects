(* mastery.ml *)

let is_prime_faster (n : int) : bool =
  if n < 0 then
    raise (Failure "invalid input to is_prime")
  else if n < 2 then
    false
  else
    let rec check (i : int) : bool =
      if i * i > n then
        true
      else if n mod i = 0 then
        false
      else
        check (i + 1)
    in
    check 2

let rec big_squares (nums : int list) : int list =
  match nums with
  | [] -> []
  | x :: rest ->
      if x * x >= 50 then
        (x * x) :: big_squares rest
      else
        big_squares rest

let rec sum_of_squares (nums : int list) : int =
  match nums with
  | [] -> 0
  | x :: rest ->
      (x * x) + sum_of_squares rest

let rec product_of_primes (nums : int list) : int =
  match nums with
  | [] -> 1
  | x :: rest ->
      if is_prime_faster x then
        x * product_of_primes rest
      else
        product_of_primes rest

let max_int_string_pairs
    (pairs : (string * int) list) : (string * int) list =
  match pairs with
  | [] ->
      raise (Failure "invalid input to max_int_string_pairs")
  | (_, number) :: rest ->
      let rec find_max
          (biggest : int)
          (remaining : (string * int) list) : int =
        match remaining with
        | [] -> biggest
        | (_, number) :: other ->
            if number > biggest then
              find_max number other
            else
              find_max biggest other
      in
      let biggest = find_max number rest in

      let rec keep_max
          (remaining : (string * int) list)
          : (string * int) list =
        match remaining with
        | [] -> []
        | (name, number) :: other ->
            if number = biggest then
              (name, number) :: keep_max other
            else
              keep_max other
      in
      keep_max pairs