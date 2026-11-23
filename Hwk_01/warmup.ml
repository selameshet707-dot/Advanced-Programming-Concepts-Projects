let dec (n: int) : int =
    n - 1

let cube (n : int) : int =
  n * n * n

let last_digit(n : int) : int =
  let abs_int : int = abs n in
  abs_int mod 10

let sum(numbers : int list) : int = 
  let rec add_numbers(numbers : int list) : int =
    match numbers with
    | [] -> 0
    | first_number :: rest -> first_number + add_numbers rest
  in 
  add_numbers numbers

let product(numbers : int list) : int = 
  let rec multiply_numbers(numbers : int list) : int =
    match numbers with
    | [] -> 1
    | first_number :: rest -> first_number * multiply_numbers rest
  in
  multiply_numbers numbers

let all_odds (numbers : int list) : int list =
  let rec find_odds (numbers : int list) : int list =
    match numbers with
    | [] -> []
    | first_number :: rest ->
        if first_number mod 2 <> 0 then
          first_number :: find_odds rest
        else
          find_odds rest
  in
  find_odds numbers

let square_all (numbers : int list) : int list =
  let rec square_numbers (numbers : int list) : int list =
    match numbers with
    | [] -> []
    | first_number :: rest ->
        (first_number * first_number) :: square_numbers rest
  in
  square_numbers numbers