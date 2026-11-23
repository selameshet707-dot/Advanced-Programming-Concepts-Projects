let dec (n: int) : int =
    n - 1

let postive (n: int) : bool = 
   n >= 0 
let rec decs lst = 
  match lst with 
  | [] -> [] (* if the list is empty return empty list*)
  | x :: xs -> dec x :: decs xs (* :: splits the lsit, then -> says call dec on x and decs on xs*)

let rec dec_postive lst = 
  match lst with
  | [] -> []
  | x :: xs -> 
  if 
      postive(dec x) then dec x :: dec_postive xs
  else 
    dec_postive xs

let negate x =
  x - (2 * (x))

let odd x = 
  x mod 2 <> 0

let rec negate_odd lst = 
  match lst with 
  |[] -> []
  |x :: xs ->
  if(odd(negate(x))) then negate(x) :: negate_odd xs
  
  else 
    negate_odd xs

let rec negates lst = 
  match lst with 
  | [] -> []
  | x :: xs -> negate(x) :: negates(xs) (**arrow vs :: **)

let square x = 
  x * x
let rec squares lst = 
  match lst with
  |[] -> []
  | x :: xs -> square(x) :: squares(xs)

let rec int_log x =
  if x < 10 then 0
  else 1 + int_log (x / 10)

let rec int_logs lst = 
  match lst with 
  |[] -> []
  |x :: xs -> int_log x :: int_logs xs

let code char =
  Char.code char

let rec codes lst = 
  match lst with 
  | [] -> []
  | x :: xs -> code x :: codes xs

(**-> means then gives so takes an int gives an int, so , :: means *)

let is_square x =
  x >= 0 && let root = int_of_float (sqrt (float_of_int x)) in
  root * root = x

let rec code_is_square lst = 
  match lst with 
  |[] -> []
  |x :: xs -> 
  if is_square (code x)
      then code x :: code_is_square xs
  else 
    code_is_square(xs)