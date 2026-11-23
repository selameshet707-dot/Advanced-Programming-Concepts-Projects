type fraction = int * int
(** [add p q] is a fraction that is the sum of the fractions [p] and [q]. *)
let add (p: fraction) (q: fraction) : fraction =
  match (p, q) with
  | ((np, dp), (nq, dq)) -> (np * dq + dp * nq, dp * dq)

let mul (p: fraction) (q: fraction) : fraction = 
  