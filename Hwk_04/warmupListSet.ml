module ListSetM : Sets.SetS = struct

  type 'a t = 'a list

  (* [empty] represents an empty set.

     Requires: None

     Example: [empty = []]
  *)
  let empty : 'a t = []


  (* [elem x s] checks if x is in set s.

     Requires: None

     Example: [elem 2 [1; 2; 3] = true]
  *)
  let rec elem (x : 'a) (s : 'a t) : bool =
    match s with
    | [] -> false
    | h :: t ->
        if x = h then true
        else elem x t


  (* [insert x s] adds x to set s if it is
     not already in the set.

     Requires: None

     Example: [insert 3 [1; 2] = [3; 1; 2]]
  *)
  let insert (x : 'a) (s : 'a t) : 'a t =
    if elem x s then s
    else x :: s

end

