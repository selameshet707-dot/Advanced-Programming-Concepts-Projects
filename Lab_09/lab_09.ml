type 'a t = 'a list

let empty : 'a t = []

let insert (x : 'a) (s : 'a t) : 'a t =
  if List.mem x s then s
  else x :: s

let elem (x : 'a) (s : 'a t) : bool =
  List.mem x s

let s1 : int t = List.fold_right insert [1;2;3] empty

let s2 : int t = List.fold_right insert [2;1;3] empty

let s3 : int t =
  List.fold_right insert [1;2;3;4;5;6;7;8;9;10] empty

let s4 : int t =
  List.fold_right insert [5;3;2;1;4;8;6;7;9;10] empty

let _ =
  assert (elem 3 s1);
  assert (elem 3 s2);
  assert (elem 3 s3);
  assert (elem 3 s4);

  assert (elem 4 (insert 4 s1));
  assert (elem 4 (insert 4 s2));
  assert (elem 4 (insert 4 s3));
  assert (elem 4 (insert 4 s4));
  assert (elem 29 (insert 29 s3));
  assert (elem 29 (insert 29 s4));

  assert (not (elem 13 s1));
  assert (not (elem 13 s4));

  print_endline ("All the tests have passed.")