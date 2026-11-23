let length lst =
  List.fold_left (fun count _ -> count + 1) 0 lst

let rev lst =
  List.fold_left (fun acc item -> item :: acc) [] lst