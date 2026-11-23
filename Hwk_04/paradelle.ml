
module type ParadelleS = sig

  type error =
    | FileNotFound of string
    | IncorrectNumLines of int
    | IncorrectLines of (int * int) list
    | IncorrectLastStanza

  type 'a result =
    | Error of error
    | Result of 'a

  val is_paradelle : string -> unit result

end

module ParadelleF (S : Sets.SetS) : ParadelleS = struct

  (* Error types for the paradelle checker. *)
  type error =
    | FileNotFound of string
    | IncorrectNumLines of int
    | IncorrectLines of (int * int) list
    | IncorrectLastStanza

  type 'a result =
    | Error of error
    | Result of 'a

  (* Counts the number of elements in a list. *)
  let length (lst : 'a list) : int =
    List.fold_left (fun count _ -> count + 1) 0 lst

  (* Reverses the order of elements in a list. *)
  let rev (lst : 'a list) : 'a list =
    List.fold_left (fun acc x -> x :: acc) [] lst

  (* Checks if an element matches a value in a list. *)
  let is_elem_by (f : 'a -> 'b -> bool)
      (value : 'b) (lst : 'a list) : bool =
    List.fold_left
      (fun found x -> found || f x value)
      false lst

  (* Checks if a value is contained in a list. *)
  let is_elem (value : 'a) (lst : 'a list) : bool =
    is_elem_by (=) value lst

  (* Removes duplicate elements from a list. *)
  let dedup (lst : 'a list) : 'a list =
    List.fold_right
      (fun x acc ->
        if is_elem x acc then acc else x :: acc)
      lst []

  (* Splits a list whenever an element matches a separator. *)
  let split_by (f : 'a -> 'b -> bool)
      (lst : 'b list) (separators : 'a list) :
      'b list list =
    let current, result =
      List.fold_left
        (fun (current, result) x ->
          if is_elem_by f x separators then
            ([], (rev current) :: result)
          else
            (x :: current, result))
        ([], []) lst
    in
    rev ((rev current) :: result)

  (* Converts a character list into a string. *)
  let string_of_chars (chars : char list) : string =
    String.of_seq (List.to_seq chars)

  (* Converts a string into a character list. *)
  let chars_of_string (str : string) : char list =
    List.of_seq (String.to_seq str)

  (* Lifts a regular function into a Result. *)
  let lift_r (f : 'a -> 'b) (a : 'a) : 'b result =
    Result (f a)

  (* Passes successful results through a pipeline. *)
  let (|>>) (a : 'a result)
      (f : 'a -> 'b result) : 'b result =
    match a with
    | Error e -> Error e
    | Result v -> f v

  (* Reads a file and returns its contents. *)
  let read_file (filename : string) : string result =
    try
      Result (
        In_channel.with_open_text
          filename
          In_channel.input_all
      )
    with Sys_error _ ->
      Error (FileNotFound filename)

  (* Splits characters into individual lines. *)
  let split_lines (chars : char list) : char list list =
    split_by (=) chars ['\n']

  (* Removes blank lines. *)
  let remove_blank_lines
      (lines : char list list) : char list list =
    List.filter (fun line -> line <> []) lines

  (* Splits a line into lowercase words without punctuation. *)
  let words_of_line (line : char list) : string list =
    let separators =
      [' '; '\t'; '\r'; '.'; '!'; '?'; ',';
       ';'; ':'; '-']
    in
    let words = split_by (=) line separators in
    List.map
      (fun word ->
        String.lowercase_ascii (string_of_chars word))
      (List.filter (fun word -> word <> []) words)

  (* Checks that the file contains exactly 24 lines. *)
  let check_line_count
      (lines : char list list) : char list list result =
    if length lines = 24 then
      Result lines
    else
      Error (IncorrectNumLines (length lines))

  (* Groups lines into stanzas of six lines each. *)
  let group_stanzas
      (lines : 'a list) : 'a list list =
    let current, groups, _ =
      List.fold_left
        (fun (current, groups, count) line ->
          if count = 6 then
            ([line], (rev current) :: groups, 1)
          else
            (line :: current, groups, count + 1))
        ([], [], 0) lines
    in
    if current = [] then
      rev groups
    else
      rev ((rev current) :: groups)


(* Checks the word requirements for one stanza. *)
let stanza_errors
    (number : int)
    (stanza : string list list) :
    (int * int) list =
  match stanza with
  | [a; b; c; d; e; f] ->
      let first = (number - 1) * 6 + 1 in
      let first_four =
        List.sort compare (a @ b @ c @ d)
      in
      let last_two =
        List.sort compare (e @ f)
      in
      (if a = b then [] else [(first, first + 1)])
      @
      (if c = d then [] else
        [(first + 2, first + 3)])
      @
      (if first_four = last_two then [] else
        [(first + 4, first + 5)])
  | _ -> []


  let check_stanzas
      (lines : string list list) :
      string list list list result =
    let stanzas = group_stanzas lines in
    match stanzas with
    | [first; second; third; _] ->
        let errors =
          stanza_errors 1 first
          @ stanza_errors 2 second
          @ stanza_errors 3 third
        in
        if errors = [] then
          Result stanzas
        else
          Error (IncorrectLines errors)
    | _ -> Error (IncorrectNumLines (length lines))

  (* Checks the last stanza using sets. *)
  let check_last_stanza
      (stanzas : string list list list) : unit result =
    match stanzas with
    | [first; second; third; last] ->
        let l1_3 =
          List.concat (first @ second @ third)
        in
        let l4 = List.concat last in

        (* Using sets to check last stanza requirement here. *)

        let s1_3 =
          List.fold_left
            (fun set word -> S.insert word set)
            S.empty l1_3
        in

        let s4 =
          List.fold_left
            (fun set word -> S.insert word set)
            S.empty l4
        in

        let first_check =
          List.fold_left
            (fun valid word -> valid && S.elem word s4)
            true l1_3
        in

        let second_check =
          List.fold_left
            (fun valid word -> valid && S.elem word s1_3)
            true l4
        in

        if first_check && second_check then
          Result ()
        else
          Error IncorrectLastStanza

    | _ -> Error IncorrectLastStanza

  (* Checks whether a file contains a paradelle. *)
  let is_paradelle (filename : string) : unit result =
    filename
    |> read_file
    |>> lift_r chars_of_string
    |>> lift_r split_lines
    |>> lift_r remove_blank_lines
    |>> check_line_count
    |>> lift_r (List.map words_of_line)
    |>> check_stanzas
    |>> check_last_stanza

end
