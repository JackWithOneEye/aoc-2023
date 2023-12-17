open Base

let rec all_equal list =
  match list with
  | prev :: curr :: rest -> if prev <> curr then false else all_equal (curr :: rest)
  | _ -> true
;;
