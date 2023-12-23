open Base

module Direction = struct
  type t =
    | Up
    | Right
    | Down
    | Left

  let ( == ) a b =
    match a, b with
    | Up, Up -> true
    | Right, Right -> true
    | Down, Down -> true
    | Left, Left -> true
    | _ -> false
  ;;
end

let rec all_equal list =
  match list with
  | prev :: curr :: rest -> if prev <> curr then false else all_equal (curr :: rest)
  | _ -> true
;;

let replace list new_el ~f =
  let rec aux list acc =
    match list with
    | head :: tail when f head -> Some (List.rev acc @ (new_el :: tail))
    | head :: tail -> aux tail (head :: acc)
    | [] -> None
  in
  aux list []
;;

let remove list ~f =
  let rec aux list acc =
    match list with
    | head :: tail when f head -> Some (List.rev acc @ tail)
    | head :: tail -> aux tail (head :: acc)
    | [] -> None
  in
  aux list []
;;
