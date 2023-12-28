open Base

module Direction = struct
  type t =
    | Up
    | Right
    | Down
    | Left
  [@@deriving compare, equal, hash, sexp_of]

  let ( == ) a b = equal a b
  let ( != ) a b = not (equal a b)

  let get_orthogonals = function
    | Up | Down -> [ Left; Right ]
    | Right | Left -> [ Up; Down ]
  ;;

  let to_string = function
    | Up -> "Up"
    | Right -> "Right"
    | Down -> "Down"
    | Left -> "Left"
  ;;

  let move_coordinate ?(steps = 1) (x, y) direction =
    match direction with
    | Up -> x, y - steps
    | Right -> x + steps, y
    | Down -> x, y + steps
    | Left -> x - steps, y
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
