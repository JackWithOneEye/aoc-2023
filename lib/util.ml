open Base

module Coordinate = struct
  type t = int * int [@@deriving compare, equal, hash, sexp_of]
end

module Direction = struct
  type t =
    | Up
    | Right
    | Down
    | Left
  [@@deriving compare, enumerate, equal, hash, sexp_of]

  let ( == ) a b = equal a b
  let ( != ) a b = not (equal a b)

  let get_orthogonals = function
    | Up | Down -> [ Left; Right ]
    | Right | Left -> [ Up; Down ]
  ;;

  let is_horizontal = function
    | Up | Down -> false
    | Right | Left -> true
  ;;

  let is_vertical = function
    | Up | Down -> true
    | Right | Left -> false
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

let replace_first list new_el ~f =
  let rec aux list acc =
    match list with
    | head :: tail when f head -> Some (List.rev_append acc (new_el :: tail))
    | head :: tail -> aux tail (head :: acc)
    | [] -> None
  in
  aux list []
;;

let remove_first list ~f =
  let rec aux list acc =
    match list with
    | head :: tail when f head -> Some (List.rev_append acc tail)
    | head :: tail -> aux tail (head :: acc)
    | [] -> None
  in
  aux list []
;;

let lcm l =
  let rec euclid a b =
    match a, b with
    | _, 0 -> a
    | _ -> euclid b (Int.rem a b)
  in
  let lcm a b =
    let gcd = euclid a b in
    a / gcd * b
  in
  match l with
  | head :: tail -> List.fold tail ~init:head ~f:lcm
  | [] -> 0
;;
