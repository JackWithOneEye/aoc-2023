open Base
open Util

module Problem : Problem.T = struct
  let number_of_day = "21"

  let parse_input input =
    let rocks = Hashtbl.create (module Coordinate) in
    let lines = input |> String.split_lines in
    let start =
      lines
      |> List.foldi ~init:None ~f:(fun y start line ->
        line
        |> String.to_list
        |> List.foldi ~init:start ~f:(fun x start char ->
          match char with
          | 'S' -> Some (x, y)
          | '#' ->
            Hashtbl.set rocks ~key:(x, y) ~data:();
            start
          | _ -> start))
    in
    ( Option.value_exn start
    , rocks
    , (lines |> List.hd_exn |> String.length) - 1
    , List.length lines - 1 )
  ;;

  let part_a input =
    let start, rocks, max_x, max_y = parse_input input in
    let memo = Hashtbl.create (module Coordinate) in
    let add_moves position set =
      (match Hashtbl.find memo position with
       | Some moves -> moves
       | None ->
         let moves =
           Direction.all
           |> List.fold ~init:[] ~f:(fun acc direction ->
             let xn, yn = Direction.move_coordinate position direction in
             if Hashtbl.find rocks (xn, yn) |> Option.is_some
                || xn < 0
                || xn > max_x
                || yn < 0
                || yn > max_y
             then acc
             else (xn, yn) :: acc)
         in
         Hashtbl.set memo ~key:position ~data:moves;
         moves)
      |> List.iter ~f:(fun pos -> Hashtbl.set set ~key:pos ~data:());
      set
    in
    let rec move_step positions acc =
      if acc = 64
      then positions |> Hashtbl.keys |> List.length
      else (
        let keys = Hashtbl.keys positions in
        Hashtbl.clear positions;
        let next =
          keys
          |> List.fold ~init:positions ~f:(fun acc position -> add_moves position acc)
        in
        move_step next (acc + 1))
    in
    let positions = Hashtbl.of_alist_exn (module Coordinate) [ start, () ] in
    move_step positions 0 |> Fmt.str "%d"
  ;;

  let part_b input = input
end
