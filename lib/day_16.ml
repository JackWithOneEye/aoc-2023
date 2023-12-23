open Base
open Util

module Problem : Problem.T = struct
  let number_of_day = "16"

  type tile =
    { symbol : char
    ; mutable energised : Direction.t list
    }

  let move (x, y) direction =
    let open Direction in
    match direction with
    | Up -> x, y - 1
    | Right -> x + 1, y
    | Down -> x, y + 1
    | Left -> x - 1, y
  ;;

  let reflect_fwd direction =
    let open Direction in
    match direction with
    | Up -> Right
    | Right -> Up
    | Down -> Left
    | Left -> Down
  ;;

  let reflect_bwd direction =
    let open Direction in
    match direction with
    | Up -> Left
    | Right -> Down
    | Down -> Right
    | Left -> Up
  ;;

  let split_beam symbol direction =
    let open Direction in
    match symbol, direction with
    | '|', Up -> None
    | '|', Right -> Some (Up, Down)
    | '|', Down -> None
    | '|', Left -> Some (Up, Down)
    | '-', Up -> Some (Left, Right)
    | '-', Right -> None
    | '-', Down -> Some (Left, Right)
    | '-', Left -> None
    | _ -> failwith "NOPE!!!"
  ;;

  let get_max_xy tiles = Array.length tiles.(0) - 1, Array.length tiles - 1

  let already_energised_from tile direction =
    tile.energised |> List.find ~f:(Direction.( == ) direction) |> Option.is_some
  ;;

  let walk start_pos start_direction tiles =
    let max_x, max_y = get_max_xy tiles in
    let rec aux pos direction =
      let x, y = pos in
      if x < 0 || x > max_x || y < 0 || y > max_y
      then ()
      else (
        let tile = tiles.(y).(x) in
        if already_energised_from tile direction
        then ()
        else (
          tile.energised <- direction :: tile.energised;
          let move = move pos in
          match tile.symbol with
          | '.' -> aux (move direction) direction
          | '/' ->
            let refl_direction = reflect_fwd direction in
            aux (move refl_direction) refl_direction
          | '\\' ->
            let refl_direction = reflect_bwd direction in
            aux (move refl_direction) refl_direction
          | split ->
            (match split_beam split direction with
             | None -> aux (move direction) direction
             | Some (a, b) ->
               aux (move a) a;
               aux (move b) b)))
    in
    aux start_pos start_direction
  ;;

  let parse_input input =
    input
    |> String.split_lines
    |> List.to_array
    |> Array.map ~f:(fun line ->
      line
      |> String.to_array
      |> Array.map ~f:(fun char -> { symbol = char; energised = [] }))
  ;;

  let part_a input =
    let tiles = parse_input input in
    walk (0, 0) Right tiles;
    tiles
    |> Array.fold ~init:0 ~f:(fun acc row ->
      acc
      + Array.count row ~f:(fun tile ->
        match tile.energised with
        | _ :: _ -> true
        | _ -> false))
    |> Fmt.str "%d"
  ;;

  let part_b input =
    let make_tiles () =
      let tiles = parse_input input in
      let max_x, max_y = get_max_xy tiles in
      tiles, max_x, max_y
    in
    let num_energised pos dir tiles =
      walk pos dir tiles;
      tiles
      |> Array.fold ~init:0 ~f:(fun acc row ->
        acc
        + Array.count row ~f:(fun tile ->
          let res =
            match tile.energised with
            | _ :: _ -> true
            | _ -> false
          in
          (* reset *)
          tile.energised <- [];
          res))
    in
    let open Stdlib.Domain in
    let up_down =
      spawn (fun _ ->
        let tiles, max_x, max_y = make_tiles () in
        List.range 0 max_x ~stop:`inclusive
        |> List.fold ~init:0 ~f:(fun max_energised x ->
          max_energised
          |> max (num_energised (x, 0) Down tiles)
          |> max (num_energised (x, max_y) Up tiles)))
    in
    let left_right =
      spawn (fun _ ->
        let tiles, max_x, max_y = make_tiles () in
        List.range 0 max_y ~stop:`inclusive
        |> List.fold ~init:0 ~f:(fun max_energised y ->
          max_energised
          |> max (num_energised (0, y) Right tiles)
          |> max (num_energised (max_x, y) Left tiles)))
    in
    max (join up_down) (join left_right) |> Fmt.str "%d"
  ;;
end
