open Base
open Util

module Problem : Problem.T = struct
  let number_of_day = "10"

  module Coord = struct
    type t =
      { x : int
      ; y : int
      }

    let ( ++ ) a b = { x = a.x + b.x; y = a.y + b.y }

    let from_direction =
      let open Direction in
      function
      | Up -> { x = 0; y = -1 }
      | Right -> { x = 1; y = 0 }
      | Down -> { x = 0; y = 1 }
      | Left -> { x = -1; y = 0 }
    ;;
  end

  module Tile = struct
    type t =
      | UpDown
      | LeftRight
      | UpRight
      | UpLeft
      | DownLeft
      | DownRight
      | Ground
      | Start

    let from_char = function
      | '|' -> UpDown
      | '-' -> LeftRight
      | 'L' -> UpRight
      | 'J' -> UpLeft
      | '7' -> DownLeft
      | 'F' -> DownRight
      | '.' -> Ground
      | 'S' -> Start
      | _ -> failwith "unknown"
    ;;

    let convert_direction to_direction tile =
      let open Direction in
      match to_direction, tile with
      | Up, UpDown -> Some Up
      | Down, UpDown -> Some Down
      | Right, LeftRight -> Some Right
      | Left, LeftRight -> Some Left
      | Down, UpRight -> Some Right
      | Left, UpRight -> Some Up
      | Down, UpLeft -> Some Left
      | Right, UpLeft -> Some Up
      | Right, DownLeft -> Some Down
      | Up, DownLeft -> Some Left
      | Up, DownRight -> Some Right
      | Left, DownRight -> Some Down
      | any, Start -> Some any
      | _ -> None
    ;;
  end

  type grid_cell =
    { tile : Tile.t
    ; mutable in_loop : bool
    ; mutable points_to : Direction.t option
    }

  let parse_input input =
    let rec make_grid lines =
      match lines with
      | head :: tail ->
        (head
         |> String.to_array
         |> Array.map ~f:(fun char ->
           { tile = Tile.from_char char; in_loop = false; points_to = None }))
        :: make_grid tail
      | [] -> []
    in
    let grid = input |> String.split_lines |> make_grid |> List.to_array in
    let start =
      grid
      |> Array.find_mapi ~f:(fun y row ->
        row
        |> Array.find_mapi ~f:(fun x { tile; in_loop = _; points_to = _ } ->
          match tile with
          | Tile.Start -> Some x
          | _ -> None)
        |> function
        | Some x -> Some Coord.{ x; y }
        | None -> None)
      |> Option.value_exn
    in
    grid, start
  ;;

  type position =
    { coord : Coord.t
    ; direction : Direction.t
    }

  let get_points_to from_direction tile =
    let open Direction in
    let open Tile in
    match from_direction, tile with
    | Up, UpDown -> Up
    | Down, UpDown -> Down
    | Left, LeftRight -> Left
    | Right, LeftRight -> Right
    | Down, UpRight -> Down
    | Left, UpRight -> Up
    | Down, UpLeft -> Down
    | Right, UpLeft -> Up
    | Up, DownLeft -> Up
    | Right, DownLeft -> Down
    | Up, DownRight -> Up
    | Left, DownRight -> Down
    | Up, Start -> Up
    | Down, Start -> Down
    | _ -> failwith "NO FLOW!!!!!!!"
  ;;

  let rec travel_loop grid { coord; direction } =
    let get_cell Coord.{ x; y } = grid.(y).(x) in
    let open Coord in
    let coord = coord ++ from_direction direction in
    if coord.x < 0 || coord.y < 0
    then 0
    else (
      let cell = get_cell coord in
      let new_direction = Tile.convert_direction direction cell.tile in
      match new_direction with
      | Some new_direction ->
        (* for part_b *)
        cell.in_loop <- true;
        cell.points_to <- Some (get_points_to direction cell.tile);
        (match cell.tile with
         | Start -> 1
         | _ -> 1 + travel_loop grid { coord; direction = new_direction })
      | None -> 0)
  ;;

  let part_a input =
    let grid, start = parse_input input in
    let steps_in_loop =
      Direction.[ Up; Right; Down; Left ]
      |> List.find_map_exn ~f:(fun direction ->
        match travel_loop grid { coord = start; direction } with
        | 0 -> None
        | steps -> Some steps)
    in
    Fmt.str "%d" (steps_in_loop / 2)
  ;;

  let part_b input =
    let grid, start = parse_input input in
    let _ =
      Direction.[ Right; Left ]
      |> List.find ~f:(fun direction ->
        travel_loop grid { coord = start; direction } <> 0)
    in
    let enclosed_tiles_in_row row =
      let acc, _, _ =
        Array.fold
          row
          ~init:(0, None, false)
          ~f:(fun (acc, enclosing_direction, enclosed) cell ->
            if cell.in_loop
            then
              let open Direction in
              match enclosing_direction, cell.points_to with
              | None, Some points_to -> acc, Some points_to, true
              | Some enclosing_direction, Some points_to ->
                acc, Some enclosing_direction, points_to == enclosing_direction
              | _ -> failwith "NOPE!!!!!!"
            else if enclosed
            then acc + 1, enclosing_direction, enclosed
            else acc, enclosing_direction, enclosed)
      in
      acc
    in
    Array.fold grid ~init:0 ~f:(fun acc row -> acc + enclosed_tiles_in_row row)
    |> Fmt.str "%d"
  ;;
end
