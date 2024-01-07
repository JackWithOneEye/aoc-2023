open Base
open Util

module Problem : Problem.T = struct
  let number_of_day = "18"

  type dig_plan_entry =
    { direction : Direction.t
    ; metres : int
    }

  let parse_input input ~create_dig_plan_entry =
    input
    |> String.split_lines
    |> List.fold ~init:[] ~f:(fun acc line ->
      line
      |> String.split ~on:' '
      |> function
      | [ d; m; c ] -> create_dig_plan_entry (d, m, c) :: acc
      | _ -> failwith "NOPE!")
    |> List.rev
  ;;

  let lagoon_volume dig_plan =
    let area, boundary =
      List.fold
        dig_plan
        ~init:(0, 0, (0, 0))
        ~f:(fun (area, boundary, (x, y)) { direction; metres } ->
          let xd, yd = Direction.move_coordinate ~steps:metres (x, y) direction in
          (* shoelace *)
          area + ((x * yd) - (y * xd)), boundary + metres, (xd, yd))
      |> fun (area, boundary, _) -> area / 2, boundary
    in
    (* pick's theorem *)
    let inner = area - (boundary / 2) + 1 in
    boundary + inner
  ;;

  let part_a input =
    input
    |> parse_input ~create_dig_plan_entry:(fun (direction, metres, _) ->
      let direction =
        let open Direction in
        match direction with
        | "U" -> Up
        | "R" -> Right
        | "D" -> Down
        | "L" | _ -> Left
      in
      { direction; metres = Int.of_string metres })
    |> lagoon_volume
    |> Fmt.str "%d"
  ;;

  let part_b input =
    input
    |> parse_input ~create_dig_plan_entry:(fun (_, _, colour) ->
      let direction =
        let open Direction in
        colour
        |> String.subo ~pos:7 ~len:1
        |> function
        | "3" -> Up
        | "0" -> Right
        | "1" -> Down
        | "2" -> Left
        | _ -> failwith "NOPE!!"
      in
      let metres =
        colour |> String.subo ~pos:2 ~len:5 |> ( ^ ) "0x" |> Int.Hex.of_string
      in
      { direction; metres })
    |> lagoon_volume
    |> Fmt.str "%d"
  ;;
end
