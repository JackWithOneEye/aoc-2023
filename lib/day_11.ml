open Base

module Problem : Problem.T = struct
  let number_of_day = "11"

  type coord =
    { x : int
    ; y : int
    }

  let set_of_range length = List.range 0 length |> Set.of_list (module Int)

  let parse_input input ~expansion =
    let lines = String.split_lines input in
    let galaxies, empty_cols, empty_rows =
      lines
      |>
      let cols = lines |> List.hd_exn |> String.length |> set_of_range in
      let rows = lines |> List.length |> set_of_range in
      List.foldi ~init:([], cols, rows) ~f:(fun y acc line ->
        line
        |> String.to_list
        |> List.foldi ~init:acc ~f:(fun x (galaxies, cols, rows) char ->
          if Char.equal char '#'
          then { x; y } :: galaxies, Set.remove cols x, Set.remove rows y
          else galaxies, cols, rows))
    in
    let empty_cols = Set.to_list empty_cols in
    let empty_rows = Set.to_list empty_rows in
    List.map galaxies ~f:(fun { x; y } ->
      let empty_cols_to_left = List.count empty_cols ~f:(fun col -> col < x) in
      let empty_rows_above = List.count empty_rows ~f:(fun row -> row < y) in
      { x = x + (empty_cols_to_left * expansion); y = y + (empty_rows_above * expansion) })
  ;;

  let rec acc_shortest_paths galaxies =
    match galaxies with
    | galaxy :: rest ->
      List.fold rest ~init:0 ~f:(fun acc other ->
        acc + Int.abs (other.x - galaxy.x) + Int.abs (other.y - galaxy.y))
      + acc_shortest_paths rest
    | [] -> 0
  ;;

  let part_a input =
    input |> parse_input ~expansion:1 |> acc_shortest_paths |> Fmt.str "%d"
  ;;

  let part_b input =
    input |> parse_input ~expansion:(1_000_000 - 1) |> acc_shortest_paths |> Fmt.str "%d"
  ;;
end
