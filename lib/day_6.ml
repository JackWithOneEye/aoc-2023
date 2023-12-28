open Base

module Problem : Problem.T = struct
  let number_of_day = "6"

  let possible_wins time distance =
    let rec acc_wins hold_time wins =
      match (time - hold_time) * hold_time > distance with
      | true -> acc_wins (hold_time + 1) (wins + 1)
      | false when wins = 0 -> acc_wins (hold_time + 1) wins
      | _ -> wins
    in
    acc_wins 1 0
  ;;

  let part_a input =
    let times, distances =
      input
      |> String.split_lines
      |> List.map ~f:(fun line ->
        line
        |> String.split ~on:' '
        |> List.filter_map ~f:(fun str -> Int.of_string_opt str))
      |> function
      | [ times; distances ] -> times, distances
      | _ -> failwith "WRONG!"
    in
    List.fold2 times distances ~init:1 ~f:(fun acc time distance ->
      possible_wins time distance * acc)
    |> function
    | List.Or_unequal_lengths.Ok res -> Fmt.str "%d" res
    | _ -> failwith "WRONG!!"
  ;;

  let part_b input =
    let unkern str =
      str |> String.filter ~f:(fun char -> Char.is_digit char) |> Int.of_string
    in
    input
    |> String.split_lines
    |> List.map ~f:unkern
    |> function
    | [ time; distance ] -> possible_wins time distance |> Fmt.str "%d"
    | _ -> failwith "WRONG!!!"
  ;;
end
