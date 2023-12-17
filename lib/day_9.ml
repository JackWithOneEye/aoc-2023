open Core

module Problem : Problem.T = struct
  let number_of_day = "9"

  let parse_input input =
    input
    |> String.split_lines
    |> List.map ~f:(fun line -> line |> String.split ~on:' ' |> List.map ~f:Int.of_string)
  ;;

  type direction =
    | L2R
    | R2L

  let extrapolate_history history ~direction =
    let op, diff =
      match direction with
      | L2R -> ( + ), fun fst scnd -> scnd - fst
      | R2L -> ( - ), fun fst scnd -> fst - scnd
    in
    let rec diff_seq numbers =
      match numbers with
      | fst :: scnd :: tail -> diff fst scnd :: diff_seq (scnd :: tail)
      | _ -> []
    in
    let rec acc_diff_seq history =
      let next_line = diff_seq history in
      op
        (List.last_exn history)
        (if Util.all_equal next_line
         then List.last_exn next_line
         else acc_diff_seq next_line)
    in
    acc_diff_seq history
  ;;

  let part_a input =
    input
    |> parse_input
    |> List.fold ~init:0 ~f:(fun acc history ->
      acc + extrapolate_history history ~direction:L2R)
    |> Fmt.str "%d"
  ;;

  let part_b input =
    input
    |> parse_input
    |> List.fold ~init:0 ~f:(fun acc history ->
      history |> List.rev |> extrapolate_history ~direction:R2L |> ( + ) acc)
    |> Fmt.str "%d"
  ;;
end
