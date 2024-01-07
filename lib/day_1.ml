open Base

module Problem : Problem.T = struct
  let number_of_day = "1"

  let part_a input =
    let rec calibration_value chars =
      match chars with
      | head :: tail when Char.is_digit head ->
        let first = Char.get_digit_exn head in
        let last =
          tail
          |> List.rev
          |> List.find_map ~f:(fun char ->
            if Char.is_digit char then Some (Char.get_digit_exn char) else None)
          |> function
          | Some digit -> digit
          | None -> first
        in
        (first * 10) + last
      | _ :: tail -> calibration_value tail
      | [] -> 0
    in
    input
    |> String.split_lines
    |> List.fold ~init:0 ~f:(fun acc line ->
      line |> String.to_list |> calibration_value |> ( + ) acc)
    |> Fmt.str "%d"
  ;;

  let part_b input =
    let parse_chars char_list =
      let rec aux char_list acc =
        match char_list with
        | [] -> acc
        | 'o' :: 'n' :: 'e' :: tail -> aux ('e' :: tail) (1 :: acc)
        | 't' :: 'w' :: 'o' :: tail -> aux ('o' :: tail) (2 :: acc)
        | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: tail -> aux ('e' :: tail) (3 :: acc)
        | 'f' :: 'o' :: 'u' :: 'r' :: tail -> aux tail (4 :: acc)
        | 'f' :: 'i' :: 'v' :: 'e' :: tail -> aux ('e' :: tail) (5 :: acc)
        | 's' :: 'i' :: 'x' :: tail -> aux tail (6 :: acc)
        | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: tail -> aux ('n' :: tail) (7 :: acc)
        | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: tail -> aux ('t' :: tail) (8 :: acc)
        | 'n' :: 'i' :: 'n' :: 'e' :: tail -> aux ('e' :: tail) (9 :: acc)
        | head :: tail when Char.is_digit head -> aux tail (Char.get_digit_exn head :: acc)
        | _ :: tail -> aux tail acc
      in
      aux char_list []
    in
    let calibration_value line =
      line
      |> String.to_list
      |> parse_chars
      |> function
      | [] -> 0
      | [ digit ] -> (digit * 10) + digit
      | last_digit :: tail -> (List.last_exn tail * 10) + last_digit
    in
    input
    |> String.split_lines
    |> List.fold ~init:0 ~f:(fun acc line -> acc + calibration_value line)
    |> Fmt.str "%d"
  ;;
end
