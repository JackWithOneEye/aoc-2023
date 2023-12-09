open Base

module Problem : Problem.T = struct
  let number_of_day = "1"

  let part_a input =
    let rec find_first_digit char_list =
      match char_list with
      | [] -> 0
      | head :: _ when Char.is_digit head -> Char.get_digit_exn head
      | _ :: tail -> find_first_digit tail
    in
    let rec calculate_sum lines =
      match lines with
      | [] -> 0
      | head :: tail ->
        let chars = String.to_list head in
        (find_first_digit chars * 10)
        + find_first_digit (List.rev chars)
        + calculate_sum tail
    in
    input |> String.split_lines |> calculate_sum |> Fmt.str "%d"
  ;;

  let part_b input =
    let rec parse_chars char_list digits =
      match char_list with
      | [] -> digits
      | 'o' :: 'n' :: 'e' :: tail -> parse_chars ('e' :: tail) (1 :: digits)
      | 't' :: 'w' :: 'o' :: tail -> parse_chars ('o' :: tail) (2 :: digits)
      | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: tail -> parse_chars ('e' :: tail) (3 :: digits)
      | 'f' :: 'o' :: 'u' :: 'r' :: tail -> parse_chars tail (4 :: digits)
      | 'f' :: 'i' :: 'v' :: 'e' :: tail -> parse_chars ('e' :: tail) (5 :: digits)
      | 's' :: 'i' :: 'x' :: tail -> parse_chars tail (6 :: digits)
      | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: tail -> parse_chars ('n' :: tail) (7 :: digits)
      | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: tail -> parse_chars ('t' :: tail) (8 :: digits)
      | 'n' :: 'i' :: 'n' :: 'e' :: tail -> parse_chars ('e' :: tail) (9 :: digits)
      | head :: tail when Char.is_digit head ->
        parse_chars tail (Char.get_digit_exn head :: digits)
      | _ :: tail -> parse_chars tail digits
    in
    let rec calculate_sum lines =
      match lines with
      | [] -> 0
      | head :: tail ->
        let chars = String.to_list head in
        let digits = parse_chars chars [] in
        let res =
          match digits with
          | [] -> 0
          | head :: tail ->
            head
            + ((match List.rev tail with
                | [] -> head
                | rev_head :: _ -> rev_head)
               * 10)
        in
        res + calculate_sum tail
    in
    input |> String.split_lines |> calculate_sum |> Fmt.str "%d"
  ;;
end
