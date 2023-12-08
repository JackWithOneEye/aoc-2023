open Base

module Problem : Problem.T = struct
  let number = "1"

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
      | 'o' :: 'n' :: 'e' :: tail -> parse_chars ([ 'e' ] @ tail) (digits @ [ 1 ])
      | 't' :: 'w' :: 'o' :: tail -> parse_chars ([ 'o' ] @ tail) (digits @ [ 2 ])
      | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: tail ->
        parse_chars ([ 'e' ] @ tail) (digits @ [ 3 ])
      | 'f' :: 'o' :: 'u' :: 'r' :: tail -> parse_chars tail (digits @ [ 4 ])
      | 'f' :: 'i' :: 'v' :: 'e' :: tail -> parse_chars ([ 'e' ] @ tail) (digits @ [ 5 ])
      | 's' :: 'i' :: 'x' :: tail -> parse_chars tail (digits @ [ 6 ])
      | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: tail ->
        parse_chars ([ 'n' ] @ tail) (digits @ [ 7 ])
      | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: tail ->
        parse_chars ([ 't' ] @ tail) (digits @ [ 8 ])
      | 'n' :: 'i' :: 'n' :: 'e' :: tail -> parse_chars ([ 'e' ] @ tail) (digits @ [ 9 ])
      | head :: tail when Char.is_digit head ->
        parse_chars tail (digits @ [ Char.get_digit_exn head ])
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
            (head * 10)
            +
              (match List.rev tail with
              | [] -> head
              | rev_head :: _ -> rev_head)
        in
        res + calculate_sum tail
    in
    input |> String.split_lines |> calculate_sum |> Fmt.str "%d"
  ;;
end
