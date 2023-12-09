open Base

module Problem : Problem.T = struct
  let number_of_day = "3"

  let str_get str i =
    if Int.between i ~low:0 ~high:(String.length str - 1)
    then Some (String.get str i)
    else None
  ;;

  let str_arr_get str_arr i j =
    if Int.between i ~low:0 ~high:(Array.length str_arr - 1)
    then str_get (Array.get str_arr i) j
    else None
  ;;

  type acc =
    { mutable current_digit_seq : (char * bool) list
    ; mutable sum : int
    }

  let part_a input =
    let lines = input |> String.split_lines |> List.to_array in
    let has_symbol_neighbour idx line line_idx =
      [ str_arr_get lines (line_idx - 1) (idx - 1)
      ; str_arr_get lines (line_idx - 1) idx
      ; str_arr_get lines (line_idx - 1) (idx + 1)
      ; str_get line (idx - 1)
      ; str_get line (idx + 1)
      ; str_arr_get lines (line_idx + 1) (idx - 1)
      ; str_arr_get lines (line_idx + 1) idx
      ; str_arr_get lines (line_idx + 1) (idx + 1)
      ]
      |> List.find ~f:(function
        | Some '.' -> false
        | Some char -> not (Char.is_digit char)
        | None -> false)
      |> Option.is_some
    in
    let sum_part_numbers_of_line line line_idx =
      let acc =
        line
        |> String.foldi ~init:{ current_digit_seq = []; sum = 0 } ~f:(fun i acc char ->
          let evaluate_number () =
            let number, is_part =
              acc.current_digit_seq
              |> List.fold
                   ~init:("", false)
                   ~f:(fun (nbr, at_least_one) (digit, has_symbol_nbr) ->
                     nbr ^ Char.to_string digit, at_least_one || has_symbol_nbr)
            in
            acc.sum <- (acc.sum + if is_part then Int.of_string number else 0);
            acc.current_digit_seq <- [];
            acc
          in
          match char with
          | digit when Char.is_digit digit ->
            acc.current_digit_seq
            <- acc.current_digit_seq @ [ digit, has_symbol_neighbour i line line_idx ];
            if i + 1 = String.length line then evaluate_number () else acc
          | _ when List.length acc.current_digit_seq = 0 -> acc
          | _ -> evaluate_number ())
      in
      acc.sum
    in
    lines
    |> Array.foldi ~init:0 ~f:(fun line_idx acc line ->
      acc + sum_part_numbers_of_line line line_idx)
    |> Fmt.str "%d"
  ;;

  type asterisk =
    { row : int
    ; col : int
    }

  type part_number =
    { row : int
    ; start_col : int
    ; end_col : int
    ; value : string
    }

  let part_b input =
    let rec collect_part_numbers chars row current_number index acc =
      match current_number, chars with
      | None, digit :: tail when Char.is_digit digit ->
        collect_part_numbers
          tail
          row
          (Some { row; start_col = index; end_col = index; value = Char.to_string digit })
          (index + 1)
          acc
      | Some { row; start_col; end_col = _; value }, digit :: tail
        when Char.is_digit digit ->
        collect_part_numbers
          tail
          row
          (Some { row; start_col; end_col = index; value = value ^ Char.to_string digit })
          (index + 1)
          acc
      | Some part_number, _ :: tail ->
        collect_part_numbers tail row None (index + 1) (part_number :: acc)
      | Some part_number, [] -> part_number :: acc
      | None, _ :: tail -> collect_part_numbers tail row None (index + 1) acc
      | None, [] -> acc
    in
    let adjacents = [ -1, -1; 0, -1; 1, -1; -1, 0; 1, 0; -1, 1; 0, 1; 1, 1 ] in
    let is_adjacent asterisk part_number =
      adjacents
      |> List.find ~f:(fun (x, y) ->
        let xa = asterisk.col + x in
        let ya = asterisk.row + y in
        part_number.row = ya && part_number.start_col <= xa && part_number.end_col >= xa)
      |> Option.is_some
    in
    let rec gear_ratio asterisk part_numbers num_adjacent acc =
      match part_numbers, num_adjacent with
      | [], na when na = 2 ->
        acc |> List.fold ~init:1 ~f:(fun sum pa -> sum * Int.of_string pa.value)
      | [], _ -> 0
      | _, na when na > 2 -> 0
      | head :: tail, na when is_adjacent asterisk head ->
        gear_ratio asterisk tail (na + 1) (head :: acc)
      | _ :: tail, na -> gear_ratio asterisk tail na acc
    in
    let lines = String.split_lines input in
    let asterisks =
      lines
      |> List.foldi ~init:[] ~f:(fun row acc line ->
        line
        |> String.to_list
        |> List.foldi ~init:acc ~f:(fun col acc_inner char ->
          match char with
          | '*' -> { row; col } :: acc_inner
          | _ -> acc_inner))
    in
    let part_numbers =
      lines
      |> List.foldi ~init:[] ~f:(fun row acc line ->
        collect_part_numbers (String.to_list line) row None 0 acc)
    in
    asterisks
    |> List.fold ~init:0 ~f:(fun acc asterisk ->
      acc + gear_ratio asterisk part_numbers 0 [])
    |> Fmt.str "%d"
  ;;
end
