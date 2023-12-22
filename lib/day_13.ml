open Base

module Problem : Problem.T = struct
  let number_of_day = "13"

  let parse_input input =
    let rec get_pattern lines =
      match lines with
      | head :: tail when head |> String.strip |> String.is_empty -> [], tail
      | head :: tail ->
        let tail, rest = get_pattern tail in
        head :: tail, rest
      | [] -> [], []
    in
    let rec collect_patterns lines =
      let pattern, rest = get_pattern lines in
      match rest with
      | [] -> [ pattern ]
      | rest -> pattern :: collect_patterns rest
    in
    input |> String.split_lines |> collect_patterns
  ;;

  let sum_reflection_lines pattern ~is_symmetric =
    let find_refl pattern ~pattern_len ~is_symmetric =
      let rec refl_lines ?(ignored = 0) pattern =
        match pattern with
        | [ _ ] -> 0
        | head :: tail ->
          if is_symmetric (head :: tail)
          then ignored + (List.length (head :: tail) / 2)
          else refl_lines tail ~ignored:(ignored + 1)
        | [] -> 0
      in
      match refl_lines pattern with
      | 0 ->
        (match refl_lines (List.rev pattern) with
         | 0 -> 0
         | bwd -> pattern_len - bwd)
      | fwd -> fwd
    in
    let vertical_reflection pattern =
      let pattern_len = pattern |> List.hd_exn |> String.length in
      pattern
      |> List.map ~f:String.to_list
      |> List.transpose_exn
      |> List.map ~f:String.of_char_list
      |> find_refl ~pattern_len ~is_symmetric
    in
    let horizontal_reflection pattern =
      find_refl pattern ~pattern_len:(List.length pattern) ~is_symmetric
    in
    List.fold pattern ~init:0 ~f:(fun acc pattern ->
      acc
      +
      match horizontal_reflection pattern with
      | 0 -> vertical_reflection pattern
      | horizontal -> horizontal * 100)
  ;;

  let part_a input =
    let is_symmetric pattern =
      let length = List.length pattern in
      length % 2 = 0
      &&
      let top, bottom = List.split_n pattern (length / 2) in
      List.equal String.equal top (List.rev bottom)
    in
    input |> parse_input |> sum_reflection_lines ~is_symmetric |> Fmt.str "%d"
  ;;

  let part_b input =
    let might_be_smudged a b =
      let rec loop ?(found_one = false) al bl =
        match al, bl with
        | a1 :: al, b1 :: bl ->
          if Char.equal a1 b1
          then loop al bl ~found_one
          else (not found_one) && loop al bl ~found_one:true
        | [], [] -> found_one
        | _ -> failwith "NOPE!"
      in
      loop (String.to_list a) (String.to_list b)
    in
    let is_symmetric pattern =
      let length = List.length pattern in
      if length % 2 = 0
      then (
        let top, bottom = List.split_n pattern (length / 2) in
        let removed_smudge = ref false in
        let equal a b =
          String.equal a b
          ||
          if (not !removed_smudge) && might_be_smudged a b
          then (
            removed_smudge := true;
            true)
          else false
        in
        List.equal equal top (List.rev bottom) && !removed_smudge)
      else false
    in
    input |> parse_input |> sum_reflection_lines ~is_symmetric |> Fmt.str "%d"
  ;;
end
