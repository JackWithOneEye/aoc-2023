open Base

module Problem : Problem.T = struct
  let number_of_day = "12"
  let memo = Hashtbl.create ~growth_allowed:true ~size:1_000 (module String)

  let rec sum_arrangements ?(in_group = false) springs group =
    let has_damaged () =
      springs |> List.find ~f:(Char.equal '#') |> Option.is_some |> Bool.to_int
    in
    let do_sum () =
      match group with
      | [] -> has_damaged ()
      | [ 0 ] -> has_damaged ()
      | _ ->
        (match springs with
         | '.' :: rest ->
           let rest = rest in
           (match group, in_group with
            | 0 :: tail, true -> sum_arrangements rest tail
            | _, true -> 0
            | group, _ -> sum_arrangements rest group)
         | '#' :: rest ->
           let rest = rest in
           (match group with
            | 0 :: _ -> 0
            | head :: tail -> sum_arrangements rest ((head - 1) :: tail) ~in_group:true
            | _ -> 0)
         (* ? *)
         | _ :: rest ->
           (match group, in_group with
            | 0 :: tail, true -> sum_arrangements ('.' :: rest) tail
            | 0 :: _, false -> 0
            | group, true -> sum_arrangements ('#' :: rest) group ~in_group:true
            | group, false ->
              sum_arrangements ('.' :: rest) group + sum_arrangements ('#' :: rest) group)
         | [] -> 0)
    in
    let hash_key =
      (if in_group then "1" else "0")
      ^ String.of_char_list springs
      ^ (group |> List.map ~f:Int.to_string |> String.concat ~sep:",")
    in
    match Hashtbl.find memo hash_key with
    | Some stored -> stored
    | None ->
      let arrangements = do_sum () in
      Hashtbl.set memo ~key:hash_key ~data:arrangements;
      arrangements
  ;;

  let parse_line line =
    line
    |> String.split ~on:' '
    |> function
    | [ springs; group ] ->
      String.to_list springs, group |> String.split ~on:',' |> List.map ~f:Int.of_string
    | _ -> failwith "NO!!!"
  ;;

  let part_a input =
    Hashtbl.clear memo;
    input
    |> String.split_lines
    |> List.fold ~init:0 ~f:(fun acc line ->
      let springs, group = parse_line line in
      acc + sum_arrangements springs group)
    |> Fmt.str "%d"
  ;;

  let part_b input =
    Hashtbl.clear memo;
    input
    |> String.split_lines
    |> List.fold ~init:0 ~f:(fun acc line ->
      let springs, group = parse_line line in
      let springs =
        List.concat
          [ springs; '?' :: springs; '?' :: springs; '?' :: springs; '?' :: springs ]
      in
      let groups = List.concat [ group; group; group; group; group ] in
      acc + sum_arrangements springs groups)
    |> Fmt.str "%d"
  ;;
end
