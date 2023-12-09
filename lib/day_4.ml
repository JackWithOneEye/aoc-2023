open Base

module Problem : Problem.T = struct
  let number_of_day = "4"

  let get_numbers line =
    let rex = Re.Pcre.regexp {|([0-9]+)(\s|$)|} in
    let extract_numbers str =
      str
      |> Re.all rex
      |> List.map ~f:(fun group -> Re.Group.get group 1 |> Int.of_string)
    in
    let winning_numbers, my_numbers = String.lsplit2_exn ~on:'|' line in
    extract_numbers winning_numbers, extract_numbers my_numbers
  ;;

  let match_count card =
    let winning_numbers, my_numbers = get_numbers card in
    List.count my_numbers ~f:(fun my_num ->
      winning_numbers |> List.find ~f:(( = ) my_num) |> Option.is_some)
  ;;

  let part_a input =
    input
    |> String.split_lines
    |> List.fold ~init:0 ~f:(fun sum card -> sum + (Int.pow 2 (match_count card) / 2))
    |> Fmt.str "%d"
  ;;

  type card =
    { matches : int
    ; mutable instances : int
    }

  let part_b input =
    let increment_next_instances cards ~prev_card =
      let rec per_match ?(match_idx = 0) cards () =
        match cards, match_idx with
        | head :: tail, i when i < prev_card.matches ->
          head.instances <- head.instances + 1;
          per_match tail ~match_idx:(i + 1) ()
        | _, _ -> ()
      in
      let rec per_instance ?(inst_idx = 0) () =
        if inst_idx < prev_card.instances
        then (
          let _ = per_match cards () in
          per_instance ~inst_idx:(inst_idx + 1) ())
        else ()
      in
      per_instance ()
    in
    let rec total_num_cards cards =
      match cards with
      | head :: tail ->
        increment_next_instances tail ~prev_card:head;
        head.instances + total_num_cards tail
      | [] -> 0
    in
    input
    |> String.split_lines
    |> List.map ~f:(fun card -> { matches = match_count card; instances = 1 })
    |> total_num_cards
    |> Fmt.str "%d"
  ;;
end
