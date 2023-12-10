open Base

module Problem : Problem.T = struct
  let number_of_day = "5"

  type conversion =
    { src_range_start : int
    ; dest_range_start : int
    ; range_len : int
    }

  let parse_input input =
    let parse_seeds line =
      match String.split ~on:' ' line with
      | _ :: tail -> List.map tail ~f:Int.of_string
      | _ -> failwith "NOPE!"
    in
    let line_to_conversion line =
      let values = line |> String.split ~on:' ' |> List.map ~f:Int.of_string in
      match values with
      | [ dest_range_start; src_range_start; range_len ] ->
        { src_range_start; dest_range_start; range_len }
      | _ -> failwith "NOPE!!"
    in
    let rec parse_maps curr_map lines =
      match lines with
      | head :: tail when String.is_empty head -> parse_maps curr_map tail
      | head :: tail when String.is_suffix head ~suffix:"map:" ->
        curr_map :: parse_maps [] tail
      | head :: tail -> parse_maps (line_to_conversion head :: curr_map) tail
      | [] -> [ curr_map ]
    in
    input
    |> String.split_lines
    |> function
    | seeds :: rest -> parse_seeds seeds, parse_maps [] rest
    | _ -> failwith "NOPE!!!"
  ;;

  let convert_to_location seed maps =
    List.fold maps ~init:seed ~f:(fun value map ->
      map
      |> List.find_map ~f:(fun { src_range_start; dest_range_start; range_len } ->
        let diff = value - src_range_start in
        if diff >= 0 && diff < range_len then Some (dest_range_start + diff) else None)
      |> function
      | Some converted -> converted
      | None -> value)
  ;;

  let part_a input =
    let seeds, maps = parse_input input in
    seeds
    |> List.map ~f:(fun seed -> convert_to_location seed maps)
    |> List.min_elt ~compare:( - )
    |> Option.value_exn
    |> Fmt.str "%d"
  ;;

  type seed_range =
    { start : int
    ; length : int
    }

  (* brute force :( *)
  let part_b input =
    let seeds, maps = parse_input input in
    let rec collect_seed_ranges seeds =
      match seeds with
      | start :: length :: rest -> { start; length } :: collect_seed_ranges rest
      | [] -> []
      | _ -> failwith "NOPE!!"
    in
    let min_location_of_range { start; length } =
      List.range start (start + length)
      |> List.fold ~init:Int.max_value ~f:(fun acc seed ->
        let location = convert_to_location seed maps in
        min acc location)
    in
    seeds
    |> collect_seed_ranges
    |> List.fold ~init:Int.max_value ~f:(fun acc range ->
      let min_location = min_location_of_range range in
      min acc min_location)
    |> Fmt.str "%d"
  ;;
end
