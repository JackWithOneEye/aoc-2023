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
    let rec parse_maps lines =
      let rec parse_lines_of_map lines =
        match lines with
        | head :: tail when String.is_empty head -> [], head :: tail
        | head :: tail ->
          let map, rest = parse_lines_of_map tail in
          line_to_conversion head :: map, rest
        | _ -> [], []
      in
      match lines with
      | head :: tail when String.is_empty head -> parse_maps tail
      | head :: tail when String.is_suffix head ~suffix:"map:" ->
        let map, rest = parse_lines_of_map tail in
        map :: parse_maps rest
      | [] -> []
      | _ -> failwith "NO ?????"
    in
    input
    |> String.split_lines
    |> function
    | seeds :: rest -> parse_seeds seeds, parse_maps rest
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
  (* let part_b input =
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
  ;; *)

  type range_mapping =
    | Equal
    | Full
    | RightHalf
    | LeftHalf
    | ThreeWay

  let get_range_mapping { src_range_start; dest_range_start = _; range_len } range =
    let range_end = range.start + range.length - 1 in
    let src_range_end = src_range_start + range_len - 1 in
    if range.start > src_range_end || range_end < src_range_start
    then Equal
    else if range.start >= src_range_start && range_end <= src_range_end
    then Full
    else if range.start < src_range_start
            && range_end >= src_range_start
            && range_end <= src_range_end
    then RightHalf
    else if range.start >= src_range_start
            && range.start <= src_range_end
            && range_end > src_range_end
    then LeftHalf
    else if range.start < src_range_start && range_end > src_range_end
    then ThreeWay
    else failwith "!!!"
  ;;

  let rec map_range map range =
    match map with
    | { src_range_start; dest_range_start; range_len } :: rest ->
      let range_end = range.start + range.length - 1 in
      let src_range_end = src_range_start + range_len - 1 in
      let shift = dest_range_start - src_range_start in
      (match get_range_mapping { src_range_start; dest_range_start; range_len } range with
       | Equal -> map_range rest range
       | Full -> [ { start = range.start + shift; length = range.length } ]
       | RightHalf ->
         { start = dest_range_start; length = range_end - src_range_start + 1 }
         :: map_range rest { start = range.start; length = src_range_start - range.start }
       | LeftHalf ->
         { start = range.start + shift; length = src_range_end - range.start + 1 }
         :: map_range
              rest
              { start = src_range_end + 1; length = range_end - src_range_end }
       | ThreeWay ->
         map_range rest { start = range.start; length = src_range_start - range.start }
         @ ({ start = dest_range_start; length = range_len }
            :: map_range
                 rest
                 { start = src_range_end + 1; length = range_end - src_range_end }))
    | [] -> [ range ]
  ;;

  (* ¡SMART FORCE! *)
  let part_b input =
    let rec collect_seed_ranges seeds =
      match seeds with
      | start :: length :: rest -> { start; length } :: collect_seed_ranges rest
      | [] -> []
      | _ -> failwith "NOPE!!"
    in
    let map_ranges ranges map =
      ranges
      |> List.fold ~init:[] ~f:(fun mapped_ranges range ->
        map_range map range @ mapped_ranges)
    in
    let map_range_to_location_ranges maps range =
      maps
      |> List.fold ~init:[ range ] ~f:map_ranges
      |> List.min_elt ~compare:(fun a b -> a.start - b.start)
      |> function
      | None -> 0
      | Some { start; length = _ } -> start
    in
    let seeds, maps = parse_input input in
    seeds
    |> collect_seed_ranges
    |> List.fold ~init:Int.max_value ~f:(fun min_loc range ->
      range |> map_range_to_location_ranges maps |> min min_loc)
    |> Fmt.str "%d"
  ;;
end
