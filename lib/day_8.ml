open Base

module Problem : Problem.T = struct
  let number_of_day = "8"

  type node =
    { label : string
    ; left : string
    ; right : string
    }

  let parse_input input =
    let rec parse_network network =
      match network with
      | head :: tail ->
        let label = String.sub head ~pos:0 ~len:3 in
        let left = String.sub head ~pos:7 ~len:3 in
        let right = String.sub head ~pos:12 ~len:3 in
        let tbl = parse_network tail in
        let _ = Hashtbl.add tbl ~key:label ~data:{ label; left; right } in
        tbl
      | [] -> Hashtbl.create ~size:(List.length network) (module String)
    in
    let lines = String.split_lines input in
    match lines with
    | instructions :: _ :: network -> String.to_list instructions, parse_network network
    | _ -> failwith "OH NO!"
  ;;

  let get_next_node instruction node =
    match instruction with
    | 'L' -> node.left
    | 'R' -> node.right
    | _ -> failwith "OH NO!!"
  ;;

  let rec steps_to_destination
    network
    all_instructions
    rest_instructions
    node
    ~is_destination
    =
    match rest_instructions, node with
    | _, { left = _; label; right = _ } when is_destination label -> 0
    | instruction :: rest_instructions, node ->
      get_next_node instruction node
      |> Hashtbl.find_exn network
      |> steps_to_destination network all_instructions rest_instructions ~is_destination
      |> ( + ) 1
    | [], _ ->
      steps_to_destination network all_instructions all_instructions node ~is_destination
  ;;

  let part_a input =
    let instructions, network = parse_input input in
    Hashtbl.find_exn network "AAA"
    |> steps_to_destination
         network
         instructions
         instructions
         ~is_destination:(String.equal "ZZZ")
    |> Fmt.str "%d"
  ;;

  let part_b input =
    let instructions, network = parse_input input in
    let steps_to_destination =
      steps_to_destination network instructions ~is_destination:(fun label ->
        match String.to_list label with
        | [ _; _; 'Z' ] -> true
        | _ -> false)
    in
    let steps =
      Hashtbl.keys network
      |> List.filter ~f:(fun key ->
        match String.to_list key with
        | [ _; _; 'A' ] -> true
        | _ -> false)
      |> List.fold ~init:[] ~f:(fun acc key ->
        let node = Hashtbl.find_exn network key in
        steps_to_destination instructions node :: acc)
    in
    steps |> Util.lcm |> Fmt.str "%d"
  ;;
end
