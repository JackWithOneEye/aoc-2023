open Base

module Problem : Problem.T = struct
  let number = "2"

  let rec every func list =
    match list with
    | [] -> true
    | head :: tail when func head -> every func tail
    | _ -> false
  ;;

  let part_a input =
    let parse_game line =
      let rec get_id id idx chars =
        (* should have just used the index instead... *)
        match chars with
        | digit :: tail when Char.is_digit digit ->
          get_id (id ^ Char.to_string digit) (idx + 1) tail
        | ':' :: ' ' :: _ -> Int.of_string id, idx + 2
        | _ -> failwith "no id found????"
      in
      let pref_len = String.length "Game " in
      let id, index =
        line |> String.subo ~pos:pref_len |> String.to_list |> get_id "" pref_len
      in
      let game =
        line
        |> String.subo ~pos:index
        |> String.split ~on:';'
        |> List.map ~f:(fun set ->
          set |> String.split ~on:',' |> List.map ~f:String.strip)
      in
      let valid_num_cubes set =
        match String.split set ~on:' ' with
        | num :: "red" :: _ -> Int.of_string num <= 12
        | num :: "green" :: _ -> Int.of_string num <= 13
        | num :: "blue" :: _ -> Int.of_string num <= 14
        | _ -> failwith "NOPE!"
      in
      let is_set_possible set = every valid_num_cubes set in
      let is_game_possible sets = every is_set_possible sets in
      game
      |> is_game_possible
      |> function
      | true -> id
      | false -> 0
    in
    input
    |> String.split_lines
    |> List.fold ~init:0 ~f:(fun acc line -> acc + parse_game line)
    |> Fmt.str "%d"
  ;;

  let part_b input =
    let rexes =
      [ Re.Pcre.regexp {|([0-9]+) red|}
      ; Re.Pcre.regexp {|([0-9]+) green|}
      ; Re.Pcre.regexp {|([0-9]+) blue|}
      ]
    in
    let calc_power line =
      List.fold rexes ~init:1 ~f:(fun acc rex ->
        acc
        * (line
           |> Re.all rex
           |> List.map ~f:(fun group -> Re.Group.get group 1 |> Int.of_string)
           |> List.max_elt ~compare:( - )
           |> Option.value_exn))
    in
    input
    |> String.split_lines
    |> List.fold ~init:0 ~f:(fun acc line -> acc + calc_power line)
    |> Fmt.str "%d"
  ;;
end
