open Base

module Problem : Problem.T = struct
  let number_of_day = "14"
  let cycle_memo = Hashtbl.create ~size:2 (module String)

  let tilt_horizontally lines =
    let shift_line line = line |> String.substr_replace_all ~pattern:".O" ~with_:"O." in
    let rec aux_loop ?(i = 0) line until =
      let shifted = shift_line line in
      if i = until then shifted else aux_loop shifted until ~i:(i + 1)
    in
    lines |> List.map ~f:(fun line -> aux_loop line (String.length line))
  ;;

  let tilt_vertically lines =
    let shift_up above curr =
      let new_above, new_curr =
        List.fold2_exn
          (String.to_list above)
          (String.to_list curr)
          ~init:([], [])
          ~f:(fun (new_above, new_curr) a c ->
            match a, c with
            | '.', 'O' -> 'O' :: new_above, '.' :: new_curr
            | a, c -> a :: new_above, c :: new_curr)
      in
      ( new_above |> List.rev |> String.of_char_list
      , new_curr |> List.rev |> String.of_char_list )
    in
    let rec shift lines acc =
      match lines with
      | [ last ] -> List.rev (last :: acc)
      | fst :: scd :: rest ->
        let fst, scd = shift_up fst scd in
        shift (scd :: rest) (fst :: acc)
      | [] -> acc
    in
    let rec aux_loop ?(i = 0) lines until =
      let shifted = shift lines [] in
      if i = until then List.rev shifted else aux_loop shifted until ~i:(i + 1)
    in
    aux_loop lines (List.length lines)
  ;;

  let calc_total_load tilted =
    List.foldi tilted ~init:0 ~f:(fun i acc line ->
      acc + ((i + 1) * String.count line ~f:(Char.equal 'O')))
  ;;

  let part_a input =
    input |> String.split_lines |> tilt_vertically |> calc_total_load |> Fmt.str "%d"
  ;;

  let part_b input =
    (* 7260321 *)
    (* 7095599 *)
    Hashtbl.clear cycle_memo;
    let tilt_cycle platform =
      let hash_key = List.fold platform ~init:"" ~f:(fun acc line -> acc ^ "_" ^ line) in
      match Hashtbl.find cycle_memo hash_key with
      | Some stored -> stored, true
      | None ->
        let rev_horiz = List.map ~f:String.rev in
        let north = platform |> tilt_vertically |> List.rev in
        let west = north |> tilt_horizontally in
        let south = west |> List.rev |> tilt_vertically in
        let east = south |> rev_horiz |> tilt_horizontally |> rev_horiz in
        let res = east in
        Hashtbl.set cycle_memo ~key:hash_key ~data:res;
        res, false
    in
    let rec cycle_tilts ?(i = 0) ?(stored_cnt = 0) platform =
      if i = 1_000_000_000
      then platform |> List.rev |> calc_total_load
      else (
        let platform, was_stored = tilt_cycle platform in
        let stored_cnt = stored_cnt + Bool.to_int was_stored in
        if stored_cnt >= 2
        then platform |> List.rev |> calc_total_load
        else cycle_tilts platform ~i:(i + 1) ~stored_cnt)
    in
    input |> String.split_lines |> cycle_tilts |> Fmt.str "%d"
  ;;
end
