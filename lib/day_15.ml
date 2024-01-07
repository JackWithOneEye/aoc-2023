open Base

module Problem : Problem.T = struct
  let number_of_day = "15"
  let parse_input input = input |> String.strip |> String.split ~on:','

  let hash_step step =
    String.fold step ~init:0 ~f:(fun acc char ->
      Int.rem ((acc + Char.to_int char) * 17) 256)
  ;;

  let part_a input =
    input
    |> parse_input
    |> List.fold ~init:0 ~f:(fun acc step -> acc + hash_step step)
    |> Fmt.str "%d"
  ;;

  let part_b input =
    let boxes : (string * int) list array = Array.create ~len:256 [] in
    let get_stuff step i =
      let label = String.prefix step i in
      let box_idx = hash_step label in
      let box = Array.get boxes box_idx in
      label, box_idx, box
    in
    let match_label label (l, _) = String.equal label l in
    let put_box step =
      let i = String.index_exn step '=' in
      let label, box_idx, box = get_stuff step i in
      let focal_length = step |> String.subo ~pos:(i + 1) |> Int.of_string in
      let lens = label, focal_length in
      Array.set
        boxes
        box_idx
        (match Util.replace_first box lens ~f:(match_label label) with
         | Some box -> box
         | None -> lens :: box)
    in
    input
    |> parse_input
    |> List.iter ~f:(fun step ->
      match String.index step '-' with
      | Some i ->
        let label, box_idx, box = get_stuff step i in
        Array.set
          boxes
          box_idx
          (match Util.remove_first box ~f:(match_label label) with
           | Some box -> box
           | None -> box)
      | None -> put_box step);
    Array.foldi boxes ~init:0 ~f:(fun i acc box ->
      let box_num = i + 1 in
      box
      |> List.rev
      |> List.foldi ~init:0 ~f:(fun j box_acc (_, focal_length) ->
        box_acc + (box_num * (j + 1) * focal_length))
      |> ( + ) acc)
    |> Fmt.str "%d"
  ;;
end
