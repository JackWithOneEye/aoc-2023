open Base

module Problem : Problem.T = struct
  let number_of_day = "19"

  type category =
    | X
    | M
    | A
    | S

  type op =
    | Gt
    | Lt

  type rule =
    { category : category option
    ; op : op option
    ; value : int option
    ; workflow : string
    }

  type rating =
    { x : int
    ; m : int
    ; a : int
    ; s : int
    }

  let parse_workflows input =
    let rec parse_rules rules acc =
      match rules with
      | head :: tail ->
        let rule =
          match String.split head ~on:':' with
          | [ workflow ] -> { category = None; op = None; value = None; workflow }
          | [ cat_and_cond; workflow ] ->
            (match String.to_list cat_and_cond with
             | category :: op :: value ->
               let category =
                 match category with
                 | 'x' -> X
                 | 'm' -> M
                 | 'a' -> A
                 | 's' | _ -> S
               in
               let op = Some (if Char.equal op '>' then Gt else Lt) in
               let value = Some (Int.of_string @@ String.of_list @@ value) in
               { category = Some category; op; value; workflow }
             | _ -> failwith "NOPE!")
          | _ -> failwith "NOPE!!"
        in
        parse_rules tail (rule :: acc)
      | [] -> List.rev acc
    in
    let rec aux lines workflows =
      match lines with
      | head :: tail when String.is_empty head -> workflows, tail
      | head :: tail ->
        (match String.split head ~on:'{' with
         | [ name; rules ] ->
           let rules =
             rules |> String.subo ~len:(String.length rules - 1) |> String.split ~on:','
           in
           Hashtbl.set workflows ~key:name ~data:(parse_rules rules []);
           aux tail workflows
         | _ -> failwith "NOPE!!!")
      | _ -> failwith "NOPE!!!!"
    in
    let lines = String.split_lines input in
    let workflows = Hashtbl.create (module String) in
    aux lines workflows
  ;;

  let parse_ratings lines =
    let rec aux lines acc =
      match lines with
      | head :: tail ->
        let category_value rating = rating |> String.subo ~pos:2 |> Int.of_string in
        let rating =
          head
          |> String.subo ~pos:1 ~len:(String.length head - 2)
          |> String.split ~on:','
          |> function
          | [ x; m; a; s ] ->
            { x = category_value x
            ; m = category_value m
            ; a = category_value a
            ; s = category_value s
            }
          | _ -> failwith "NOPE!!!!!"
        in
        aux tail (rating :: acc)
      | [] -> acc
    in
    aux lines []
  ;;

  let part_a input =
    let workflows, ratings =
      parse_workflows input |> fun (workflow, rest) -> workflow, parse_ratings rest
    in
    let rec loop_rules { x; m; a; s } rules =
      match rules with
      | { category = Some category; op = Some op; value = Some value; workflow } :: tail
        ->
        let matches =
          category
          |> (function
                | X -> x
                | M -> m
                | A -> a
                | S -> s)
          |> fun r ->
          match op with
          | Gt -> r > value
          | Lt -> r < value
        in
        if matches then workflow else loop_rules { x; m; a; s } tail
      | { category = _; op = _; value = _; workflow } :: _ -> workflow
      | [] -> ""
    in
    let rec rating_score rating workflow_name =
      let rules = Hashtbl.find_exn workflows workflow_name in
      match loop_rules rating rules with
      | "A" ->
        let { x; m; a; s } = rating in
        x + m + a + s
      | "R" -> 0
      | workflow_name -> rating_score rating workflow_name
    in
    let rec loop_ratings ratings acc =
      match ratings with
      | head :: tail ->
        let score = rating_score head "in" in
        loop_ratings tail (acc + score)
      | [] -> acc
    in
    loop_ratings ratings 0 |> Fmt.str "%d"
  ;;

  type rating_ranges =
    { x : int * int
    ; m : int * int
    ; a : int * int
    ; s : int * int
    }

  let part_b input =
    let workflows, _ = parse_workflows input in
    let split_ranges ranges rule =
      match rule with
      | { category = Some category; op = Some op; value = Some value; workflow } ->
        let split =
          let split_range (a, b) =
            match op with
            | Gt -> (value + 1, b), (a, value)
            | Lt -> (a, value - 1), (value, b)
          in
          match category with
          | X ->
            let x1, x2 = split_range ranges.x in
            { ranges with x = x1 }, { ranges with x = x2 }
          | M ->
            let m1, m2 = split_range ranges.m in
            { ranges with m = m1 }, { ranges with m = m2 }
          | A ->
            let a1, a2 = split_range ranges.a in
            { ranges with a = a1 }, { ranges with a = a2 }
          | S ->
            let s1, s2 = split_range ranges.s in
            { ranges with s = s1 }, { ranges with s = s2 }
        in
        workflow, split
      | { category = _; op = _; value = _; workflow } -> workflow, (ranges, ranges)
    in
    let rec do_workflow workflow_name ranges acc =
      match workflow_name with
      | "A" ->
        let length (a, b) = b - a + 1 in
        let { x; m; a; s } = ranges in
        acc + (length x * length m * length a * length s)
      | "R" -> acc
      | workflow_name ->
        let rules = Hashtbl.find_exn workflows workflow_name in
        loop_rules rules ranges acc
    and loop_rules rules ranges acc =
      match rules with
      | head :: tail ->
        let next_workflow_name, (ranges, rest_ranges) = split_ranges ranges head in
        acc |> loop_rules tail rest_ranges |> do_workflow next_workflow_name ranges
      | [] -> acc
    in
    let min_max = 1, 4000 in
    do_workflow "in" { x = min_max; m = min_max; a = min_max; s = min_max } 0
    |> Fmt.str "%d"
  ;;
end
