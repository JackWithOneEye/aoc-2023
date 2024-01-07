open Core

module Problem : Problem.T = struct
  let number_of_day = "20"

  type module_type =
    | Broadcaster
    | FlipFlop
    | Conjunction

  type pulse =
    | Lo
    | Hi
    | Term

  module Broadcaster = struct
    type t = { destinations : string list }

    let receive_pulse _t pulse = pulse
  end

  module FlipFlopModule = struct
    type state =
      | On
      | Off

    type t =
      { destinations : string list
      ; mutable state : state
      }

    let receive_pulse t pulse =
      match pulse with
      | Lo ->
        let next_state, pulse =
          match t.state with
          | Off -> On, Hi
          | On -> Off, Lo
        in
        t.state <- next_state;
        pulse
      | Hi -> Term
      | Term -> Term
    ;;
  end

  module ConjunctionModule = struct
    type t =
      { destinations : string list
      ; recent_pulses : (string, pulse) Hashtbl.t
      }

    let receive_pulse t pulse input =
      Hashtbl.set t.recent_pulses ~key:input ~data:pulse;
      let all_high =
        Hashtbl.for_all t.recent_pulses ~f:(function
          | Hi -> true
          | _ -> false)
      in
      if all_high then Lo else Hi
    ;;
  end

  type flip_flops = (string, FlipFlopModule.t) Hashtbl.t
  type conjunctions = (string, ConjunctionModule.t) Hashtbl.t

  let parse_input input =
    let parse_type_and_name type_and_name =
      match type_and_name with
      | "broadcaster" -> "broadcaster", Broadcaster
      | other ->
        let name = String.subo ~pos:1 other in
        (match String.get other 0 with
         | '%' -> name, FlipFlop
         | '&' -> name, Conjunction
         | _ -> failwith "NOPE!")
    in
    let rec aux
      lines
      (broadcaster : Broadcaster.t option)
      (flip_flops : flip_flops)
      (conjunctions : conjunctions)
      =
      match lines with
      | head :: tail ->
        (match String.split head ~on:' ' with
         | type_and_name :: _ :: destinations ->
           let name, type_ = parse_type_and_name type_and_name in
           let destinations =
             List.map destinations ~f:(fun dest ->
               String.strip dest ~drop:(Char.equal ','))
           in
           (match type_ with
            | Broadcaster -> aux tail (Some { destinations }) flip_flops conjunctions
            | FlipFlop ->
              Hashtbl.set flip_flops ~key:name ~data:{ destinations; state = Off };
              aux tail broadcaster flip_flops conjunctions
            | Conjunction ->
              Hashtbl.set
                conjunctions
                ~key:name
                ~data:{ destinations; recent_pulses = Hashtbl.create (module String) };
              aux tail broadcaster flip_flops conjunctions)
         | _ -> failwith "NOPE!!")
      | [] -> Option.value_exn broadcaster, flip_flops, conjunctions
    in
    let lines = String.split_lines input in
    let broadcaster, flip_flops, conjunctions =
      aux lines None (Hashtbl.create (module String)) (Hashtbl.create (module String))
    in
    let set_default_pulses key destinations =
      List.iter destinations ~f:(fun dest ->
        match Hashtbl.find conjunctions dest with
        | Some c -> Hashtbl.set c.recent_pulses ~key ~data:Lo
        | None -> ())
    in
    set_default_pulses "broadcaster" broadcaster.destinations;
    Hashtbl.iter_keys flip_flops ~f:(fun ff_key ->
      let ff = Hashtbl.find_exn flip_flops ff_key in
      set_default_pulses ff_key ff.destinations);
    Hashtbl.iter_keys conjunctions ~f:(fun cj_key ->
      let cj = Hashtbl.find_exn conjunctions cj_key in
      set_default_pulses cj_key cj.destinations);
    broadcaster, flip_flops, conjunctions
  ;;

  let push_button
    (broadcaster : Broadcaster.t)
    (flip_flops : flip_flops)
    (conjunctions : conjunctions)
    lo
    hi
    =
    let incr_pulse_counters pulse lo hi =
      match pulse with
      | Lo -> lo + 1, hi
      | Hi -> lo, hi + 1
      | Term -> lo, hi
    in
    let get_module key = Hashtbl.find flip_flops key, Hashtbl.find conjunctions key in
    let rec aux input destinations pulse lo hi =
      let (lo, hi), next =
        List.fold
          destinations
          ~init:((lo, hi), [])
          ~f:(fun ((lo, hi), next) dest ->
            let next_pulse, next_destinations =
              match get_module dest with
              | Some ff, None -> FlipFlopModule.receive_pulse ff pulse, ff.destinations
              | None, Some cj ->
                ConjunctionModule.receive_pulse cj pulse input, cj.destinations
              | None, None -> Term, []
              | _ -> failwith "NOPE!!!"
            in
            ( incr_pulse_counters pulse lo hi
            , match next_pulse with
              | Term -> next
              | next_pulse -> (dest, next_destinations, next_pulse) :: next ))
      in
      loop_next (List.rev next) lo hi
    and loop_next next lo hi =
      match next with
      | (input, destinations, pulse) :: tail ->
        let lo, hi = aux input destinations pulse lo hi in
        loop_next tail lo hi
      | [] -> lo, hi
    in
    let pulse = Broadcaster.receive_pulse broadcaster Lo in
    aux "broadcaster" broadcaster.destinations pulse (lo + 1) hi
  ;;

  let part_a input =
    let broadcaster, flip_flops, conjunctions = parse_input input in
    List.range 0 1000
    |> List.fold ~init:(0, 0) ~f:(fun (lo, hi) _ ->
      push_button broadcaster flip_flops conjunctions lo hi)
    |> Tuple2.uncurry ( * )
    |> Fmt.str "%d"
  ;;

  let part_b input = input
end
