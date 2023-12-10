open Base

module Problem : Problem.T = struct
  let number_of_day = "7"

  type hand_type =
    | HighCard
    | OnePair
    | TwoPair
    | ThreeOfAKind
    | FullHouse
    | FourOfAKind
    | FiveOfAKind

  type hand =
    { h_type : hand_type
    ; cards : string
    ; bid : int
    }

  let strength_of_hand_type hand_type =
    match hand_type with
    | HighCard -> 1
    | OnePair -> 2
    | TwoPair -> 3
    | ThreeOfAKind -> 4
    | FullHouse -> 5
    | FourOfAKind -> 6
    | FiveOfAKind -> 7
  ;;

  let get_hand_type cards card_table =
    String.iter cards ~f:(fun char ->
      Hashtbl.update card_table char ~f:(fun v ->
        match v with
        | Some cnt -> cnt + 1
        | None -> 1);
      ());
    let max_dupes =
      card_table
      |> Hashtbl.to_alist
      |> List.fold ~init:0 ~f:(fun acc (_, value) -> max acc value)
    in
    match Hashtbl.length card_table with
    | 5 -> HighCard
    | 4 -> OnePair
    | 3 when max_dupes = 2 -> TwoPair
    | 3 when max_dupes = 3 -> ThreeOfAKind
    | 2 when max_dupes = 3 -> FullHouse
    | 2 when max_dupes = 4 -> FourOfAKind
    | 1 -> FiveOfAKind
    | _ -> failwith "FAIL!"
  ;;

  let compare_hands a b ~get_card_strength =
    let to_strenghts cards = cards |> String.to_list |> List.map ~f:get_card_strength in
    let rec compare_cards a_cards b_cards =
      match a_cards, b_cards with
      | card_a :: _, card_b :: _ when card_a > card_b -> 1
      | card_a :: tail_a, card_b :: tail_b when card_a = card_b ->
        compare_cards tail_a tail_b
      | card_a :: _, card_b :: _ when card_a < card_b -> -1
      | [], [] -> 0
      | _ -> failwith "FAIL!!"
    in
    let a_strength = strength_of_hand_type a.h_type in
    let b_strength = strength_of_hand_type b.h_type in
    if a_strength = b_strength
    then compare_cards (to_strenghts a.cards) (to_strenghts b.cards)
    else a_strength - b_strength
  ;;

  let part_a input =
    let get_card_strength card =
      match card with
      | 'A' -> 14
      | 'K' -> 13
      | 'Q' -> 12
      | 'J' -> 11
      | 'T' -> 10
      | '2' .. '9' -> Char.get_digit_exn card
      | _ -> failwith "FAIL!!!"
    in
    let card_table = Hashtbl.create ~size:5 (module Char) in
    input
    |> String.split_lines
    |> List.map ~f:(fun line ->
      Hashtbl.clear card_table;
      let hand, bid = String.lsplit2_exn line ~on:' ' in
      { h_type = get_hand_type hand card_table; cards = hand; bid = Int.of_string bid })
    |> List.sort ~compare:(compare_hands ~get_card_strength)
    |> List.foldi ~init:0 ~f:(fun i acc hand -> acc + ((i + 1) * hand.bid))
    |> Fmt.str "%d"
  ;;

  let part_b input =
    let get_card_strength card =
      match card with
      | 'A' -> 13
      | 'K' -> 12
      | 'Q' -> 11
      | 'T' -> 10
      | '2' .. '9' -> Char.get_digit_exn card
      | 'J' -> 1
      | _ -> failwith "FAIL!!!!"
    in
    let card_table = Hashtbl.create ~size:5 (module Char) in
    let get_hand_type_with_joker cards =
      Hashtbl.clear card_table;
      let hand_type = get_hand_type cards card_table in
      let num_jokers = Hashtbl.find card_table 'J' in
      match hand_type, num_jokers with
      | HighCard, Some 1 -> OnePair
      | OnePair, Some 1 -> ThreeOfAKind
      | OnePair, Some 2 -> ThreeOfAKind
      | TwoPair, Some 1 -> FullHouse
      | TwoPair, Some 2 -> FourOfAKind
      | ThreeOfAKind, Some 1 -> FourOfAKind
      | ThreeOfAKind, Some 3 -> FourOfAKind
      | FullHouse, Some _ -> FiveOfAKind
      | FourOfAKind, Some _ -> FiveOfAKind
      | _, _ -> hand_type
    in
    input
    |> String.split_lines
    |> List.map ~f:(fun line ->
      let hand, bid = String.lsplit2_exn line ~on:' ' in
      { h_type = get_hand_type_with_joker hand; cards = hand; bid = Int.of_string bid })
    |> List.sort ~compare:(compare_hands ~get_card_strength)
    |> List.foldi ~init:0 ~f:(fun i acc hand -> acc + ((i + 1) * hand.bid))
    |> Fmt.str "%d"
  ;;
end
