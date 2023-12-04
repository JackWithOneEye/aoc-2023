open Base
open Components

let html_to_string html = Fmt.str "%a" (Tyxml.Html.pp ()) html
let elt_to_string elt = Fmt.str "%a" (Tyxml.Html.pp_elt ()) elt
let zero_ascii = Char.to_int '0'
let nine_ascii = Char.to_int '9'

let day1_a input =
  let rec find_first_digit char_list =
    match char_list with
    | [] -> 0
    | head :: tail ->
      let head_ascii = Char.to_int head in
      if head_ascii >= zero_ascii && head_ascii <= nine_ascii
      then head_ascii - zero_ascii
      else find_first_digit tail
  in
  let rec calculate_sum lines =
    match lines with
    | [] -> 0
    | head :: tail ->
      let chars = String.to_list head in
      (find_first_digit chars * 10)
      + find_first_digit (List.rev chars)
      + calculate_sum tail
  in
  input |> String.split_lines |> calculate_sum |> Fmt.str "%d"
;;

let day1_b input =
  let rec parse_chars char_list digits =
    match char_list with
    | [] -> digits
    | 'o' :: 'n' :: 'e' :: tail -> parse_chars ([ 'e' ] @ tail) (digits @ [ 1 ])
    | 't' :: 'w' :: 'o' :: tail -> parse_chars ([ 'o' ] @ tail) (digits @ [ 2 ])
    | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: tail ->
      parse_chars ([ 'e' ] @ tail) (digits @ [ 3 ])
    | 'f' :: 'o' :: 'u' :: 'r' :: tail -> parse_chars tail (digits @ [ 4 ])
    | 'f' :: 'i' :: 'v' :: 'e' :: tail -> parse_chars ([ 'e' ] @ tail) (digits @ [ 5 ])
    | 's' :: 'i' :: 'x' :: tail -> parse_chars tail (digits @ [ 6 ])
    | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: tail ->
      parse_chars ([ 'n' ] @ tail) (digits @ [ 7 ])
    | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: tail ->
      parse_chars ([ 't' ] @ tail) (digits @ [ 8 ])
    | 'n' :: 'i' :: 'n' :: 'e' :: tail -> parse_chars ([ 'e' ] @ tail) (digits @ [ 9 ])
    | head :: tail ->
      let head_ascii = Char.to_int head in
      if head_ascii >= zero_ascii && head_ascii <= nine_ascii
      then parse_chars tail (digits @ [ head_ascii - zero_ascii ])
      else parse_chars tail digits
  in
  let rec calculate_sum lines =
    match lines with
    | [] -> 0
    | head :: tail ->
      let chars = String.to_list head in
      let digits = parse_chars chars [] in
      let res =
        match digits with
        | [] -> 0
        | head :: tail ->
          (head * 10)
          +
            (match List.rev tail with
            | [] -> head
            | rev_head :: _ -> rev_head)
      in
      res + calculate_sum tail
  in
  input |> String.split_lines |> calculate_sum |> Fmt.str "%d"
;;

let post_handler fn request =
  let open Lwt.Syntax in
  let* form_data = Dream.form ~csrf:false request in
  match form_data with
  | `Ok [ ("input", input) ] -> input |> fn |> Dream.html
  | _ -> Dream.empty `Bad_Request
;;

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream_livereload.inject_script ()
  @@ Dream.router
       [ Dream.get "/" (fun _ ->
           Index.main |> Index.layout |> html_to_string |> Dream.html)
       ; Dream.get "/day1" (fun _ ->
           Index.day "Day 1" "/day1" |> Index.layout |> elt_to_string |> Dream.html)
       ; Dream.post "/day1/a" (post_handler day1_a)
       ; Dream.post "/day1/b" (post_handler day1_b)
       ; Dream.get "/static/**" @@ Dream.static "./static"
       ; Dream_livereload.route ()
       ]
;;
