open Aoc_2023
open Base
open Components

let html_to_string html = Fmt.str "%a" (Tyxml.Html.pp ()) html
(* let elt_to_string elt = Fmt.str "%a" (Tyxml.Html.pp_elt ()) elt *)

let post_handler fn request =
  let open Lwt.Syntax in
  let* form_data = Dream.form ~csrf:false request in
  match form_data with
  | `Ok [ ("input", input) ] -> input |> fn |> Dream.html
  | _ -> Dream.empty `Bad_Request
;;

let day_subrouter (module Problem : Problem.T) =
  let sub_root = "/day" ^ Problem.number_of_day in
  let page_html =
    Index.day ("Day " ^ Problem.number_of_day) sub_root |> Index.layout |> html_to_string
  in
  Dream.scope
    sub_root
    []
    [ Dream.get "" (fun _ -> Dream.html page_html)
    ; Dream.post "/a" @@ post_handler Problem.part_a
    ; Dream.post "/b" @@ post_handler Problem.part_b
    ]
;;

let day_subrouters =
  [ (module Day_1.Problem)
  ; (module Day_2.Problem)
  ; (module Day_3.Problem)
  ; (module Day_4.Problem)
  ; (module Day_5.Problem)
  ; (module Day_6.Problem)
  ; (module Day_7.Problem)
  ; (module Day_8.Problem)
  ; (module Day_9.Problem)
  ; (module Day_10.Problem)
  ]
  |> List.map ~f:day_subrouter
;;

let main_html =
  day_subrouters |> List.length |> Index.main |> Index.layout |> html_to_string
;;

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream_livereload.inject_script ()
  @@ Dream.router
       ([ Dream.get "/" (fun _ -> Dream.html main_html)
        ; Dream.get "/static/**" @@ Dream.static "./static"
        ; Dream_livereload.route ()
        ]
        @ day_subrouters)
;;
