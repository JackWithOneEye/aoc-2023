open Aoc_2023
open Base
open Components

let html_to_string html = Fmt.str "%a" (Tyxml.Html.pp ()) html
let elt_to_string elt = Fmt.str "%a" (Tyxml.Html.pp_elt ()) elt

let post_handler fn request =
  let open Lwt.Syntax in
  let* form_data = Dream.form ~csrf:false request in
  match form_data with
  | `Ok [ ("input", input) ] -> input |> fn |> Dream.html
  | _ -> Dream.empty `Bad_Request
;;

let day_subrouter (module Problem : Problem.T) =
  let sub_root = "/day" ^ Problem.number in
  Dream.scope
    sub_root
    []
    [ Dream.get "" (fun _ ->
        Index.day ("Day " ^ Problem.number) sub_root
        |> Index.layout
        |> elt_to_string
        |> Dream.html)
    ; Dream.post "/a" (post_handler Problem.part_a)
    ; Dream.post "/b" (post_handler Problem.part_b)
    ]
;;

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream_livereload.inject_script ()
  @@ Dream.router
       [ Dream.get "/" (fun _ ->
           Index.main |> Index.layout |> html_to_string |> Dream.html)
       ; day_subrouter (module Day_1.Problem)
       ; day_subrouter (module Day_2.Problem)
       ; Dream.get "/static/**" @@ Dream.static "./static"
       ; Dream_livereload.route ()
       ]
;;
