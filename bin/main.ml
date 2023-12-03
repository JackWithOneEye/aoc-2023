open Components

let html_to_string html = Fmt.str "%a" (Tyxml.Html.pp ()) html
(* let elt_to_string elt = Fmt.str "%a" (Tyxml.Html.pp_elt ()) elt *)

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream_livereload.inject_script ()
  @@ Dream.router
       [ Dream.get "/" (fun _ -> Index.layout |> html_to_string |> Dream.html)
       ; Dream.get "/static/**" @@ Dream.static "./static"
       ; Dream_livereload.route ()
       ]
;;
