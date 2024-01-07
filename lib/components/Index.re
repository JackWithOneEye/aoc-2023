open Tyxml;
open Base;

let doors = solved_days =>
  List.range(1, 24, ~stop=`inclusive)
  |> List.map(~f=day =>
       <button
         class_="p-1 border border-red-800 border-2 rounded-sm cursor-pointer hover:bg-red-600 active:bg-red-700 data-[disabled=true]:bg-green-100 data-[disabled=true]:pointer-events-none"
         _data_disabled={
                          if (day > solved_days) {
                            "true";
                          } else {
                            "false";
                          }
                        }
         _hx_get={"day" ++ Int.to_string(day)}>
         {Html.txt(Int.to_string(day))}
       </button>
     );

let main = solved_days =>
  <div
    id="doors"
    class_="grid grid-cols-4 gap-y-8 gap-x-16 h-full"
    _hx_target="#doors"
    _hx_swap="outerHTML"
    _hx_select="#form"
    _hx_push_url="true">
    ...{doors(solved_days)}
  </div>;

let part_form = (path, part) =>
  <div class_="flex flex-col gap-1">
    <h3 class_="text-1xl"> {Html.txt(part ++ ")")} </h3>
    <form
      class_="flex-1 flex flex-col gap-2"
      _hx_post={path ++ "/" ++ part}
      _hx_target={"#result-" ++ part}
      _hx_swap="innerHTML">
      <textarea
        class_="flex-1 border border-red-800 border-2 bg-green-50 rounded-sm focus:bg-green-100 outline-none p-1"
        name="input"
        placeholder="Insert input here..."
      />
      <button
        class_="p-1 border border-red-800 border-2 rounded-sm hover:bg-red-600 active:bg-red-700">
        <span> "Submit" </span>
        <span class_="htmx-indicator"> "..." </span>
      </button>
    </form>
    <div class_="flex">
      <span> "El Resulto:" </span>
      <span id={"result-" ++ part} />
    </div>
  </div>;

let day = (title, path) =>
  <div id="form" class_="flex flex-col gap-4 h-full w-full">
    <div class_="flex justify-between pb-2">
      <h2 class_="text-2xl"> {Html.txt(title)} </h2>
      <button
        class_="px-2 py-1 border border-red-800 border-2 rounded-sm hover:bg-red-400 active:bg-red-600"
        _hx_get="/"
        _hx_target="#form"
        _hx_select="#doors"
        _hx_swap="outerHTML"
        _hx_push_url="true">
        <span> "Back" </span>
      </button>
    </div>
    <div class_="flex-1 grid grid-cols-2 gap-16">
      {part_form(path, "a")}
      {part_form(path, "b")}
    </div>
  </div>;

let title = "¡AOC 2023!";

let layout = content =>
  <Html.html>
    <head>
      <title> {Html.txt(title)} </title>
      <meta charset="utf-8" />
      <link rel="stylesheet" href="/static/styles.css" />
      <script src="/static/index.js" />
    </head>
    <body class_="bg-green-100 text-red-900 font-sans">
      <div class_="h-screen flex flex-col">
        <header class_="flex items-center px-3 py-4">
          <span class_="italic font-semibold text-4xl">
            {Html.txt(title)}
          </span>
        </header>
        <main class_="contents">
          <div class_="flex flex-col flex-1 gap-x-4 gap-y-4 overflow-auto p-4">
            <div
              class_="w-full bg-green-200 border border-red-900 border-4 rounded-sm shadow flex-1 p-12">
              content
            </div>
          </div>
        </main>
      </div>
    </body>
  </Html.html>;
