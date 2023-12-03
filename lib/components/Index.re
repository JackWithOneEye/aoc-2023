open Tyxml;

let title = "AOC 2023!";

let layout =
  <Html.html>
    <head>
      <title> {Html.txt(title)} </title>
      <meta charset="utf-8" />
      <link rel="stylesheet" href="/static/styles.css" />
      <script src="/static/index.js" />
    </head>
    <body class_="bg-green-100 text-red-950 font-sans">
      <div class_="h-screen flex flex-col">
        <header class_="flex items-center px-3 pt-2">
          <span class_="italic font-semibold text-3xl">
            {Html.txt(title)}
          </span>
        </header>
        <main class_="contents" />
      </div>
    </body>
  </Html.html>;
