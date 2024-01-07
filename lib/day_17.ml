open Base
open Util

module Problem : Problem.T = struct
  let number_of_day = "17"

  module VisitedNode = struct
    type t =
      { position : int * int
      ; direction : Direction.t
      ; moves : int
      }
    [@@deriving compare, hash, sexp_of]
  end

  type node =
    { position : int * int
    ; heat_loss_sum : int
    ; direction : Direction.t
    }

  let dijkstra grid ~min_moves ~max_moves =
    let visited_nodes, node_heap =
      let position = 0, 0 in
      let start_directions = [ Direction.Right; Direction.Down ] in
      ( Hashtbl.of_alist_exn
          (module VisitedNode)
          (start_directions
           |> List.map ~f:(fun direction ->
             VisitedNode.{ position; direction; moves = 0 }, ()))
      , Pairing_heap.of_list
          (start_directions
           |> List.map ~f:(fun direction -> { position; heat_loss_sum = 0; direction }))
          ~cmp:(fun a b -> a.heat_loss_sum - b.heat_loss_sum) )
    in
    let max_x, max_y = Array.length grid.(0) - 1, Array.length grid - 1 in
    let coord_in_range (x, y) = x >= 0 && x <= max_x && y >= 0 && y <= max_y in
    let iter_moves node direction init_heat_loss =
      let all_moves = List.range min_moves max_moves ~stop:`inclusive in
      List.fold_until
        all_moves
        ~init:init_heat_loss
        ~f:(fun heat_loss moves ->
          let x, y = Direction.move_coordinate ~steps:moves node.position direction in
          if coord_in_range (x, y)
          then (
            let heat_loss = heat_loss + grid.(y).(x) in
            let vn = VisitedNode.{ position = x, y; direction; moves } in
            if Hashtbl.find visited_nodes vn |> Option.is_none
            then (
              Pairing_heap.add
                node_heap
                { position = x, y
                ; heat_loss_sum = node.heat_loss_sum + heat_loss
                ; direction
                };
              Hashtbl.set visited_nodes ~key:vn ~data:());
            Continue heat_loss)
          else Stop ())
        ~finish:(fun _ -> ())
    in
    let iter_directions node =
      let handle_direction direction =
        List.range 1 min_moves
        |> List.fold_until
             ~init:0
             ~f:(fun acc moves ->
               let x, y =
                 Direction.move_coordinate ~steps:moves node.position direction
               in
               if coord_in_range (x, y) then Continue (acc + grid.(y).(x)) else Stop ())
             ~finish:(iter_moves node direction)
      in
      node.direction |> Direction.get_orthogonals |> List.iter ~f:handle_direction
    in
    let rec aux () =
      match Pairing_heap.pop node_heap with
      | None -> failwith "NO!!!"
      | Some node when Coordinate.equal node.position (max_x, max_y) -> node.heat_loss_sum
      | Some node ->
        iter_directions node;
        aux ()
    in
    aux ()
  ;;

  let exec input min_moves max_moves =
    let grid =
      input
      |> String.split_lines
      |> List.to_array
      |> Array.map ~f:(fun row ->
        row |> String.to_array |> Array.map ~f:Char.get_digit_exn)
    in
    grid |> dijkstra ~min_moves ~max_moves |> Fmt.str "%d"
  ;;

  let part_a input = exec input 1 3
  let part_b input = exec input 4 10
end
