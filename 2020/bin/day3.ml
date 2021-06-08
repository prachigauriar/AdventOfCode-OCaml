open Core
open Stdio


type slope = { right: int; down: int } 


let count_trees lines slope =
  let count_trees_on_line line_index (count, index) line =
    if line_index % slope.down <> 0 then
      (count, index)
    else 
       let new_index = (index + slope.right) % (String.length line) in
       let new_count = count + (if (Char.equal line.[index] '#') then 1 else 0) in
       (new_count, new_index)
  in
  let (tree_count, _) = List.foldi lines ~init:(0, 0) ~f:count_trees_on_line in
  tree_count


let part1 lines =
  count_trees lines { right = 3; down = 1}
  |> printf "Part 1: %d\n"

let part2 lines =
  let slopes = [
    { right = 1; down = 1 };
    { right = 3; down = 1 };
    { right = 5; down = 1 };
    { right = 7; down = 1 };
    { right = 1; down = 2 }
  ] in
  List.map slopes ~f:(count_trees lines)
  |> List.fold ~init:1 ~f:( * )
  |> printf "Part 2: %d\n"
      

let lines =
  In_channel.input_lines In_channel.stdin
let () = part1 lines
let () = part2 lines
