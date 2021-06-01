open Core
open Stdio


(** Finds the first pair (x, y) such that x + y = 2020 and outputs x * y. 
    @param numbers A list of numbers over which to search for x and y.
*)
let part1 numbers =
  let find_pair list sum =
    List.find_map list
      ~f:(fun x -> List.find_map list
             ~f:(fun y -> if x + y = sum then Some (x, y) else None)
         ) in
  printf "Part 1: ";
  match find_pair numbers 2020 with
  | None ->
    printf "No match\n" 
  | Some (x, y) -> 
    printf "%d * %d = %d\n" x y (x * y)


(** Finds the first triplet (x, y, z) such that x + y + z = 2020 and outputs 
    x * y * z. 
    
    @param numbers A list of numbers over which to search for x, y, and z.
*)
let part2 numbers =
  let find_triplet list sum = 
    List.find_map list
      ~f:(fun x -> List.find_map list
             ~f:(fun y -> List.find_map list
                    ~f:(fun z ->
                        if x + y + z = sum then Some (x, y, z) else None
                      )
                )
         ) in
  printf "Part 2: ";
  match find_triplet numbers 2020 with
  | None ->
    printf "No match\n" 
  | Some (x, y, z) -> 
    printf "%d * %d * %d = %d\n" x y z (x * y * z)
      

(* Read the numbers from stdin and then execute part1 and part2 *)
let numbers =
  In_channel.input_lines In_channel.stdin
  |> List.map ~f:int_of_string
let () = 
  part1 numbers;
  part2 numbers
