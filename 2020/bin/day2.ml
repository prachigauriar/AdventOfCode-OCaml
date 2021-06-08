open Core
open Stdio


type password_policy1 = {
  character: char;
  min_count: int;
  max_count: int
}


type password_policy2 = {
  character: char;
  index1: int;
  index2: int
}


let xor (bool1: bool) (bool2: bool) =
  not (Bool.equal bool1 bool2)


let parse_line ~f line =
  Scanf.sscanf
    line
    "%d-%d %c: %s"
    f


(** Parses lines into a list of (password_policy1, password) pairs and then 
    validates each password using its corresponding policy.
*)
let part1 lines =
  let construct_policy min_count max_count character password = 
      ({ character; min_count; max_count }, password) in

  let validate_password (policy: password_policy1) password  =
    let char_count = String.count password
        ~f:(fun c -> Char.equal c policy.character) in
    char_count >= policy.min_count && char_count <= policy.max_count in

  List.map lines ~f:(parse_line ~f:construct_policy)
  |> List.count ~f:(fun (policy, password) -> validate_password policy password)
  |> printf "Part 1: %d\n" 
    

(** Parses lines into a list of (password_policy2, password) pairs and then 
    validates each password using its corresponding policy.
*)
let part2 lines =
  let construct_policy index1 index2 character password = 
    ({ character; index1 = index1 - 1; index2 = index2 - 1 }, password) in

  let validate_password policy password =
    let equals_policy_char = Char.equal policy.character in 
    xor
      (equals_policy_char password.[policy.index1])
      (equals_policy_char password.[policy.index2]) in

  List.map lines ~f:(parse_line ~f:construct_policy)
  |> List.count ~f:(fun (policy, password) -> validate_password policy password)
  |> printf "Part 2: %d\n" 


let lines =
  In_channel.input_lines In_channel.stdin
let () = 
  part1 lines;
  part2 lines
