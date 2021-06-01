(*
 Advent of Code - Day 4, Part 1

 A new system policy has been put in place that requires all accounts to use a
 passphrase instead of simply a password. A passphrase consists of a series of
 words (lowercase letters) separated by spaces.

 To ensure security, a valid passphrase must contain no duplicate words.

 For example:

   - aa bb cc dd ee is valid.
   - aa bb cc dd aa is not valid - the word aa appears more than once.
   - aa bb cc dd aaa is valid - aa and aaa count as different words.

 The system's full passphrase list is available as your puzzle input. How many
 passphrases are valid?
 *)
open Core


module StringSet = Set.Make (String)


let count_valid_passphrases () =
  let is_valid_passphrase line =
    let phrase_list = String.split_on_chars line ~on:[' '] in
    let phrase_set = StringSet.of_list phrase_list in
    StringSet.length phrase_set = List.length phrase_list in
  In_channel.fold_lines In_channel.stdin
    ~init:0
    ~f:(fun count line -> count + if is_valid_passphrase line then 1 else 0)


(*
 To run this, pipe the contents of a file to this binary, e.g.,

     cat resources/day4_input.txt | ./day4_passphrase1.native
 *)
let () =
  printf "Valid passphrases = %d\n" (count_valid_passphrases ())
