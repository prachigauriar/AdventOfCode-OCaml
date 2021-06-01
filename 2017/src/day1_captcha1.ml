(*
 Advent of Code - Day 1, Part 1

 Review a string of digits (your puzzle input) and find the sum of all digits
 that match the next digit in the list. The list is circular, so the digit after
 the last digit is the first digit in the list.

 For example:

   - "1122" produces a sum of 3 (1 + 2) because the first digit (1) matches the
     second digit and the third digit (2) matches the fourth digit.
   - "1111" produces 4 because each digit (all 1) matches the next.
   - "1234" produces 0 because no digit matches the next.
   - "91212129" produces 9 because the only digit that matches the next one is
     the last digit, 9.
 *)

open Core
open Test_utilities


(* Returns a string of digits as an int list. *)
let digit_list_of_string string =
  List.filter_map (String.to_list string)
    ~f:(fun c -> int_of_string_opt (Char.to_string c))


let captcha digits =
  let rec captcha' list previous sum =
    match list with
    | [] -> sum
    | head :: tail when head = previous -> captcha' tail head (sum + head)
    | head :: tail -> captcha' tail head sum in
  match digit_list_of_string digits with
  | [] | [_] -> 0
  | head :: tail -> captcha' (tail @ [head]) head 0


let test_data = [
  { input = "1122"; output = 3 };
  { input = "1111"; output = 4 };
  { input = "1234"; output = 0 };
  { input = "91212129"; output = 9 }
]


let () =
  List.iter
    test_data
    ~f:(fun expectation ->
      test
        captcha
        expectation
        "captcha"
        ident
        string_of_int)
