(*
 Advent of Code - Day 1, Part 2

 Now, instead of considering the next digit, it wants you to consider the digit
 halfway around the circular list. That is, if your list contains 10 items, only
 include a digit in your sum if the digit 10/2 = 5 steps forward matches it.
 Fortunately, your list has an even number of elements.

 For example:

   - "1212" produces 6: the list contains 4 items, and all four digits match the
     digit 2 items ahead.
   - "1221" produces 0, because every comparison is between a 1 and a 2.
   - "123425" produces 4, because both 2s match each other, but no other digit
     has a match.
   - "123123" produces 12.
   - "12131415" produces 4.
 *)

open Core
open Test_utilities


(* Returns a string of digits as an int array. *)
let digit_array_of_string string =
  Array.filter_map (String.to_array string)
    ~f:(fun c -> int_of_string_opt (Char.to_string c))


let captcha digits_string =
  let digits = digit_array_of_string digits_string in
  let length = Array.length digits in
  let half_length = length / 2 in
  let captcha' index checksum value =
    let pair_index = (index + half_length) mod length in
    if digits.(pair_index) = value then
      checksum + value
    else
      checksum in
  Array.foldi ~init:0 ~f:captcha' digits


let test_data = [
  { input = "1212"; output = 6 };
  { input = "1221"; output = 0 };
  { input = "123425"; output = 4 };
  { input = "123123"; output = 12 };
  { input = "12131415"; output = 4}
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
