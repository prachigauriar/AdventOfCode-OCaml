(*
 test_utilities.ml
 Advent of Code

 Created by Prachi Gauriar on 1/21/2018.
 Copyright (c) 2018 Prachi Gauriar. All rights reserved.
 *)

open Core

(** Test Expectation *)
type ('input, 'output) test_expectation = {
  input : 'input; (** The input to the test. *)
  output : 'output (** The expected output of the test. *)
}


(** Returns a sequential list of ints between min and max. *)
let rec int_list ~min ~max =
  if min > max then
    []
  else
    min :: int_list ~min:(min + 1) ~max


(**
 Simplistic test harness that ensures f has the correct output.

   - Parameters:
     - f: the function to test
     - expectation: the test expectation with an input and expected output.
     - f_name: the name of f for output purposes
     - string_of_input: function that converts input type to a string
     - string_of_output: function that converts output type to a string
 *)
let test f expectation f_name string_of_input string_of_output =
  let actual = f expectation.input in
  let input_string = string_of_input expectation.input in
  let actual_string = string_of_output actual in
  if actual = expectation.output then
    printf "Test passed: %s %s = %s\n" f_name input_string actual_string
  else
    let expected_string = string_of_output expectation.output in
    printf "Test failed: %s %s = %s ≠ %s\n" f_name input_string actual_string expected_string
