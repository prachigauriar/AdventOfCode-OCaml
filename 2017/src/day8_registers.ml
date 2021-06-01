(* day1_captcha1.ml
   Advent of Code

   Created by Prachi Gauriar on 3/3/2018
   Copyright (c) 2018 Prachi Gauriar. All rights reserved. *)

(* Advent of Code - Day 8
   
   You receive a signal directly from the CPU. Because of your recent assistance
   with jump instructions, it would like you to compute the result of a series
   of unusual register instructions.
   
   Each instruction consists of several parts: the register to modify, whether
   to increase or decrease that register's value, the amount by which to
   increase or decrease it, and a condition. If the condition fails, skip the
   instruction without modifying the register. The registers all start at 0. The
   instructions look like this:
   
       b inc 5 if a > 1
       a inc 1 if b < 5
       c dec -10 if a >= 1
       c inc -20 if c == 10

   These instructions would be processed as follows:

     - Because a starts at 0, it is not greater than 1, and so b is not
       modified.
     - a is increased by 1 (to 1) because b is less than 5 (it is 0).
     - c is decreased by -10 (to 10) because a is now greater than or equal to 1
       (it is 1).  
     - c is increased by -20 (to -10) because c is equal to 10.
   
   After this process, the largest value in any register is 1.

   You might also encounter <= (less than or equal to) or != (not equal
   to). However, the CPU doesn't have the bandwidth to tell you what all the
   registers are named, and leaves that to you to determine.

   What is the largest value in any register after completing the instructions
   in your puzzle input? 

   # Part 2

   To be safe, the CPU also needs to know the highest value held in any register
   during this process so that it can decide how much memory to allocate to
   these operations. For example, in the above instructions, the highest value
   ever held was 10 (in register c after the third instruction was evaluated).
*)

open Core


module Program : sig 
  type t

  val to_int_string_map : t -> int String.Map.t
  val empty : t
  val evaluate_exn : t -> string -> t
end = struct
  type t = int String.Map.t

  let to_int_string_map t = t
  let empty = String.Map.empty

  (* Models increment and decrement operations *)
  module Operation = struct
    type t = Inc | Dec

    let of_string s  =
      match s with
      | "inc" -> Some Inc
      | "dec" -> Some Dec
      | _ -> None

    let of_string_exn s = Option.value_exn (of_string s)

    let evaluate operation op1 op2  =
      match operation with
      | Inc -> op1 + op2
      | Dec -> op1 - op2
  end


  (* Models arithmetic relations *)
  module Relation = struct
    type t = LT | LTE | EQ | NE | GTE | GT

    let of_string s = 
      match s with
      | "<" -> Some LT
      | "<=" -> Some LTE
      | "==" -> Some EQ
      | "!=" -> Some NE
      | ">=" -> Some GTE
      | ">" -> Some GT
      | _ -> None

    let of_string_exn s = Option.value_exn (of_string s)

    let evaluate t lhs rhs =
      match t with
      | LT -> lhs < rhs
      | LTE -> lhs <= rhs
      | EQ -> lhs = rhs
      | NE -> lhs <> rhs
      | GTE -> lhs >= rhs
      | GT -> lhs > rhs
  end


  type instruction = {
      register : string;
      operation : Operation.t;
      operand : int;
      condition_register : string;
      relation : Relation.t;
      relation_operand : int
    }

  
  let instruction_of_string_exn s =
    match (String.split s ~on:' ') with
    | 
      [register; operation_str; operand_str;
       "if"; condition_register; relation_str; relation_operand_str] ->
      {
        register;
        operation = Operation.of_string_exn operation_str;
        operand = int_of_string operand_str;
        condition_register;
        relation = Relation.of_string_exn relation_str;
        relation_operand = int_of_string relation_operand_str
      }
    | _ -> failwith "invalid_instruction"


  
  let read_register program register =
    Option.value (String.Map.find program register) ~default:0

  
  let update_register program register value =
    String.Map.set program register value


  let is_condition_true program inst =
    let relation = inst.relation in
    let lhs = read_register program inst.condition_register in
    let rhs = inst.relation_operand in
    Relation.evaluate relation lhs rhs

  
  let evaluate_exn program inst_str =
    let inst = instruction_of_string_exn inst_str in
    if not (is_condition_true program inst) then program
    else
      let register = inst.register in
      let operation = inst.operation in
      let operand = inst.operand in
      let current_value = read_register program register in
      let new_value = Operation.evaluate operation current_value operand in
      update_register program register new_value 
end


let max_register_value program =
  Program.to_int_string_map program
  |> String.Map.to_alist 
  |> List.reduce
    ~f:(fun (max_key, max_value) (key, value) ->
        if value > max_value then (key, value)
        else (max_key, max_value)
      )

let evaluate_instruction (program, max_pair, max_value) inst =
  let next_program = Program.evaluate_exn program inst in
  let next_max_pair = max_register_value next_program in
  let next_max_value = match next_max_pair with
    | Some (_, value) -> max max_value value
    | _ -> 0 in
  (next_program, next_max_pair, next_max_value)
  


(* Usage: ./day8_registers.native filename *)
let () =
  if Array.length Sys.argv < 2 then
    printf "Usage: %s filename\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let instructions = In_channel.read_lines filename in
    let (program, max_pair, max_value) =
      List.fold instructions
        ~init:(Program.empty, None, 0)
        ~f:evaluate_instruction in
    let (register, value) = Option.value_exn max_pair in
    printf "Register %s holds the max value of %d\n" register value;
    printf "The maximum value held was %d\n" max_value

