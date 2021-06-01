(*
 day6_redistribute.ml
 Advent of Code

 Created by Prachi Gauriar on 2/10/2018.
 Copyright (c) 2018 Prachi Gauriar. All rights reserved.
 *)

(*
 Advent of Code - Day 6

 # Part 1

 A debugger program here is having an issue: it is trying to repair a memory
 reallocation routine, but it keeps getting stuck in an infinite loop.

 In this area, there are sixteen memory banks; each memory bank can hold any
 number of blocks. The goal of the reallocation routine is to balance the blocks
 between the memory banks.

 The reallocation routine operates in cycles. In each cycle, it finds the memory
 bank with the most blocks (ties won by the lowest-numbered memory bank) and
 redistributes those blocks among the banks. To do this, it removes all of the
 blocks from the selected bank, then moves to the next (by index) memory bank
 and inserts one of the blocks. It continues doing this until it runs out of
 blocks; if it reaches the last memory bank, it wraps around to the first one.

 The debugger would like to know how many redistributions can be done before a
 blocks-in-banks configuration is produced that has been seen before.

 For example, imagine a scenario with only four memory banks:

   - The banks start with 0, 2, 7, and 0 blocks. The third bank has the most
     blocks, so it is chosen for redistribution.
   - Starting with the next bank (the fourth bank) and then continuing to the
     first bank, the second bank, and so on, the 7 blocks are spread out over
     the memory banks. The fourth, first, and second banks get two blocks each,
     and the third bank gets one back. The final result looks like this:
     `2 4 1 2`.
   - Next, the second bank is chosen because it contains the most blocks (four).
     Because there are four memory banks, each gets one block. The result is:
     `3 1 2 3`.
   - Now, there is a tie between the first and fourth memory banks, both of
     which have three blocks. The first bank wins the tie, and its three blocks
     are distributed evenly over the other three banks, leaving it with none:
     `0 2 3 4`.
   - The fourth bank is chosen, and its four blocks are distributed such that
     each of the four banks receives one: `1 3 4 1`.
   - The third bank is chosen, and the same thing happens: `2 4 1 2`.

 At this point, we've reached a state we've seen before: `2 4 1 2` was already
 seen. The infinite loop is detected after the fifth block redistribution cycle,
 and so the answer in this example is 5.

 Given the initial block counts in your puzzle input, how many redistribution
 cycles must be completed before a configuration is produced that has been seen
 before?


 # Part 2

 Out of curiosity, the debugger would also like to know the size of the loop:
 starting from a state that has already been seen, how many block redistribution
 cycles must be performed before that same state is seen again?

 In the example above, `2 4 1 2` is seen again after four cycles, and so the
 answer in that example would be 4.

 How many cycles are in the infinite loop that arises from the configuration in
 your puzzle input?
 *)
open Core


module Memory_bank : sig
  type t
  val of_list : int list -> t
  val to_list : t -> int list
  val sexp_of_t : t -> Sexp.t

  val redistribute : t -> t
  val count_reallocations : t -> int * int
end = struct
  type t = int list
  let of_list x = assert (List.length x > 0); x
  let to_list x = x
  let sexp_of_t x = List.sexp_of_t Int.sexp_of_t x


  let redistribute bank =
    let (max_i, max) = List.foldi bank ~init:(0, 0)
      ~f:(fun index (max_index, max) element ->
        if element > max
          then (index, element)
        else
          (max_index, max)) in

    let bank_count = List.length bank in
    let standard_amount = max / bank_count in

    let adds_remaining i =
      let remaining = max mod bank_count in
      if max_i + remaining < bank_count then
        i > max_i && i <= max_i + remaining
      else
        i <= (max_i + remaining) mod bank_count || i > max_i in

    List.mapi bank ~f:(fun i element ->
      if i = max_i then
        standard_amount
      else
        standard_amount + element + (if (adds_remaining i) then 1 else 0))


  let count_reallocations bank =
    let rec reallocate banks bank =
      let next_bank = redistribute bank in
      match List.findi banks ~f:(fun i b -> b = next_bank) with
      | None -> reallocate (next_bank :: banks) next_bank
      | Some (i, _) -> (banks, i + 1) in
    let (banks, cycle_length) = reallocate [bank] bank in
    (List.length banks, cycle_length)
end


let () =
  let bank = Memory_bank.of_list [0; 2; 7; 0] in
  let (step_count, cycle_length) = Memory_bank.count_reallocations bank in
  printf "Bank = %s\n" (Sexp.to_string_hum (Memory_bank.sexp_of_t bank));
  printf "Steps = %d; cycle length = %d\n" step_count cycle_length


let () =
  let bank = Memory_bank.of_list [4; 1; 15; 12; 0; 9; 9; 5; 5; 8; 7; 3; 14; 5; 12; 3] in
  let (step_count, cycle_length) = Memory_bank.count_reallocations bank in
  printf "Bank = %s\n" (Sexp.to_string_hum (Memory_bank.sexp_of_t bank));
  printf "Steps = %d; cycle length = %d\n" step_count cycle_length
