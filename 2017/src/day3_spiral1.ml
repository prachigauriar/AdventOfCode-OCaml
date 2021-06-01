(*
 Advent of Code - Day 3, Part 1

 Each square on the grid is allocated in a spiral pattern starting at a
 location marked 1 and then counting up while spiraling outward. For example,
 the first few squares are allocated like this:

     17  16  15  14  13
     18   5   4   3  12
     19   6   1   2  11
     20   7   8   9  10
     21  22  23---> ...

 While this is very space-efficient (no squares are skipped), requested data
 must be carried back to square 1 (the location of the only access port for
 this memory system) by programs that can only move up, down, left, or right.
 They always take the shortest path: the Manhattan Distance between the
 location of the data and square 1.

 For example:

   - Data from square 1 is carried 0 steps, since it's at the access port.
   - Data from square 12 is carried 3 steps, such as: down, left, left.
   - Data from square 23 is carried only 2 steps: up twice.
   - Data from square 1024 must be carried 31 steps.

 How many steps are required to carry the data from the square identified in
 your puzzle input all the way to the access port?
 *)
open Core
open Test_utilities

(*
 My solution just uses math. We break the grid into rings, with the innermost
 ring called Ring 0, the next called Ring 1, Ring 2, etc.

 ┌──────────────────────────────────┐
 │ 37   36   35   34   33   32   31 │
 │    ┌────────────────────────┐    │
 │ 38 │ 17   16   15   14   13 │ 30 │
 │    │    ┌──────────────┐    │    │
 │ 39 │ 18 │  5    4    3 │ 12 │ 29 │
 │    │    │     ┌───┐    │    │    │
 │ 40 │ 19 │  6  │ 1 │  2 │ 11 │ 28 │
 │    │    │     └───┘    │    │    │
 │ 41 │ 20 │  7    8    9 │ 10 │ 27 │
 │    │    └─── Ring  1 ──┘    │    │
 │ 42 │ 21   22   23   24   25 │ 26 │
 │    └──────── Ring  2 ───────┘    │
 │ 43   44   45   46   47   48   49 │
 └───────────── Ring  3 ────────────┘

 *)

(* The width of Ring i (in columns) is 2i + 1. *)
let ring_width i =
  assert (i >= 0);
  2 * i + 1


(* The maximum value in a ring is just the square of its width. *)
let ring_max_value i =
  assert (i >= 0);
  (ring_width i) * (ring_width i)


(*
 The minimum value in a ring is one more than the previous ring’s max value. For
 example, Ring 1’s max value is 9, so Ring 2’s min value is 10.
 *)
let ring_min_value i =
  assert (i >= 0);
  match i with
  | 0 -> 1
  | _ -> ring_max_value (i - 1) + 1


(*
 The minimum distance from an element in Ring_i to 1 is i. This happens on the
 center points of the ring’s edges.
 *)
let ring_min_distance i =
  assert (i >= 0);
  i


(*
 The maximum distance from an element in Ring_i to 1 is 2i. This happens at the
 corners. Corners are i spaces away from the center points of the edges, which
 are in turn i spaces from 1. i + i = 2i.
 *)
let ring_max_distance i =
  assert (i >= 0);
  2 * i


(*
 The length of any edge of a ring is simply the total number of elements in the
 ring divided by 4. Rings are squares, so they will always have four edges of
 equal length. The number of elements in the max value - min value + 1. But we
 can actually do some math to simplify this:

    ElementCount(i)
      = R_max(i) - R_min(i) + 1
      = (2i + 1)^2 - ((2(i - 1) + 1)^2 + 1) + 1
      = 4i^2 + 4i + 1 - 4i^2 + 4i - 2 + 1
      = 8i

    EdgeLength(i) = 8i / 4 = 2i

  This is precisely the same as the max distance of an element in Ring_i to
  1, i.e. the distance to a corner. This makes sense, because to get to a
  corner from the center, you have to go half an edge length left or right,
  followed by half an edge length up or down. So you go a full edge length.
 *)
let ring_edge_length = ring_max_distance


(*
 The values in a given ring are just the values between square odds. That is,

     Ring 0 = { 1^2 }
     Ring 1 = { n | 1^2 < n ≤ 3^2 }
     Ring 2 = { n | 3^2 < n ≤ 5^2 }
            …
     Ring i = { n | (2i - 1)^2 < n ≤ (2i + 1)^2 }
            …

 Thus, we can determine which ring a number n is in by determining the next
 square odd immediately after n. We can then convert that number to its
 corresponding ring by doing a little math.

 By rearranging these operations somewhat, we find that

     let s = ceil(sqrt(n))

 Then i = (s - s mod 2) / 2.

 *)
let ring_number n =
  assert (n > 0);
  let s = int_of_float (Float.round_up (sqrt (float_of_int n))) in
  (s - s mod 2) / 2


type edge =
  | North
  | East
  | South
  | West


let edge_of_int i =
  match i mod 4 with
  | 0 -> East
  | 1 -> North
  | 2 -> West
  | _ -> South


(*
 If n = 1, its coordinate is (0, 0). Otherwise, we need to do the following:

   - Calculate the ring_number of n (let’s call this R)
   - Determine the offset of n from the beginning of R. This is just
     n - ring_min_value R
   - Determine which edge n is on by dividing the offset by the edge length of R
   - Determine where on the edge n is (edge_offset), which is just the offset
     mod the edge_length of R

 So now we know which edge n is on and where on the edge it is. Every edge is
 going to have one of its coordinates pinned R from the center, though the
 specifics depend on the edge:

   - East: the X coordinate is always R.
   - North: the Y coordinate is always R.
   - West: the X coordinate is always -R.
   - South: the Y coordinate is always -R.

 For the other coordinate, in the East and South cases, we’re increasing towards
 R, while in the North and West cases, we’re decreasing away from R, which is
 expressed by the following:

   - East: the Y coordinate is (edge_offset + 1) - R
   - North: the X coordinate is R - (edge_offset + 1)
   - West: the Y coordinate is R - (edge_offset + 1)
   - South: the X coordinate is (edge_offset + 1) - R
 *)
let coordinate n =
  assert (n > 0);
  if n = 1 then
    (0, 0)
  else
    let ring = ring_number n in
    let edge_length = ring_edge_length ring in
    let offset = n - ring_min_value ring in
    let edge = edge_of_int (offset / edge_length) in
    let edge_offset = offset mod edge_length in
    match edge with
    | East -> (ring, (edge_offset + 1) - ring)
    | North -> (ring - (edge_offset + 1), ring)
    | West -> (-ring, ring - (edge_offset + 1))
    | South -> ((edge_offset + 1) - ring, -ring)


(*
 The spiral distance of a given n from the center is just the sum of the absolute
 values of its coordinates.
 *)
let spiral_distance n =
  assert (n > 0);
  let (x, y) = coordinate n in
  abs x + abs y


let test_data = [
  { input = 1; output = 0 };
  { input = 12; output = 3 };
  { input = 23; output = 2 };
  { input = 1024; output = 31 };
  { input = 289326; output = 419 };
]


let () =
  List.iter
    test_data
    ~f:(fun expectation ->
      test
        spiral_distance
        expectation
        "spiral_distance"
        string_of_int
        string_of_int)
