(*
 day7_tree.ml
 Advent of Code

 Created by Prachi Gauriar on 2/17/2018.
 Copyright (c) 2018 Prachi Gauriar. All rights reserved.
 *)

(*
 Advent of Code - Day 7

 # Part 1

 Wandering further through the circuits of the computer, you come upon a tower
 of programs that have gotten themselves into a bit of trouble. A recursive
 algorithm has gotten out of hand, and now they're balanced precariously in a
 large tower.

 One program at the bottom supports the entire tower. It's holding a large disc,
 and on the disc are balanced several more sub-towers. At the bottom of these
 sub-towers, standing on the bottom disc, are other programs, each holding their
 own disc, and so on. At the very tops of these sub-sub-sub-...-towers, many
 programs stand simply keeping the disc below them balanced but with no disc of
 their own.

 You offer to help, but first you need to understand the structure of these
 towers. You ask each program to yell out their name, their weight, and (if
 they're holding a disc) the names of the programs immediately above them
 balancing on that disc. You write this information down (your puzzle input).
 Unfortunately, in their panic, they don't do this in an orderly fashion; by the
 time you're done, you're not sure which program gave which information.

 For example, if your list is the following:

     pbga (66)
     xhth (57)
     ebii (61)
     havc (66)
     ktlj (57)
     fwft (72) -> ktlj, cntj, xhth
     qoyq (66)
     padx (45) -> pbga, havc, qoyq
     tknk (41) -> ugml, padx, fwft
     jptl (61)
     ugml (68) -> gyxo, ebii, jptl
     gyxo (61)
     cntj (57)

 …then you would be able to recreate the structure of the towers that looks like
 this:

                     gyxo
                   /
              ugml - ebii
            /      \
           |         jptl
           |
           |         pbga
          /        /
     tknk --- padx - havc
          \        \
           |         qoyq
           |
           |         ktlj
            \      /
              fwft - cntj
                   \
                     xhth

 In this example, `tknk` is at the bottom of the tower (the bottom program), and
 is holding up `ugml`, `padx`, and `fwft`. Those programs are, in turn, holding
 up other programs; in this example, none of those programs are holding up any
 other programs, and are all the tops of their own towers. (The actual tower
 balancing in front of you is much larger.)

 Before you're ready to help them, you need to make sure your information is
 correct. What is the name of the bottom program?

 # Part 2

 The programs explain the situation: they can't get down. Rather, they could get
 down, if they weren't expending all of their energy trying to keep the tower
 balanced. Apparently, one program has the wrong weight, and until it's fixed,
 they're stuck here.

 For any program holding a disc, each program standing on that disc forms a
 sub-tower. Each of those sub-towers are supposed to be the same weight, or the
 disc itself isn't balanced. The weight of a tower is the sum of the weights of
 the programs in that tower.

 In the example above, this means that for `ugml`'s disc to be balanced, `gyxo`,
 `ebii`, and `jptl` must all have the same weight, and they do: 61.

 However, for `tknk` to be balanced, each of the programs standing on its disc
 and all programs above it must each match. This means that the following sums
 must all be the same:

    ugml + (gyxo + ebii + jptl) = 68 + (61 + 61 + 61) = 251
    padx + (pbga + havc + qoyq) = 45 + (66 + 66 + 66) = 243
    fwft + (ktlj + cntj + xhth) = 72 + (57 + 57 + 57) = 243
 
 As you can see, tknk's disc is unbalanced: `ugml`'s stack is heavier than the
 other two. Even though the nodes above `ugml` are balanced, `ugml` itself is
 too heavy: it needs to be 8 units lighter for its stack to weigh 243 and keep
 the towers balanced. If this change were made, its weight would be 60.

 Given that exactly one program is the wrong weight, what would its weight need
 to be to balance the entire tower?
 *)
open Core

(* We define shorter versions of these names for brevity’s sake. *)
module SMap = String.Map
module SSet = String.Set


(* A program has a name and weight. *)
type program = {
  name : string;
  weight : int
} [@@deriving sexp]


(* A tree of programs along with a total weight *)
type program_tree = {
  program : program;
  total_weight : int;
  children : program_tree list
} [@@deriving sexp]


(* Takes a string of the form
     «name» («weight»)( -> «child1», «child2», …, «childN»)?
   and returns a a tuple of the name, weight, and a list of children *)
let parse_entry entry =
  let regex = Str.regexp "\\([A-Za-z]+\\) (\\([0-9]+\\))\\( -> \\(.*\\)\\)?" in
  let _ = Str.search_forward regex entry 0 in
  let name = Str.matched_group 1 entry in
  let weight =
    Str.matched_group 2 entry
    |> int_of_string in
  let children =
    try
      Str.matched_group 4 entry
      |> Str.split (Str.regexp ", ")
    with _ -> [] in
  (name, weight, children)


(* Returns a forest (a list of trees) based on the data in entries. *)
let parse_entries entries =
  (* Accumulates entry's data in program_map, children_map, name_set, and 
     child_set.

       - program_map maps names to programs.
       - children_map maps names to a list of child names
       - name_set is a set of all names
       - child_set is a set of all child names *)
  let summarize_entry (program_map, children_map, name_set, child_set) entry =
    let (name, weight, children) = parse_entry entry in
    (
      SMap.set program_map ~key:name ~data:{ name; weight },
      SMap.set children_map ~key:name ~data:children,
      SSet.add name_set name,
      SSet.union child_set (SSet.of_list children)
    ) in

  (* Build the various data structures so that we can understand the tree *)
  let (program_map, children_map, name_set, child_set) =
    List.fold entries
      ~init:(SMap.empty, SMap.empty, SSet.empty, SSet.empty)
      ~f:summarize_entry in

  (* Builds a tree rooted at name using the program_map and children_map defined
     above *)
  let rec build_tree name =
    let child_names = SMap.find_exn children_map name in
    let program = (SMap.find_exn program_map name) in
    let children = List.map child_names ~f:build_tree in
    let total_weight =
      program.weight + (List.fold children
                          ~init:0
                          ~f:(fun sum c -> sum + c.total_weight)) in
    { program; total_weight; children } in

  (* The roots of the trees are the programs that don’t appear in the child set *)
  let roots = SSet.to_list (SSet.diff name_set child_set) in

  (* Build trees for each of the roots *)
  List.map roots ~f:build_tree


let are_children_balanced { program; children } =
  let rec all_equal_to value list =
    match list with
    | [] -> true
    | hd :: tl ->
      if hd <> value then false
      else all_equal_to value tl in
  let child_weights = List.map children ~f:(fun c -> c.total_weight) in
  match child_weights with
  | [] -> true
  | hd :: tl -> all_equal_to hd tl


(* let find_unbalanced_subtree tree =
 *   let unbalanced_tree tree =
 *     List.group tree.children ~break:(fun l r -> l.total_weight <> r.total_weight)
 *     |> List.find_exn ~f:(fun child_list ->
 *         match child_list with
 *         | [_] -> true
 *         | _ -> false
 *       ) in *)
  
      

(* Usage: ./day7_tree.native filename *)
let () =
  if Array.length Sys.argv < 2 then
    printf "Usage: %s filename\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let entries = In_channel.read_lines filename in
    let trees = parse_entries entries in
    let print_tree tree =
      sexp_of_program_tree tree
      |> Sexp.to_string_hum
      |> printf "%s\n" in
    List.iter trees ~f:print_tree
