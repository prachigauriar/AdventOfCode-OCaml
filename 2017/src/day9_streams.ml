(* day9_streams.ml
   Advent of Code

   Created by Prachi Gauriar on 3/3/2018
   Copyright (c) 2018 Prachi Gauriar. All rights reserved. *)

(* advent of Code - Day 9
   
   # Part 1
   
   A large stream blocks your path. According to the locals, it's not safe to
   cross the stream at the moment because it's full of garbage. You look down at
   the stream; rather than water, you discover that it's a stream of characters.
   
   You sit for a while and record part of the stream (your puzzle input). The
   characters represent groups - sequences that begin with { and end with
   }. Within a group, there are zero or more other things, separated by commas:
   either another group or garbage. Since groups can contain other groups, a }
   only closes the most-recently-opened unclosed group - that is, they are
   nestable. Your puzzle input represents a single, large group which itself
   contains many smaller ones.
   
   Sometimes, instead of a group, you will find garbage. Garbage begins with <
   and ends with >. Between those angle brackets, almost any character can
   appear, including { and }. Within garbage, < has no special meaning.
   
   In a futile attempt to clean up the garbage, some program has canceled some
   of the characters within it using !: inside garbage, any character that comes
   after ! should be ignored, including <, >, and even another !.
   
   You don't see any characters that deviate from these rules. Outside garbage,
   you only find well-formed groups, and garbage always terminates according to
   the rules above.

   Here are some self-contained pieces of garbage:

     - `<>`, empty garbage.
     - `<random characters>`, garbage containing random characters.
     - `<<<<>`, because the extra < are ignored.
     - `<{!>}>`, because the first `>` is canceled.
     - `<!!>`, because the second `!` is canceled, allowing the `>` to terminate the 
       garbage.
     - `<!!!>>`, because the second `!` and the first `>` are canceled.
     - `<{o"i!a,<{i<a>`, which ends at the first `>`.

   Here are some examples of whole streams and the number of groups they contain:

     - `{}`, 1 group.
     - `{{{}}}`, 3 groups.
     - `{{},{}}`, also 3 groups.
     - `{{{},{},{{}}}}`, 6 groups.
     - `{<{},{},{{}}>}`, 1 group (which itself contains garbage).
     - `{<a>,<a>,<a>,<a>}`, 1 group.
     - `{{<a>},{<a>},{<a>},{<a>}}`, 5 groups.
     - `{{<!>},{<!>},{<!>},{<a>}}`, 2 groups (since all but the last `>` are 
       canceled).
   
   Your goal is to find the total score for all groups in your input. Each group
   is assigned a score which is one more than the score of the group that
   immediately contains it. (The outermost group gets a score of 1.)

     - `{}`, score of 1.
     - `{{{}}}`, score of 1 + 2 + 3 = 6.
     - `{{},{}}`, score of 1 + 2 + 2 = 5.
     - `{{{},{},{{}}}}`, score of 1 + 2 + 3 + 3 + 3 + 4 = 16.
     - `{<a>,<a>,<a>,<a>}`, score of 1.
     - `{{<ab>},{<ab>},{<ab>},{<ab>}}`, score of 1 + 2 + 2 + 2 + 2 = 9.
     - `{{<!!>},{<!!>},{<!!>},{<!!>}}`, score of 1 + 2 + 2 + 2 + 2 = 9.
     - `{{<a!>},{<a!>},{<a!>},{<ab>}}`, score of 1 + 2 = 3.
   
   What is the total score for all groups in your input?

   
   # Part 2
   
   Now, you're ready to remove the garbage.

   To prove you've removed it, you need to count all of the characters within
   the garbage. The leading and trailing < and > don't count, nor do any
   canceled characters or the ! doing the canceling.

     - `<>`, 0 characters.
     - `<random characters>`, 17 characters.
     - `<<<<>`, 3 characters.
     - `<{!>}>`, 2 characters.
     - `<!!>`, 0 characters.
     - `<!!!>>`, 0 characters.
     - `<{o"i!a,<{i<a>`, 10 characters.
   
   How many non-canceled characters are within the garbage in your puzzle input? *)

open Core


module Stream_processor : sig
  type t [@@deriving sexp]
  type group [@@deriving sexp]
  
  val empty : t
  val process_exn : t -> char -> t
  val scores : t -> (group * int) list
  val garbage_count : t -> int
end = struct
  module Group : sig
    type t = { subgroups : t list } [@@deriving sexp]

    val empty : t
    val append_subgroup : t -> t -> t
    val score : t -> int
  end = struct
    type t = { subgroups : t list } [@@deriving sexp]

    let empty = { subgroups = [] }
    let append_subgroup parent subgroup =
      { subgroups = parent.subgroups @ [subgroup] }

    let score t =
      let rec score' t nest_level =
        List.fold t.subgroups
          ~init:nest_level
          ~f:(fun sum subgroup -> sum + (score' subgroup (nest_level + 1))) in
      score' t 1
  end [@@deriving sexp]

  type group = Group.t
  let sexp_of_group = Group.sexp_of_t
  let group_of_sexp = Group.t_of_sexp
  
  type token = Group | Garbage | Cancel [@@deriving sexp]


  type t = {
    group_stack : Group.t list;
    token_stack : token list;
    completed : Group.t list;
    garbage_count : int
  } [@@deriving sexp]

  
  let empty = {
    group_stack = [];
    token_stack = [];
    completed = [];
    garbage_count = 0
  }

  
  let process_empty_exn t c =
    match c with
    | '{' -> { t with group_stack = [Group.empty]; token_stack = [Group] }
    | '<' -> { t with token_stack = [Garbage] }
    | _ -> t


  let process_group_open t = 
    let { group_stack; token_stack } = t in
    { t with group_stack = Group.empty :: group_stack;
             token_stack = Group :: token_stack }

  let process_group_close t =
    let { group_stack; token_stack; completed } = t in
    let token_stack = List.tl_exn token_stack in
    match group_stack with
    | group :: parent :: tl ->
      { t with group_stack = (Group.append_subgroup parent group) :: tl;
               token_stack }
    | group :: tl ->
      { t with group_stack = tl;
               token_stack;
               completed = completed @ [group] }
    | _ -> failwith "invalid stream"

  
  let process_group t c =
    match c with 
    | '{' -> process_group_open t 
    | '}' -> process_group_close t 
    | '<' -> { t with token_stack = Garbage :: t.token_stack }
    | _ -> t
               
  
  let process_garbage t c =
    match c with
    | '>' -> { t with token_stack = List.tl_exn t.token_stack }
    | '!' -> { t with token_stack = Cancel :: t.token_stack }
    | _ -> { t with garbage_count = t.garbage_count + 1 }

  
  let process_exn t c =
    match t.token_stack with
    | [] -> process_empty_exn t c
    | token :: tl -> match token with
      | Group -> process_group t c
      | Garbage -> process_garbage t c
      | Cancel -> { t with token_stack = tl }


  let scores t =
    List.map t.completed ~f:(fun group -> (group, Group.score group))

  let garbage_count t = t.garbage_count
end


(* Usage: ./day9_streams.native [filename] *)
let () =
  let stream = if Array.length Sys.argv < 2 then
      In_channel.input_all In_channel.stdin
    else
      In_channel.read_all Sys.argv.(1) in
  let processed_stream = String.fold stream
    ~init:Stream_processor.empty
    ~f:(fun s c -> Stream_processor.process_exn s c) in
  Stream_processor.scores processed_stream
  |> List.iter 
    ~f:(fun (group, score) ->
        Stream_processor.sexp_of_group group
        |> Sexp.to_string
        |> printf "(score %d) (group %s)\n" score 
      );
  printf "Garbage count = %d\n" (Stream_processor.garbage_count processed_stream)
