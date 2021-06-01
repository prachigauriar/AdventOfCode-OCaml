(*
 Advent of Code - Day 4, Part 2

 For added security, yet another system policy has been put in place. Now, a
 valid passphrase must contain no two words that are anagrams of each other -
 that is, a passphrase is invalid if any word's letters can be rearranged to
 form any other word in the passphrase.

 For example:

   - "abcde fghij" is a valid passphrase.
   - "abcde xyz ecdab" is not valid - the letters from the third word can be
     rearranged to form the first word.
   - "a ab abc abd abf abj" is a valid passphrase, because all letters need to
     be used when forming another word.
   - "iiii oiii ooii oooi oooo" is valid.
   - "oiii ioii iioi iiio" is not valid - any of these words can be rearranged
     to form any other word.

 Under this new system policy, how many passphrases are valid?
 *)
open Core


module IntSet = Set.Make (Int)


(*
 We can determine if two words are anagrams by using an “anagram hash”.

 The fundamental theorem of arithmetic states that every integer greater than
 one either is a prime number itself or can be represented as the product of
 prime numbers; moreover, this representation is unique, up to (except for) the
 order of the factors.

 Our anagram hash works by first assigning each letter a prime number, which
 we’ll call the letter’s prime ID. The anagram hash of a string is the product
 of its letters’ prime IDs. Two strings are anagrams if they have the same
 anagram hash, which are guaranteed to uniquely identify a string containing
 precisely those characters, though they may be in a different order.
 *)
let anagram_hash string =
  let prime_id character =
    match character with
    | 'a' -> 2
    | 'b' -> 3
    | 'c' -> 5
    | 'd' -> 7
    | 'e' -> 11
    | 'f' -> 13
    | 'g' -> 17
    | 'h' -> 19
    | 'i' -> 23
    | 'j' -> 29
    | 'k' -> 31
    | 'l' -> 37
    | 'm' -> 41
    | 'n' -> 43
    | 'o' -> 47
    | 'p' -> 53
    | 'q' -> 59
    | 'r' -> 61
    | 's' -> 67
    | 't' -> 71
    | 'u' -> 73
    | 'v' -> 79
    | 'w' -> 83
    | 'x' -> 89
    | 'y' -> 97
    | 'z' -> 101
    | _ -> 0 in
  String.fold ~init:1 ~f:(fun product char -> product * (prime_id char)) string


let count_valid_passphrases () =
  let is_valid_passphrase line =
    let phrase_list = String.split_on_chars line ~on:[' '] in
    let anagram_set = IntSet.of_list (List.map ~f:anagram_hash phrase_list) in
    IntSet.length anagram_set = List.length phrase_list in
  In_channel.fold_lines In_channel.stdin
    ~init:0
    ~f:(fun count line -> count + if is_valid_passphrase line then 1 else 0)


(*
 To run this, pipe the contents of a file to this binary, e.g.,

     cat resources/day4_input.txt | ./day4_passphrase2.native
 *)
let () =
  printf "Valid passphrases = %d\n" (count_valid_passphrases ())
