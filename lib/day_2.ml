type shape = (* love too make new types on the fly!*)
  | Rock
  | Paper
  | Scissor

type result =
  | WIN
  | LOSE
  | DRAW

(* terminology:
1. the game is the entirety of the file
2. a round is one line of the file where column 2 is what I play
and column 1 is what the opponent plays
note: I initially wrote this code assuming the reverse: my mistake in
not reading the instructions correctly!
*)

let convert_char_to_shape char = match char with
  | "X" | "A" -> Rock
  | "Y" | "B" -> Paper
  | "Z" | "C" -> Scissor
  | _ -> Rock

let get_value_of_shape s = match s with | Rock -> 1 | Paper -> 2 |Scissor -> 3

let determine_result shape_1 shape_2 =
  match shape_1, shape_2 with
  | Rock, Scissor | Scissor, Paper | Paper, Rock -> WIN
  | Rock, Paper | Paper, Scissor | Scissor, Rock -> LOSE
  | Rock, Rock | Paper, Paper | Scissor, Scissor -> DRAW

let partial_score_a_hand r = match r with | WIN -> 6 | DRAW -> 3 | LOSE -> 0

let full_round_score line =
  match String.split_on_char ' ' line with
  | [c1; c2] ->
  let h1, h2 = convert_char_to_shape c1, convert_char_to_shape c2 in
    (get_value_of_shape h2) + partial_score_a_hand (determine_result h2 h1)
  | _ -> 0


let map_game_to_scores list =
  List.fold_left (fun acc row -> acc @ [full_round_score row]) [] list

let map_game_to_score list =
  List.fold_left (fun acc row -> acc + full_round_score row) 0 list

let part_1 input =
  input |> map_game_to_score |> print_int |> print_newline

(*part 2 assigns a different interpretation to the columns of the file*)
let convert_char_to_shape_v2 c = match c with
  |"A" -> Rock
  |"B" -> Paper
  |"C" -> Scissor
  | _ -> Rock
