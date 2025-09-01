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
2. a round is one line of the file where
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
(*
let determine_hand_score shape_1 shape_2 =
  match shape_1, shape_2 with
  | Rock, Paper | Paper, Scissor | Scissor, Rock -> 0 (*my loss*)
  | Rock, Rock | Paper, Paper | Scissor, Scissor -> 3 (*draw*)
  | Paper, Rock | Scissor, Paper | Rock, Scissor -> 6 (*my win*)


let score_a_round a b =
let my_shape= convert_char_to_shape a and
  their_shape = convert_char_to_shape b in
  match my_shape, their_shape with
  | Some Rock, Some shape -> 1 + determine_hand_score Rock shape
  | Some Paper, Some shape -> 2 + determine_hand_score Paper shape
  | Some Scissor, Some shape -> 3 + determine_hand_score Scissor shape
  | _, _ -> 0 ;;

let parse_and_accumulate_score acc l =
  match String.split_on_char ' ' l with
  | [a; b] -> acc + score_a_round a b
  | _ -> acc;;


let part_1 input = (*input is a list of strings, each item is of the form `a b`*)
  print_newline (print_int (List.fold_left parse_and_accumulate_score 0 input))
*)
