type shape = (* love too make new types on the fly!*)
  | ROCK
  | PAPER
  | SCISSOR

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
  | "X" | "A" -> ROCK
  | "Y" | "B" -> PAPER
  | "Z" | "C" -> SCISSOR
  | _ -> ROCK

let get_value_of_shape s = match s with | ROCK -> 1 | PAPER -> 2 |SCISSOR -> 3

let determine_result shape_1 shape_2 =
  match shape_1, shape_2 with
  | ROCK, SCISSOR | SCISSOR, PAPER | PAPER, ROCK -> WIN
  | ROCK, PAPER | PAPER, SCISSOR | SCISSOR, ROCK -> LOSE
  | ROCK, ROCK | PAPER, PAPER | SCISSOR, SCISSOR -> DRAW

let partial_score_a_hand r = match r with | WIN -> 6 | DRAW -> 3 | LOSE -> 0

let full_round_score line =
  match String.split_on_char ' ' line with
  | [c1; c2] ->
  let h1, h2 = convert_char_to_shape c1, convert_char_to_shape c2 in
    (get_value_of_shape h2) + partial_score_a_hand (determine_result h2 h1)
  | _ -> 0


let map_game_to_scores list =
  List.fold_left (fun acc row -> acc @ [full_round_score row]) [] list

let map_game_to_score list func =
  List.fold_left (fun acc row -> acc + func row) 0 list

let part_1 input = (map_game_to_score input full_round_score)

(*part 2 assigns a different interpretation to the columns of the file*)
let convert_char_to_shape_v2 c = match c with
  |"A" -> ROCK
  |"B" -> PAPER
  |"C" -> SCISSOR
  | _ -> ROCK

let convert_char_to_a_result c = 
  match c with 
  | "X" -> LOSE
  | "Y" -> DRAW
  | "Z" -> WIN
  | _ -> WIN

let determine_hand_to_play opp_hand result = 
    match opp_hand, result with 
   | ROCK, LOSE | SCISSOR, DRAW | PAPER, WIN -> SCISSOR
   | ROCK, DRAW | SCISSOR, WIN | PAPER, LOSE -> ROCK
   | ROCK, WIN | SCISSOR, LOSE | PAPER, DRAW -> PAPER

let full_round_score_v2 line = match String.split_on_char ' ' line with 
  | [c1; c2] -> 
    let h1, r = convert_char_to_shape_v2 c1, convert_char_to_a_result c2 
      in (get_value_of_shape (determine_hand_to_play h1 r))
      + partial_score_a_hand r
| _ -> 0

let part_2 input =  (map_game_to_score input full_round_score_v2)
let run input= 
  let v1, v2 = part_1 input, part_2 input in
   print_string ("part 1: " ^ (string_of_int v1) ^ " and part 2: " ^ (string_of_int v2) ^ "\n");