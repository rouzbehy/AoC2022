
let use_msg = "Present solution to Advent of Code Problems [-problem_no]"
let problem_no = ref 0;;
let anon_fun _ = ()

let speclist = [
  ("-problem_no", Arg.Set_int problem_no, "Pick the problem to solve")
]

let read_input_file input_choice =
  In_channel.with_open_text (
    String.concat "" ["./data/day_"; (string_of_int input_choice) ; ".txt"]
  )  In_channel.input_lines;;


let () =  Arg.parse speclist anon_fun use_msg in
  match !problem_no with
    | 1 -> AoC2022.Day_1.run (read_input_file !problem_no)
    | 2 -> AoC2022.Day_2.part_1 (read_input_file !problem_no)
    | _ -> print_endline "Invalid option";;
