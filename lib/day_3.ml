let offset = 91;;

let convert_char_to_int c = (int_of_char c) - offset;;

let determine_repeated_char line = 
  let str1 = String.sub line 0 ((String.length line) / 2) in 
  let str2 = String.sub line ((String.length line) / 2) (String.length line+1) in 
  str1 ^ "\n" ^ str2;;
  




let run input = 
  print_endline (List.fold_left (fun acc row -> acc ^ "\n"^ row) "" input)