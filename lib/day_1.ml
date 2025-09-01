let rec group input result =
  match input with
  | [] -> result
  | "" :: rest -> group rest (0 :: result)
  | cals :: rest ->
    group rest  (match result with
    | [] -> [int_of_string cals]
    | hd :: tail -> (hd + int_of_string cals) :: tail);;

let rec max_of_list input curr =
  match input with
  | [] -> curr
  | hd::rest -> max_of_list rest (max hd curr);;

let rec max_3 input (m1, m2, m3) =
  match input with
  | [] -> (m1, m2, m3)
  | hd :: rest ->
    max_3 rest (match (m1,m2,m3) with
                  | m1, m2, _ when hd > m1 -> (hd, m1, m2)
                  | m1, m2, _ when hd > m2 -> (m1, hd, m2)
                  | m1, m2, m3 when hd > m3 -> (m1, m2, hd)
                  | _ -> (m1, m2, m3));;

let part_one input= max_of_list (group input []) 0;;

let part_two input =
    let (a, b, c) = max_3 (group input [])(0, 0, 0) in
      (a+b+c);;

let run raw_input =
  print_string ((string_of_int (part_one raw_input)) ^
  "\t"
  ^ (string_of_int (part_two raw_input)));;
