let read_lines_to_array filename =
  let contents = In_channel.with_open_bin filename In_channel.input_all in
  String.split_on_char '\n' contents
  |> List.filter (fun line -> String.trim line <> "")
  |> Array.of_list

let find_first_digit s =
  let len = String.length s in
  let rec find_digit i =
    if i >= len then raise (Failure ("Digit not found in " ^ s))
    else if s.[i] >= '0' && s.[i] <= '9' then int_of_char s.[i] - int_of_char '0'
    else find_digit (i + 1)
  in
  find_digit 0

let find_last_digit s =
  let rec find_digit i =
    if i < 0 then raise (Failure ("Digit not found in " ^ s))
    else if s.[i] >= '0' && s.[i] <= '9' then int_of_char s.[i] - int_of_char '0'
    else find_digit (i - 1)
  in
  find_digit (String.length s - 1)

let get_calibration_value str =
  find_first_digit str * 10 + find_last_digit str

let sum_calibration_values arr =
  let values = Array.map get_calibration_value arr in
  Array.fold_left (+) 0 values

let lines = read_lines_to_array "day01/input.txt"
let sum_calib = sum_calibration_values lines
let () = Printf.printf "%d\n" sum_calib
