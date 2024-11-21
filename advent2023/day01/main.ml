module StringMap = Map.Make (String)

let read_lines_to_array filename =
  let contents = In_channel.with_open_bin filename In_channel.input_all in
  String.split_on_char '\n' contents
  |> List.filter (fun line -> String.trim line <> "")
  |> Array.of_list

let digit_map =
  StringMap.empty
  |> StringMap.add "0" 0
  |> StringMap.add "1" 1
  |> StringMap.add "2" 2
  |> StringMap.add "3" 3
  |> StringMap.add "4" 4
  |> StringMap.add "5" 5
  |> StringMap.add "6" 6
  |> StringMap.add "7" 7
  |> StringMap.add "8" 8
  |> StringMap.add "9" 9
  |> StringMap.add "zero" 0
  |> StringMap.add "one" 1
  |> StringMap.add "two" 2
  |> StringMap.add "three" 3
  |> StringMap.add "four" 4
  |> StringMap.add "five" 5
  |> StringMap.add "six" 6
  |> StringMap.add "seven" 7
  |> StringMap.add "eight" 8
  |> StringMap.add "nine" 9

let match_digit_at_pos s i =
  StringMap.fold
    (fun key value acc ->
      let digit_len = String.length key in
      match acc with
      | Some a -> Some a
      | None
        when i + digit_len <= String.length s && key = String.sub s i digit_len
        ->
          Some value
      | None -> acc)
    digit_map None

let find_first_digit s =
  let rec find_digit i =
    if i >= String.length s then raise (Failure ("Digit not found in " ^ s))
    else
      match match_digit_at_pos s i with
      | Some d -> d
      | None -> find_digit (i + 1)
  in
  find_digit 0

let find_last_digit s =
  let rec find_digit i =
    if i < 0 then raise (Failure ("Digit not found in " ^ s))
    else
      match match_digit_at_pos s i with
      | Some d -> d
      | None -> find_digit (i - 1)
  in
  find_digit (String.length s - 1)

let get_calibration_value str =
  (find_first_digit str * 10) + find_last_digit str

let sum_calibration_values arr =
  let values = Array.map get_calibration_value arr in
  Array.fold_left ( + ) 0 values

let lines = read_lines_to_array "day01/input.txt"
let sum_calib = sum_calibration_values lines
let () = Printf.printf "%d\n" sum_calib
