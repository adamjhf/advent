let read_lines_to_list filename =
  let contents = In_channel.with_open_bin filename In_channel.input_all in
  String.split_on_char '\n' contents
  |> List.filter (fun line -> String.trim line <> "")

let parse_line_to_sets line =
  try
    let i = String.index line ':' in
    let sets_part = String.sub line (i + 1) (String.length line - i - 1) in
    String.split_on_char ';' sets_part |> List.map String.trim
  with Not_found -> failwith "Incorrect line format"

let parse_set set =
  String.split_on_char ',' set
  |> List.map String.trim
  |> List.map (fun color_count ->
         match String.split_on_char ' ' color_count with
         | [ count; color ] -> (color, int_of_string count)
         | _ -> failwith "Invalid color count format")

let () =
  read_lines_to_list "day02/input.txt"
  |> List.map parse_line_to_sets
  |> List.map (List.map parse_set)
  |> List.map List.length
  |> List.fold_left ( + ) 0
  |> Printf.printf "%d\n"
