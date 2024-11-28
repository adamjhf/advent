let read_lines_to_list filename =
  let contents = In_channel.with_open_bin filename In_channel.input_all in
  String.split_on_char '\n' contents
  |> List.filter (fun line -> String.trim line <> "")

type part_number = {
  number: int;
  y: int;
  x_start: int;
  x_end: int;
}

let line_to_part_numbers y line =
  let rec part_numbers x cur acc =
    match line.[x] with