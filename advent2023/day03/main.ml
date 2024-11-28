let read_lines_to_list filename =
  let contents = In_channel.with_open_bin filename In_channel.input_all in
  String.split_on_char '\n' contents
  |> List.filter (fun line -> String.trim line <> "")

type part_number = { number : int; y : int; x_start : int; x_end : int }
[@@deriving show]

type symbol = { x : int; y : int } [@@deriving show]
type parsed_line = part_number list * symbol list [@@deriving show]

let digit_to_int c = int_of_string (String.make 1 c)

let new_part number x y =
  {
    number;
    y;
    x_start = x + 1 - String.length (string_of_int number);
    x_end = x;
  }

let line_to_part_numbers y line =
  let rec get_part_numbers x cur parts syms =
    match line.[x] with
    | '0' .. '9' as digit ->
        get_part_numbers (x + 1) ((10 * cur) + digit_to_int digit) parts syms
    | '.' when cur > 0 ->
        get_part_numbers (x + 1) 0 (new_part cur x y :: parts) syms
    | '.' -> get_part_numbers (x + 1) 0 parts syms
    | exception _ -> (parts, syms)
    | _ when cur > 0 ->
        get_part_numbers (x + 1) 0 (new_part cur x y :: parts) ({ x; y } :: syms)
    | _ -> get_part_numbers (x + 1) 0 parts ({ x; y } :: syms)
  in
  get_part_numbers 0 0 [] []

let () =
  let lines = read_lines_to_list "day03/input.txt" in
  let parsed = line_to_part_numbers 1 (List.nth lines 1) in
  print_endline (show_parsed_line parsed)
