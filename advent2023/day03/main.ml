let read_lines_to_list filename =
  let contents = In_channel.with_open_bin filename In_channel.input_all in
  String.split_on_char '\n' contents
  |> List.filter (fun line -> String.trim line <> "")

type part_number = { number : int; y : int; x_start : int; x_end : int }
[@@deriving show]

type position = { x : int; y : int } [@@deriving show]

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

let is_part_number symbols n =
  let width = n.x_end - n.x_start + 2 in
  let above = List.init width (fun x -> { x = x + n.x_start; y = n.y - 1 }) in
  let below = List.init width (fun x -> { x = x + n.x_start; y = n.y + 1 }) in
  let around =
    ({ x = n.x_start - 1; y = n.y } :: { x = n.x_end + 1; y = n.y } :: above)
    @ below
  in
  List.exists
    (fun symbol ->
      List.exists (fun pos -> symbol.x = pos.x && symbol.y = pos.y) around)
    symbols

let () =
  let lines = read_lines_to_list "day03/input.txt" in
  let numbers, symbols =
    List.mapi line_to_part_numbers lines
    |> List.split
    |> fun (x, y) -> (List.flatten x, List.flatten y)
  in
  List.filter (is_part_number symbols) numbers
  |> List.map (fun part -> part.number)
  |> List.map (fun l ->
         Printf.printf "%d\n" l;
         l)
  |> List.fold_left ( + ) 0
  |> Printf.printf "%d\n"
