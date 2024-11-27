module StringMap = Map.Make (String)

let read_lines_to_list filename =
  let contents = In_channel.with_open_bin filename In_channel.input_all in
  String.split_on_char '\n' contents
  |> List.filter (fun line -> String.trim line <> "")

let parse_line_to_sets line =
  try
    let i = String.index line ':' in
    let game_num = String.sub line 5 (i - 5) |> int_of_string in
    let sets_part = String.sub line (i + 1) (String.length line - i - 1) in
    let sets = String.split_on_char ';' sets_part |> List.map String.trim in
    (game_num, sets)
  with Not_found -> failwith "Incorrect line format"

let parse_set set =
  String.split_on_char ',' set
  |> List.map String.trim
  |> List.map (fun color_count ->
         match String.split_on_char ' ' color_count with
         | [ count; color ] -> (color, int_of_string count)
         | _ -> failwith "Invalid color count format")

(*let limits =
    StringMap.empty
    |> StringMap.add "red" 12
    |> StringMap.add "green" 13
    |> StringMap.add "blue" 14

  let is_cube_possible cube =
    match StringMap.find_opt (fst cube) limits with
    | Some limit -> snd cube <= limit
    | None -> failwith ("Unknown color " ^ fst cube)

  let is_set_possible set =
    List.map is_cube_possible set |> List.fold_left ( && ) true

  let is_game_possible game =
      List.map is_set_possible (snd game) |> List.fold_left ( && ) true

    let () =
       read_lines_to_list "day02/input.txt"
       |> List.map parse_line_to_sets
       |> List.map (fun (x, y) -> (x, List.map parse_set y))
       |> List.filter is_game_possible
       |> List.fold_left (fun acc (x, _) -> acc + x) 0
       |> Printf.printf "%d\n"
*)

let init_map =
  StringMap.empty
  |> StringMap.add "red" 0
  |> StringMap.add "green" 0
  |> StringMap.add "blue" 0

let power game =
  List.flatten game
  |> List.fold_left
       (fun acc set ->
         StringMap.update (fst set)
           (function
             | Some prev -> Some (max prev (snd set))
             | None -> failwith ("Unknown color" ^ fst set))
           acc)
       init_map
  |> fun x -> StringMap.fold (fun _key value acc -> acc * value) x 1

let () =
  read_lines_to_list "day02/input.txt"
  |> List.map parse_line_to_sets
  |> List.map (fun (x, y) -> (x, List.map parse_set y))
  |> List.map (fun (_, y) -> power y)
  |> List.fold_left ( + ) 0
  |> Printf.printf "%d\n"
