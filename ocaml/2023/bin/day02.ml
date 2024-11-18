open Core
open Printf

let read_lines (path : string) : string list =
  let ic = In_channel.create path in
  let contents = In_channel.input_lines ic in
  In_channel.close ic;
  contents
;;

let find_substring (src : string) (sub : string) : (int * int) option =
  let len_src = String.length src in
  let len_sub = String.length sub in
  let rec aux i =
    if i + len_sub > len_src
    then None
    else if String.equal (String.sub ~pos:i ~len:len_sub src) sub
    then Some (i, i + len_sub - 1)
    else aux (i + 1)
  in
  aux 0
;;

let max_red = 12
let max_green = 13
let max_blue = 14

type cube_set =
  { red : int
  ; green : int
  ; blue : int
  }

let print_cube_set rbg =
  printf "{ red = %i; green = %i; blue = %i; }\n" rbg.red rbg.green rbg.blue
;;

let update_cube_set cs color count =
  match color with
  | "red" -> { cs with red = count }
  | "green" -> { cs with green = count }
  | "blue" -> { cs with blue = count }
  | _ -> failwith "Invalid color"
;;

let rec create_cube_set (rolls : (int * string) list) (init : cube_set) =
  match rolls with
  | [] -> init
  | (count, color) :: [] -> update_cube_set init color count
  | (count, color) :: rolls_subset ->
    create_cube_set rolls_subset (update_cube_set init color count)
;;

let minimum_cube_set (counts : cube_set list) : cube_set =
  let init = { red = 0; green = 0; blue = 0 } in
  List.fold counts ~init ~f:(fun acc count ->
    { red = max acc.red count.red
    ; green = max acc.green count.green
    ; blue = max acc.blue count.blue
    })
;;

let calculate_game_power minimum_cube_set =
  minimum_cube_set.red * minimum_cube_set.green * minimum_cube_set.blue
;;

let parse_game_id (line : string) : int =
  (* Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green *)
  let id_start =
    match find_substring line "Game " with
    | Some (_, stop) -> stop + 1
    | None -> -1
  in
  let id_end =
    match find_substring line ":" with
    | Some (start, _) -> start
    | None -> -1
  in
  let len = id_end - id_start in
  let id = String.sub line ~pos:id_start ~len in
  int_of_string id
;;

let extract_game_text line =
  (* " 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" *)
  match String.split line ~on:':' with
  | [ _; right ] -> right (* "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" *)
  | _ -> failwith "Could not split line on ':'"
;;

let split_game_text_into_rounds_text game_text =
  (* [ " 3 blue, 4 red"; "1 red, 2 green, 6 blue"; "2 green" ] *)
  match String.split game_text ~on:';' with
  | [] -> failwith "This game had zero sets"
  | sets -> List.map sets ~f:String.strip
;;

let split_round_text_game_sets game_sets_text =
  List.map game_sets_text ~f:(fun x -> String.split x ~on:',' |> List.map ~f:String.strip)
;;

let parse_color_count roll_text =
  match String.split roll_text ~on:' ' with
  | [ count; color ] -> int_of_string count, color
  | _ -> failwith "Invalid count and color"
;;

let parse_rounds_into_cube_colors (raw_games : string list list) : cube_set list =
  (* let x = List.map ~f:(List.map ~f:(fun s -> s ^ "!")) raw_games in *)
  let color_counts = List.map ~f:(List.map ~f:parse_color_count) raw_games in
  let init_count = { red = 0; green = 0; blue = 0 } in
  let cube_counts =
    List.map ~f:(fun counts -> create_cube_set counts init_count) color_counts
  in
  cube_counts
;;

let parse_game (line : string) =
  (* Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green *)
  (* [ { red = 0; green = 0; blue = 3; }; ...] *)
  line
  (* "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" *)
  |> extract_game_text
  (* [ "3 blue, 4 red"; "1 red, 2 green, 6 blue"; "2 green" ] *)
  |> split_game_text_into_rounds_text
  (* [ ["3 blue"; "4 red"]; ["1 red"; "2 green"; "6 blue"]; ["2 green"] ] *)
  |> split_round_text_game_sets
  (* [ { red = 4; green = 0; blue = 3; }; ... ] *)
  |> parse_rounds_into_cube_colors
;;

let is_game_possible (game : cube_set list) : bool =
  List.for_all
    ~f:(fun count ->
      count.red <= max_red && count.green <= max_green && count.blue <= max_blue)
    game
;;

let () =
  print_endline "";
  let lines = read_lines "data/day02/part1-full.txt" in
  let total_id, total_power =
    List.fold lines ~init:(0, 0) ~f:(fun (acc_id, acc_power) line ->
      let game_id = parse_game_id line in
      let game = parse_game line in
      let valid = is_game_possible game in
      let min_cube_set = minimum_cube_set game in
      let power = calculate_game_power min_cube_set in
      printf "==========\n";
      printf "line: %s\n" line;
      printf "game_id = %i\n" game_id;
      printf "valid = %b\n" valid;
      List.iter ~f:print_cube_set game;
      printf "min_cube_set: ";
      print_cube_set min_cube_set;
      printf "power: %i\n" power;
      printf "==========\n\n";
      let next_acc_id = if valid then acc_id + game_id else acc_id in
      next_acc_id, acc_power + power)
  in
  printf "total_id: %i\n" total_id;
  printf "total_power: %i\n\n" total_power
;;
