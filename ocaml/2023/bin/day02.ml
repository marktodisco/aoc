open Core
open Printf

exception No_lines of string

type cube_count =
  { red : int
  ; green : int
  ; blue : int
  }

let update_color cubes color count =
  match color with
  | "red" -> { cubes with red = count }
  | "green" -> { cubes with green = count }
  | "blue" -> { cubes with blue = count }
  | _ -> failwith "Invalid color"
;;

let rec create_cube_count (rolls : (int * string) list) init : cube_count =
  print_endline ("rolls length: " ^ string_of_int (List.length rolls));
  match rolls with
  | [] -> init
  | (count, color) :: [] -> update_color init color count
  | (count, color) :: rolls_subset ->
    create_cube_count rolls_subset (update_color init color count)
;;

let print_rbg rbg =
  Printf.printf "{ red = %i; green = %i; blue = %i; }\n" rbg.red rbg.green rbg.blue
;;

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

let parse_game_id (line : string) : int =
  (* Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green *)
  print_endline ("line = " ^ line);
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

let parse_rounds_into_cube_colors (raw_games : string list list) : cube_count list =
  (* let x = List.map ~f:(List.map ~f:(fun s -> s ^ "!")) raw_games in *)
  let color_counts = List.map ~f:(List.map ~f:parse_color_count) raw_games in
  let init_count = { red = 0; green = 0; blue = 0 } in
  let cube_counts =
    List.map ~f:(fun counts -> create_cube_count counts init_count) color_counts
  in
  cube_counts
;;

(* let mapper games = List.map ~f:(fun game -> parse_color_count) *)
(* let from_string_to_count s : string = parse_color_count s in *)
(* let games_as_strings =
    List.map
      ~f:(fun game -> List.map ~f:(fun game_set -> parse_color_count game_set) game)
      raw_games
  in
  let init_counts = { red = 0; green = 0; blue = 0 } in
  let games_as_rolls =
    List.map ~f:(fun rolls -> create_cube_count games_as_strings init_counts)
  in
  games_as_rolls *)

(* let parse_game_sets (line : string) : (int * int * int) list = *)
let parse_game (line : string) : string list =
  (* Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green *)
  (* [ { red = 0; green = 0; blue = 3; }; ...] *)
  create_cube_count [ 1, "red"; 10, "green"; -1, "blue" ] { red = 0; green = 0; blue = 0 }
  |> print_rbg;
  let game_text = extract_game_text line in
  let game_sets_text = split_game_text_into_rounds_text game_text in
  let _ = split_round_text_game_sets game_sets_text in
  let _ =
    line
    |> extract_game_text (* "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" *)
    |> split_game_text_into_rounds_text
       (* [ "3 blue, 4 red"; "1 red, 2 green, 6 blue"; "2 green" ] *)
    |> split_round_text_game_sets
    (* [ ["3 blue"; "4 red"]; ["1 red"; "2 green"; "6 blue"]; ["2 green"] ] *)
    |> parse_rounds_into_cube_colors
  in
  (* let (games : cube_count list list) =
     List.map game_sets ~f:(fun game_set -> List.map (String.split game_set ~on:','))
     in *)
  (* need to start a iteration here *)
  let game_text =
    (match List.nth game_sets_text 0 with
     | Some x -> x
     | None -> failwith "No games")
    |> String.strip
  in
  let rolls = List.map (String.split game_text ~on:',') ~f:String.strip in
  let roll =
    match List.nth rolls 0 with
    | Some x -> x
    | None -> failwith "No rolls"
  in
  let count, color = parse_color_count roll in
  let cubes : cube_count = { red = 0; green = 0; blue = 0 } in
  let cubes = update_color cubes color count in
  (*  *)
  print_endline ("game_text: " ^ game_text);
  print_endline ("roll: " ^ roll);
  print_rbg cubes;
  (*  *)
  game_sets_text
;;

let () =
  print_endline "";
  let lines = read_lines "data/day02/part1-test.txt" in
  let line =
    match List.nth lines 0 with
    | Some s -> s
    | None -> raise (No_lines "No lines found in source")
  in
  let game_id = parse_game_id line in
  (* games : (cube_color list) list *)
  let _ = parse_game line in
  printf "==========\n";
  printf "game_id = %i\n" game_id;
  printf "==========\n"
;;
