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

(* let parse_game_sets (line : string) : (int * int * int) list = *)
let parse_game (line : string) : string list =
  (* Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green *)
  let game_text =
    match String.split line ~on:':' with
    | [ _; right ] -> right
    | _ -> failwith "Could not split line on ':'"
  in
  print_endline ("game_text: " ^ game_text);
  let game_sets =
    match String.split game_text ~on:';' with
    | [] -> failwith "This game had zero sets"
    | sets -> sets
  in
  printf "text = %s\n" game_text;
  let game_text =
    (match List.nth game_sets 0 with
     | Some x -> x
     | None -> failwith "No games")
    |> String.strip
  in
  let rolls = List.map (String.split game_text ~on:',') ~f:String.strip in
  List.iter rolls ~f:print_endline;
  let roll =
    match List.nth rolls 0 with
    | Some x -> x
    | None -> failwith "No rolls"
  in
  print_endline ("roll: " ^ roll);
  let count, color =
    match String.split roll ~on:' ' with
    | [ count; color ] -> int_of_string count, color
    | _ -> failwith "Invalid count and color"
  in
  let cubes : cube_count = { red = 0; green = 0; blue = 0 } in
  print_rbg cubes;
  let cubes = update_color cubes color count in
  print_rbg cubes;
  printf "%s: " color;
  printf "%i\n" count;
  (* let games = List.map game_sets ~f:(fun x -> String.split x ~on:',') in *)
  game_sets
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
  let _ = parse_game line in
  printf "==========\n";
  printf "game_id = %i\n" game_id;
  printf "==========\n"
;;
