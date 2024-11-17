open Core
open Printf

exception No_lines of string
exception RuntimeError of string

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
  let game =
    match String.split line ~on:':' with
    | [ _; right ] -> right
    | _ -> raise (RuntimeError "Could not split line on ':'")
  in
  let game_sets =
    match String.split game ~on:';' with
    | [] -> raise (RuntimeError "This game had zero sets")
    | sets -> sets
  in
  printf "text = %s\n" game;
  let game =
    match List.nth game_sets 0 with
    | Some x -> x
    | None -> failwith "No games"
  in
  print_endline ("game: " ^ game);
  let games = List.map game_sets ~f:(fun x -> String.split x ~on:',') in
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
  let games = parse_game line in
  List.iter games ~f:(fun x -> printf "  %s\n" x);
  printf "==========\n";
  printf "game_id = %i\n" game_id;
  printf "==========\n"
;;
