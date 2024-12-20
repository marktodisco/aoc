open Core
open Printf

let pair_from_line (pair : string) : int * int =
  let x =
    pair |> String.substr_replace_all ~pattern:"   " ~with_:" " |> String.split ~on:' '
  in
  match x with
  | [ x; y ] -> int_of_string x, int_of_string y
  | _ -> failwith "Bad"
;;

let pairs_to_lists pairs =
  let rec inner pairs left right =
    match pairs with
    | [] -> left, right
    | (x, y) :: tail -> inner tail (x :: left) (y :: right)
  in
  let l, r = inner pairs [] [] in
  List.sort ~compare l, List.sort r ~compare
;;

let zip (left, right) =
  let rec inner left right zipped =
    match left, right with
    | _, [] | [], _ -> zipped
    | left_head :: left_tail, right_head :: right_tail ->
      inner left_tail right_tail ((left_head, right_head) :: zipped)
  in
  inner left right []
;;

let calculate_distance (x, y) = abs (x - y)

let solve_part1 =
  let distance =
    (* "../data/d1-test.txt" *)
    "../data/d1.txt"
    |> Advent.IO.read_lines
    |> List.map ~f:pair_from_line
    |> pairs_to_lists
    |> zip
    |> List.map ~f:calculate_distance
    |> List.fold ~init:0 ~f:( + )
  in
  print_endline "\n";
  print_endline "Part 1:";
  print_endline "-------";
  printf "distance: %i\n" distance;
  print_endline ""
;;

let count_items left right =
  let h = Stdlib.Hashtbl.create 1000 in
  (* initialize *)
  List.iter left ~f:(fun l ->
    match Stdlib.Hashtbl.find_opt h l with
    | Some _ -> ()
    | None -> Stdlib.Hashtbl.add h l 0);
  (* count *)
  List.iter right ~f:(fun r ->
    match Stdlib.Hashtbl.find_opt h r with
    | None -> ()
    | Some c -> Stdlib.Hashtbl.replace h r (c + 1));
  h
;;

let calculate_similarity (left : int list) count =
  List.fold left ~init:0 ~f:(fun acc k ->
    match Stdlib.Hashtbl.find_opt count k with
    | None -> acc
    | Some v -> acc + (k * v))
;;

let solve_part2 =
  let left, right =
    (* "../data/d1-test.txt" *)
    "../data/d1.txt"
    |> Advent.IO.read_lines
    |> List.map ~f:pair_from_line
    |> pairs_to_lists
  in
  let count = count_items left right in
  let similarity = calculate_similarity left count in
  print_endline "Part 2:";
  print_endline "-------";
  printf "similarity: %i\n" similarity;
  print_endline "\n"
;;

let () =
  solve_part1;
  solve_part2
;;
