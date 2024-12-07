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

let () =
  let distance =
    (* "./data/d1p1-test.txt" *)
    "./data/d1p1.txt"
    |> Advent.read_lines
    |> List.map ~f:pair_from_line
    |> pairs_to_lists
    |> zip
    |> List.map ~f:calculate_distance
    |> List.fold ~init:0 ~f:( + )
  in
  print_endline "\n";
  printf "distance: %i\n" distance;
  print_endline "\n"
;;
