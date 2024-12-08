open Printf

let parse_reports lines =
  List.map (fun line -> String.split_on_char ' ' line |> List.map int_of_string) lines
;;

let diff (nums : int list) =
  let rec inner (acc : int list) (xs : int list) =
    match xs with
    | [ _ ] | [] -> acc
    | x1 :: x2 :: tail -> inner ((x2 - x1) :: acc) (x2 :: tail)
  in
  inner [] nums
;;

let is_increasing diffs = List.for_all (fun x -> x > 0) diffs
let is_decreasing diffs = List.for_all (fun x -> x < 0) diffs
let is_gradual diffs = diffs |> List.map abs |> List.for_all (fun x -> x <= 3)
let xor a b = (a || b) && not (a && b)

let is_safe (report : int list) =
  let diffs = diff report in
  xor (is_increasing diffs) (is_decreasing diffs) && is_gradual diffs
;;

let () =
  let reports = "./data/d2.txt" |> Advent.read_lines |> parse_reports in
  let num_safe = reports |> List.filter is_safe |> List.length in
  print_endline "\n";
  print_endline "Part1:";
  print_endline "------";
  (* List.iter (Advent.print_list Advent.int_printer) reports; *)
  printf "Safe: %i\n" num_safe;
  print_endline ""
;;
