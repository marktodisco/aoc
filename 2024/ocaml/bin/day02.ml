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

let is_increasing diffs =
  let valid = List.filter (fun x -> x > 0) diffs in
  let num_invalid = List.length diffs - List.length valid in
  match num_invalid with
  | n when n < 0 -> failwith "Cannot be negative"
  | n -> n <= 0
;;

let is_decreasing diffs =
  let valid = List.filter (fun x -> x < 0) diffs in
  let num_invalid = List.length diffs - List.length valid in
  match num_invalid with
  | n when n < 0 -> failwith "Cannot be negative"
  | n -> n <= 0
;;

let is_gradual ?(spike = 3) diffs =
  let valid = List.filter (fun x -> x <= spike) (List.map abs diffs) in
  let num_invalid = List.length diffs - List.length valid in
  match num_invalid with
  | n when n < 0 -> failwith "Cannot be negative"
  | n -> n <= 0
;;

let xor a b = (a || b) && not (a && b)

let is_safe (report : int list) =
  let diffs = diff report in
  xor (is_increasing diffs) (is_decreasing diffs) && is_gradual diffs
;;

let all_safe report =
  let is_default_safe = is_safe report in
  let reports' = List.mapi (fun i _ -> Advent.dropi report i) report in
  let is_dropped_safe = List.map is_safe reports' in
  is_default_safe || List.exists Fun.id is_dropped_safe
;;

let () =
  let reports = "../data/d2.txt" |> Advent.IO.read_lines |> parse_reports in
  let safe = reports |> List.filter all_safe in
  let num_safe = safe |> List.length in
  print_endline "\n";
  print_endline "Part1:";
  print_endline "------";
  (* List.iter (fun r -> Advent.print_list Advent.int_printer r) reports;
     print_endline "";
     List.iter (fun r -> Advent.print_list Advent.int_printer r) safe; *)
  printf "\nSafe: %i\n" num_safe;
  print_endline ""
in
print_endline "\n"
