open Core

let take (puzzle : string array array) ((i, j) : int * int) : string option =
  let nr, nc = Array.length puzzle, Array.length puzzle.(0) in
  if i < nr && j < nc && i >= 0 && j >= 0 then Some puzzle.(i).(j) else None
;;

let take_many (puzzle : string array array) (ids : (int * int) list) : string =
  let rec aux ids' acc =
    match ids' with
    | [] -> acc |> List.rev |> Core.String.concat
    | (i, j) :: tail ->
      (match take puzzle (i, j) with
       | None -> aux tail acc
       | Some s -> aux tail (s :: acc))
  in
  aux ids []
;;

let east puzzle i j = take_many puzzle [ i, j; i, j + 1; i, j + 2; i, j + 3 ]
let west puzzle i j = east puzzle i j |> Core.String.rev
let south puzzle i j = take_many puzzle [ i, j; i + 1, j; i + 2, j; i + 3, j ]
let north puzzle i j = south puzzle i j |> Core.String.rev

let north_east puzzle i j =
  take_many puzzle [ i, j; i - 1, j + 1; i - 2, j + 2; i - 3, j + 3 ]
;;

let south_west puzzle i j = north_east puzzle i j |> Core.String.rev

let south_east puzzle i j =
  take_many puzzle [ i, j; i + 1, j + 1; i + 2, j + 2; i + 3, j + 3 ]
;;

let north_west puzzle i j = south_east puzzle i j |> Core.String.rev

let __ puzzle i j =
  [ north puzzle i j
  ; south puzzle i j
  ; east puzzle i j
  ; west puzzle i j
  ; north_west puzzle i j
  ; north_east puzzle i j
  ; south_west puzzle i j
  ; south_east puzzle i j
  ]
;;

let get_3x3 puzzle i j : string option =
  let nr, nc = Array.length puzzle, Array.length puzzle.(0) in
  let delta = 3 in
  if i + delta > nr || j + delta > nc
  then None
  else (
    let idxs =
      [ 0, 0; 0, 1; 0, 2; 1, 0; 1, 1; 1, 2; 2, 0; 2, 1; 2, 2 ]
      |> Core.List.map ~f:(fun (i', j') -> i + i', j + j')
    in
    Some (take_many puzzle idxs))
;;

let is_valid_block (block : string option) i j =
  let valid_cases = [ "MSAMS"; "SSAMM"; "MMASS"; "SMASM" ] in
  match block with
  | None -> false
  | Some b ->
    let x = [ b.[0]; b.[2]; b.[4]; b.[6]; b.[8] ] |> Core.String.of_char_list in
    let v =
      Core.List.fold_left valid_cases ~init:false ~f:(fun acc case ->
        acc || String.( = ) x case)
    in
    let _, _ = i, j in
    (* Printf.printf "(%i, %i): %s\n" i j (if v then x ^ " *" else x); *)
    v
;;

let count puzzle =
  let nr = Array.length puzzle in
  let nc = Array.length puzzle.(0) in
  let n = ref 0 in
  for i = 0 to nr - 1 do
    for j = 0 to nc - 1 do
      let block = get_3x3 puzzle i j in
      if is_valid_block block i j then n := !n + 1
    done
  done;
  !n
;;

let () =
  let data =
    "./data/day4.txt" |> Advent.IO.read_lines |> List.map ~f:Advent.list_of_string
  in
  let puzzle = data |> Core.List.map ~f:Array.of_list |> Array.of_list in
  (* Printf.printf "\npuzzle: %s\n" ""; *)
  (* List.iter ~f:(Advent.IO.print_list Advent.IO.string_printer ~prefix:"") data; *)
  let count = count puzzle in
  Printf.printf "\ncount: %i\n" count;
  ()
;;
