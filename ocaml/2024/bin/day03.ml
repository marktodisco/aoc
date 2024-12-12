open Printf

let get_matches (pattern : string) (text : string) =
  let regex = Str.regexp pattern in
  let rec aux pos acc =
    try
      let start = Str.search_forward regex text pos in
      let group = Str.matched_group 1 text in
      aux (start + 1) (group :: acc)
    with
    | Not_found -> List.rev acc
  in
  aux 0 []
;;

let product nums = List.fold_left Int.mul 1 nums

let () =
  let lines = "./data/d3.txt" |> Advent.read_lines in
  (* let lines =
    [ "mul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(0,1,1)+mul(32,64]then(mul(11,8)mul(8,5))"
    ]
  in *)
  let pattern = {|mul(\([0-9]+\(,[0-9]+\)*\))|} in
  let matches = lines |> List.map (get_matches pattern) |> List.flatten in
  let numbers =
    matches
    |> List.map (String.split_on_char ',')
    |> List.map (fun xs -> List.map int_of_string xs)
    |> List.map product
    |> List.fold_left ( + ) 0
  in
  printf "\nmul: %i\n" numbers
;;
