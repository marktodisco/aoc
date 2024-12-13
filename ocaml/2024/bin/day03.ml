open Printf

let () =
  (* let lines = [ "mul(1,2)mul[3,4]don't()mul(6,7)do()mul(100,1000)" ] in *)
  let lines = "./data/d3.txt" |> Advent.read_lines in
  let matches = lines |> List.map Advent.extract_groups |> List.flatten in
  let valid_matches = matches |> Advent.filter_mul_groups in
  let total =
    valid_matches
    |> List.map (String.split_on_char ',')
    |> List.map (fun xs -> List.map int_of_string xs)
    |> List.map Advent.product
    |> List.fold_left ( + ) 0
  in
  printf "\nmul: %i\n" total;
  printf "%s" "\n";
  (* Advent.print_list Advent.string_printer matches; *)
  (* Advent.print_list Advent.string_printer valid_matches; *)
  printf "%s" "\n"
;;
