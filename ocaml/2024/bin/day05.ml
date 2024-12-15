let () =
  print_endline "";
  let lines = "./data/day5-test.txt" |> Advent.IO.read_lines in
  let rules, updates = lines |> Advent.Day05.read_parts in
  let graph = Advent.Day05.build_graph rules in
  let node = Hashtbl.find graph 47 in
  Advent.Day05.print_node node;
  Advent.IO.print_list
    (fun (x, y) -> Printf.printf "(%i, %i)" x y)
    ~prefix:"rules: "
    rules;
  let _ = updates in
  (* List.iter (Advent.IO.print_list Advent.IO.int_printer ~prefix:"updates: ") updates; *)
  print_endline ""
;;
