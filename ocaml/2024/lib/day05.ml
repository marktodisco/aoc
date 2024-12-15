let read_parts lines =
  let rec aux is_rules lines' rules updates =
    match lines' with
    | [] -> rules, updates
    | "" :: tail -> aux false tail rules updates
    | h :: tail ->
      let next_rules = if is_rules then h :: rules else rules in
      let next_updates = if not is_rules then h :: updates else updates in
      aux is_rules tail next_rules next_updates
  in
  let rules, updates = aux true lines [] [] in
  ( rules
    |> List.map (fun r ->
      String.split_on_char '|' r
      |> List.map int_of_string
      |> function
      | [ x; y ] -> x, y
      | _ -> failwith "Expected 2-tuple")
    |> List.rev
  , updates
    |> List.map (fun r -> String.split_on_char ',' r |> List.map int_of_string)
    |> List.rev )
;;

type node =
  { before : int list
  ; after : int list
  }

let default_node = { before = []; after = [] }

let string_node node =
  "{ before = "
  ^ "[ "
  ^ (node.before |> List.map string_of_int |> String.concat "; ")
  ^ " ]; "
  ^ "after = [ "
  ^ (node.after |> List.map string_of_int |> String.concat "; ")
  ^ " ]"
  ^ " }"
;;

let print_node node = print_endline (string_node node)

let update (graph : (int, node) Hashtbl.t) (rules : (int * int) list) =
  let rec update' rules' =
    match rules' with
    | [] -> ()
    | (before, after) :: tail ->
      let before_node =
        match Hashtbl.find graph before with
        | exception Not_found ->
          Printf.printf "Not_found before: %i\n" before;
          { default_node with after = [ after ] }
        | node -> { node with after = after :: node.after }
      in
      let after_node =
        match Hashtbl.find graph after with
        | exception Not_found ->
          Printf.printf "Not_found after: %i\n" after;
          { default_node with before = [ before ] }
        | node -> { node with before = before :: node.before }
      in
      Hashtbl.replace graph before before_node;
      Hashtbl.replace graph after after_node;
      update' tail
  in
  update' rules
;;

let build_graph rules =
  let graph : (int, node) Hashtbl.t = Hashtbl.create 14000 in
  update graph rules;
  Hashtbl.iter (fun k v -> (Printf.printf "%i => %s\n") k (string_node v)) graph;
  graph
;;
