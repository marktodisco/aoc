open Core

let read_lines (path : string) : string list =
  let ic = In_channel.create path in
  let contents = In_channel.input_lines ic in
  In_channel.close ic;
  contents
;;

let print_list (items : string list) =
  print_string "[ ";
  List.iter items ~f:(fun s -> print_string ("\"" ^ s ^ "\"" ^ " ; "));
  print_string " ]"
;;

let print_hashtbl h = Stdlib.Hashtbl.iter (fun k v -> printf "%i => %i\n" k v) h
