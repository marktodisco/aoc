open Core
open Printf

let read_lines (path : string) : string list =
  let ic = In_channel.create path in
  let contents = In_channel.input_lines ic in
  In_channel.close ic;
  contents
;;

let print_list ?(prefix = "") ?(postfix = "\n") printer lst =
  printf "%s" prefix;
  printf "[";
  List.iteri
    ~f:(fun i x ->
      if i > 0 then printf "; ";
      printer x)
    lst;
  printf "]";
  printf "%s" postfix
;;

let int_printer = printf "%d"
let bool_printer = printf "%b"
let string_printer = printf "\"%s\""
let print_hashtbl h = Stdlib.Hashtbl.iter (fun k v -> printf "%i => %i\n" k v) h

let dropi items i =
  if i < 0 || i >= List.length items then failwith ("invalid index: " ^ string_of_int i);
  List.filteri ~f:(fun i' _ -> i' <> i) items
;;
