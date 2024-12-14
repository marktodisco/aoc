open Printf

module IO = struct
  let read_lines (path : string) : string list =
    let ic = Core.In_channel.create path in
    let contents = Core.In_channel.input_lines ic in
    In_channel.close ic;
    contents
  ;;

  let print_list ?(prefix = "") ?(postfix = "\n") printer lst =
    printf "%s" prefix;
    printf "[";
    Core.List.iteri
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
  let char_printer = printf "'%c'"
  let print_hashtbl h = Stdlib.Hashtbl.iter (fun k v -> printf "%i => %i\n" k v) h
end

let dropi items i =
  if i < 0 || i >= List.length items then failwith ("invalid index: " ^ string_of_int i);
  Core.List.filteri ~f:(fun i' _ -> i' <> i) items
;;

let list_of_string s =
  List.init (String.length s) (String.get s) |> List.map (String.make 1)
;;

let int_of_bool b = if b then 1 else 0

module Regex = struct
  open Stdlib

  let extract_groups text =
    let mul_pattern =
      Re.seq
        [ Re.str "mul("
        ; Re.group (Re.seq [ Re.rep1 Re.digit; Re.str ","; Re.rep1 Re.digit ])
        ; Re.str ")"
        ]
    in
    let do_pattern = Re.group (Re.str "do()") in
    let dont_pattern = Re.group (Re.str "don't()") in
    let regex = Re.compile (Re.alt [ mul_pattern; do_pattern; dont_pattern ]) in
    let matches = Re.all regex text in
    List.map
      (fun g ->
        Re.Group.all g |> Array.to_list |> List.filteri (fun i s -> i > 0 && s <> ""))
      matches
    |> List.flatten
  ;;

  let filter_mul_groups groups =
    let rec aux enabled groups' acc =
      match groups' with
      | "do()" :: tail -> aux true tail acc
      | "don't()" :: tail -> aux false tail acc
      | g :: tail -> aux enabled tail (if enabled then g :: acc else acc)
      | [] -> acc
    in
    List.rev (aux true groups [])
  ;;
end

module Math = struct
  open Stdlib

  let product nums = List.fold_left Int.mul 1 nums
end
