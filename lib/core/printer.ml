(* adapted from mjambon's work, published in the public domain:
https://gist.github.com/mjambon/75f54d3c9f1a352b38a8eab81880a735 *)

module type S = sig
  include Utils.S
  val to_string: t -> string
end

module Make (I : S) = struct
  let rec iter f = function
    | [] -> ()
    | [x] ->
      f `Last x
    | x :: tl ->
      f `Not_last x;
      iter f tl

  let to_buffer ?(line_prefix = "") buf x =
    let rec print_root indent x =
      Printf.bprintf buf "%s\n" (I.to_string x);
      iter (print_child indent) (I.get_children x)
    and print_child indent pos x =
      let line =
        match pos with
        | `Last -> "└── "
        | `Not_last -> "├── "
      in
      Printf.bprintf buf "%s%s" indent line;
      let extra_indent =
        match pos with
        | `Last -> "    "
        | `Not_last -> "│   "
      in
      print_root (indent ^ extra_indent) x
    in
    Buffer.add_string buf line_prefix;
    print_root line_prefix x

  let to_string ?line_prefix x =
    let buf = Buffer.create 1000 in
    to_buffer ?line_prefix buf x;
    Buffer.contents buf
end

module Tests = struct
  type t =
    | Node of string * t * t
    | Leaf

  module M = struct
    type nonrec t = t

    let to_string = function
      | Leaf -> "."
      | Node (name, _, _) -> name

    let get_children = function
      | Leaf -> []
      | Node (_, a, b) -> List.filter ((<>) Leaf) [a; b]
  end

  module N = Make (M)

  let%expect_test _ =
    let shared_node =
      Node (
        "hello",
        Node ("world", Leaf, Leaf),
        Node ("you", Leaf, Leaf)
      )
    in
    let tree =
      Node (
        "root",
        Node (
          "Mr. Poopypants",
          Node (
            "something something",
            shared_node,
            Leaf
          ),
          Node (
            "Ms. Poopypants",
            Leaf,
            Leaf
          )
        ),
        shared_node
      )
    in
    print_endline @@ N.to_string ~line_prefix:"* " tree;
    [%expect {|
  * root
  * ├── Mr. Poopypants
  * │   ├── something something
  * │   │   └── hello
  * │   │       ├── world
  * │   │       └── you
  * │   └── Ms. Poopypants
  * └── hello
  *     ├── world
  *     └── you
    |}]
end
