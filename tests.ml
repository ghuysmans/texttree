(* FIXME make this a real test *)
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

module N = Tree.To_string.Make (M)

let test () =
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
  let result = N.to_string ~line_prefix:"* " tree in
  let expected_result = "\
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
"
  in
  print_string result;
  flush stdout;
  assert (result = expected_result)

let tests = [
  "to_string", test;
]
