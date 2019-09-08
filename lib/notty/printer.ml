let rec pow f init = function
  | 0 -> init
  | k -> pow f (f init) (k - 1)

let rec map f = function
  | [x] -> [f `Last x]
  | h :: t -> f `Not_last h :: map f t
  | [] -> []


module type S = sig
  include Texttree.Utils.S
  val to_image: t -> Notty.image
end

module Make (T : S) = struct
  module U = Texttree.Utils.Make (T)

  let rec to_image t =
    let ghost = Notty.A.(fg (gray 5)) in
    match T.get_children t with
    | [] -> T.to_image t
    | children -> Notty.I.(T.to_image t
    <-> vcat (map (fun pos n ->
      let parent =
        let any  = "├── " in
        let last = "└── " in
        string ghost
          (match pos with
          | `Last when T.get_children n =  [] -> last
          | _ -> any)
      in
      let left =
        let children = string ghost "│   " in
        pow (fun acc -> acc <-> children) parent (U.count n - 1)
      in
      left <|> to_image n
    ) children))
end
