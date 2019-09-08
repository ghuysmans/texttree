let (<|>) a b =
  match a with
  | None -> b ()
  | Some _ -> a

module type S = sig
  type t
  val get_children: t -> t list
end

module Make (I : S) = struct
  let rec count t =
    match I.get_children t with
    | [] -> 1
    | children ->
      (* sum *) List.fold_left (+) 1 (* counts *) (List.map count children)

  let rec find_opt f node =
    if f node then
      Some node
    else
      I.get_children node |> List.fold_left (fun first n ->
        first <|> fun () -> find_opt f n
      ) None
end
