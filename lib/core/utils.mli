module type S = sig
  type t
  val get_children : t -> t list
end

module Make : functor (I : S) -> sig
  val count : I.t -> int
  val find_opt : (I.t -> bool) -> I.t -> I.t option
end
