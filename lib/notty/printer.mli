module type S = sig
  include Texttree.Utils.S
  val to_image: t -> Notty.image
end

module Make : functor (I : S) -> sig
  val to_image : I.t -> Notty.image
end
