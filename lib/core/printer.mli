module type S = sig
  include Utils.S
  val to_string: t -> string
end

module Make : functor (I : S) -> sig
  val to_buffer : ?line_prefix:string -> Buffer.t -> I.t -> unit
  val to_string : ?line_prefix:string -> I.t -> string
end
