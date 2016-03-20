module type S =
  sig
    type t
    module Player : Util.COMPARABLE
    val default : t
    val compare : t -> t -> int
    val hash : t -> int
    val equal : t -> t -> bool
    val set : Player.t -> int -> t -> t
    val get : Player.t -> t -> int
    val to_list : t -> (Player.t * int) list
    val of_list : (Player.t * int) list -> t
  end

module Make :
  functor (P : Util.COMPARABLE) ->
    S
  with module Player = P
