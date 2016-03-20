(** A possible choice of action for each player. *)

module type S =
sig 
  type t 
  module Action:Util.SIMPLE
  module Player:Util.COMPARABLE
  exception Action_not_found
  val empty : t
  val default : t
  val compare : t -> t -> int
  val set_action : Player.t -> Action.t -> t -> t

  (** raises [Action_not_found] if the action is not defined *)
  val get_action : Player.t -> t -> Action.t
  val fold : (Player.t -> Action.t -> 'a -> 'a) -> t -> 'a -> 'a
end

module Make :
  functor (P:Util.COMPARABLE) ->
    functor (A:Util.COMPARABLE) ->
      S
  with
    module Player = P
  and 
    module Action = A

module Util :
  functor (M:S) ->
sig
  val to_actions : M.t -> (M.Player.t * M.Action.t) list
  val move_of_actions : (M.Player.t * M.Action.t) list -> M.t
  val to_string : (M.Action.t -> string) -> M.t -> string
end
