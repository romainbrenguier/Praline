(*
 * PRALINE
 * Copyright (C) 2011
 * LSV, CNRS & ENS de Cachan, France
 * Author: brenguier (Romain Brenguier <brenguier@lsv.ens-cachan.fr>)
 * File: timed.mli
 * Created: Fri Aug 19 2011
 * 
 * This software is free software; you can redistribute it and/or       
 * modify it under the terms of the GNU Library General Public          
 * License version 2.1.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 *)

module Clock :
sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module ClockSet : Set.S with type elt = Clock.t

module Constraint :
sig 
  type t 
  val equal : Clock.t -> int -> t 
  val smaller : Clock.t -> int -> t 
  val smaller_eq : Clock.t -> int -> t 
  val greater : Clock.t -> int -> t 
  val greater_eq : Clock.t -> int -> t 
  val compare : t -> t -> int
  val to_string : t -> string
end

module Region : 
sig

  type t

  val initial : ClockSet.t -> int -> t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  val next : t -> t
  val resets : t -> ClockSet.t -> t

  val integer_value : t -> Clock.t -> int
  val is_integer : t -> Clock.t -> bool
  val compare_fract : t -> Clock.t -> Clock.t -> int

  val satisfies_constraints : Constraint.t list -> t -> bool

  val fprint : out_channel -> t -> unit

end

module Game :
  functor (S:Graph.Sig.COMPARABLE) ->
    functor (Player:Graph.Sig.COMPARABLE) ->
sig
  module Label :
  sig 
    type t 

    val player : t -> Player.t
    val constr : t -> Constraint.t list
    val reset : t -> ClockSet.t

    val set_player : t -> Player.t -> t
    val set_constr : t -> Constraint.t list -> t
    val set_reset : t -> ClockSet.t -> t

    val make : player:Player.t -> constr:Constraint.t list
      -> reset:ClockSet.t -> t

    val compare : t -> t -> int
  end 

  module G : Graph.Sig.P 
    with 
      type V.t = S.t
    and
      type E.label = Label.t

  module RArena : Arena.S

  val region_game : G.t -> RArena.t
end
 
