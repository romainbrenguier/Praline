(*
 * Praline
 * Copyright 2011 LSV, CNRS & ENS de Cachan, France
 * Author: Romain Brenguier <brenguier@lsv.ens-cachan.fr>
 * File: game.mli
 * Created: Thu Jul 28 2011
 * 
 * This file is part of Praline.
 * 
 * Praline is a free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details. 
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

(** Games played on graphs. *)

(** Signature of games *)
module type S =
sig
  module A : Arena.S
  (*module PlayerSet : Set.S with type elt = A.M.Player.t*)
  (*module StateSet : Set.S with type elt = A.G.V.t*)
  
  type objective
  type t 

  val set_arena : t -> A.t -> t

  (** Build a game from an arena. *)
  val make : A.t -> t

  (** Arena of a game *)
  val arena : t -> A.t

  (** Objective of one player *)
  val objective : t -> A.M.Player.t -> objective

  (** Objective of a player in the game *)
  val set_objective : t -> A.M.Player.t -> objective -> t
   
  (** Set of players in the game *)
  val players : t -> Arena.Util(A).PlayerSet.t

  (** Game can optionaly have a starting state *)
  val start : t -> A.G.V.t option

  val set_start : t -> A.G.V.t -> t

end
  
(** Signature for Buchi games *)
module type BUCHI =
sig 
  include S
(** Objectives give a value to each state, the goal is to maximize 
    the maximum value visited infinitely often. 
    The default value is 0. *)
  val buchi_objective : objective -> (A.G.V.t -> int)
end

(** Output a Buchi game in a file *)
module Output :
  functor (G:BUCHI) ->
    functor (P:
      sig 
	val state : G.A.G.V.t -> string
	val action : G.A.M.Action.t -> string
	val player : G.A.M.Player.t -> int
      end) ->
sig

    (** [output filename g] output the arena of [g] in
	the file [filename.dot] and the objectives in the 
	file [filename.game].*)
    val output : string -> G.t -> unit
  
    (** output the arena of [g] in the file [filename] *)
    val output_arena : string -> G.t -> unit
end

(** Signature for games with objectives which are definable by Buchi automata *)
module type BUCHI_DEFINABLE =
sig 
  include S
  module Auto : Auto.P
    with type Alphabet.elt = A.G.V.t

  type buchi_objective =
    | Internal of (A.G.V.t -> int)
    | Automaton of (Auto.t * (Auto.States.elt -> int))

  val to_buchi : t -> objective -> buchi_objective

end 

(** Functor to build a concurrent buchi games *)
module MakeBuchi :
  functor (A:Arena.S) ->
sig
  include BUCHI
  val make_objective : (A.G.V.t -> int) -> objective
end
  with module A = A

(** Functor to build a concurrent game from an Arena *)
module Make :
  functor (A:Arena.S) ->
sig
  include BUCHI_DEFINABLE
  val reach : (A.G.V.t -> int) -> objective
  val buchi : (A.G.V.t -> int) -> objective
  val safety : Arena.Util(A).StateSet.t -> objective
  val automaton : Auto.t -> objective
end
  with module A = A
  and type Auto.States.elt = int
  and type Auto.Alphabet.elt = A.G.V.t


module ToBuchi :
  functor (G:BUCHI_DEFINABLE) ->
sig
  include BUCHI

  (** Create a Buchi game from a game with winning conditions 
      that are expressible as Buchi automata *)
  val to_buchi : G.t -> t

  (** Gives the states that correspond to the initial states 
      of the Buchi automata *)
  val initial : G.t -> Arena.Util(A).StateSet.t

  val from_buchi : t -> G.t

  val arena_from_buchi : A.t -> G.A.t

  module DotPrinter :
    functor (P:
      sig 
	val conf : G.A.G.V.t -> string
	val state : G.Auto.States.elt -> string
	val action : G.A.M.Action.t -> string
      end ) ->
  sig

    val string_of_state : A.G.V.t -> string

    val objectives : t -> (A.M.Player.t -> string) -> string

    (** [fprint_graph ppf g] pretty prints the graph [g] in 
	the dot language on the formatter ppf.*)
    val fprint_graph : Format.formatter -> A.G.t -> unit
      
    (** [output_graph oc g] pretty prints the graph [g] in
	the dot language on the channel oc.*)
    val output_graph : Pervasives.out_channel -> A.G.t -> unit
  end

end 
  with
    module A.M = G.A.M
(*    type A.M.Player.t = G.A.M.Player.t*)
(*  and
    module PlayerSet = G.PlayerSet *)
