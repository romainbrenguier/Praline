(*
 * Praline
 * Copyright 2011 LSV, CNRS & ENS de Cachan, France
 * Author: Romain Brenguier <brenguier@lsv.ens-cachan.fr>
 * File: generator.mli
 * Created: Wed Aug 24 2011
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


module Player : Util.PARSABLE
module Move : Move.S with type Player.t = Player.t and type Action.t = int
module StringMap : Map.S with type key = string

module Expression : 
sig

  type value = 
    | Int of int
    | Float of float
    | Action of Move.Player.t
    | Var of string
	
  type t =
    | Val of value
    | Apply of string * t list

  val evaluate : int StringMap.t -> Move.t -> t -> int

  val evaluate_float : int StringMap.t -> Move.t -> t -> float

end

module Instruction :
sig

  type t =
    | While of Expression.t * t
    | If of Expression.t * t * t
    | Set of string * Expression.t
    | Seq of t list
    | Legal of string * Expression.t list

  val evaluate : int StringMap.t -> Move.t -> t -> int StringMap.t

  val legal : int StringMap.t -> t -> (int list) StringMap.t

end
    
module Generic : 
sig
  module G : Game.BUCHI with type A.G.V.t = int StringMap.t
			and module A.M = Move

  type state = int StringMap.t

  type t = 
      {
	players : Move.Player.t list;
	payoff : Move.Player.t -> state -> int;
	init : state;
	move : state -> Move.Player.t -> Move.Action.t list;
	tab : state -> Move.t -> state
      }

  (** Given the list of players, payoff function, initial state, construct the rest of the game. *)
  val game : t -> G.t

  module Print :
  sig
    val fprint_graph : Format.formatter -> G.A.G.t -> unit
    val output_graph : Pervasives.out_channel -> G.A.G.t -> unit
  end

  module Output :
  sig
    (** [output filename g] output the arena of [g] in
	the file [filename.dot] and the objectives in the 
	file [filename.game].*)
    val output : string -> G.t -> unit
  
    (** output the arena of [g] in the file [filename] *)
    val output_arena : string -> G.t -> unit
  end
end

module Parser :
sig
  val state_of_string : string -> Generic.state
  val string_of_state : Generic.state -> string
  val assoc_of_state : Generic.state -> (string * int) list
end

 
