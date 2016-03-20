(*
 * Praline
 * Copyright 2011 LSV, CNRS & ENS de Cachan, France
 * Author: Romain Brenguier <brenguier@lsv.ens-cachan.fr>
 * File: strategy.mli
 * Created: Wed Sep 21 2011
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

(** A strategy profile is a function that given a memory state and the current 
    configuration of the game, gives a move and a new memory state. *)

module Make :
  functor (G : Game.BUCHI) -> 
sig 
  type t
  module Memory : 
    sig 
      type t 
      val initial : t
      
      exception AllSuspects
      (** raises AllSuspects if all the players are suspect *)
      val suspects : t -> G.A.M.Player.t list
    end

  (** Given the memory, tells the move to perform and the new state of the memory *)
  val move : t -> Memory.t -> G.A.M.t * Memory.t

  (** Given the memory and the current configuration of the game, tells the new memory state *)
  val update : t -> Memory.t -> G.A.G.V.t -> Memory.t

  (** the empty strategy *)
  val empty : unit -> t

  (** construct a strategy that loop through all the states of the subgame*)
  val loop : t -> G.A.G.V.t list -> G.A.G.t -> t

  (** [path strategy scc arena] construct the strategy that follow a path in [arena] to go
      to the strongly connected conponnent [scc] *)
  val path : t -> G.A.G.t -> G.A.G.t -> t

  (** [path strategy scc arena initial_state] computes the updates from the initial memory state *)
  val initial_moves : t -> G.A.G.t -> G.A.G.V.t list -> G.A.G.t -> G.A.G.vertex option -> t
    
  (** construct the strategy from the moves that are witnesses of 
      the belonging of states inside the repellor *)
  val move_from_repellor : t -> (G.A.M.Player.t list * int * Payoff.Make(G.A.M.Player).t,G.A.t) Hashtbl.t -> Payoff.Make(G.A.M.Player).t -> t

  (** computes the updates we should do, once we now enough about the strategy *)
  val updates_from_moves : G.t -> Payoff.Make(G.A.M.Player).t -> t -> G.A.G.t -> G.A.G.V.t list -> t

  val from_shape : G.t -> Repellor.Make(G).repellors -> Repellor.Make(G).shape -> t

  (** output the strategy in the dot file format *)
  val to_string : t ->
    (G.A.G.V.t -> string) ->
    (G.A.M.Player.t -> string) -> 
    (G.A.M.t -> string) -> string

  (** output the strategy as pseudocode *)
  val to_code : t ->
    (G.A.G.V.t -> (string * int) list) ->
    (G.A.M.Player.t -> string) -> 
    (G.A.M.Action.t -> int) -> string

  exception NoMove of G.A.G.V.t
  val play : G.t -> t ->
    (G.A.G.V.t -> string) ->
    (G.A.M.Player.t -> string) -> 
    (G.A.M.t -> string) -> 
    (G.A.G.V.t -> G.A.M.t -> Generator.Instruction.t -> G.A.G.V.t) ->
    (G.A.M.Player.t -> G.A.G.V.t -> int) -> 
    unit

end

 
