(*
 * Praline
 * Copyright 2011 LSV, CNRS & ENS de Cachan, France
 * Author: Romain Brenguier <brenguier@lsv.ens-cachan.fr>
 * File: arena.mli
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

(** Special kind of graphs, on which we can play games. *)

(** Signature for arenas. *)
module type S =
sig
  module G : Graph.Sig.P
  module M : Move.S
  type t = G.t

  (** gives the move associated to the label of an edge *)
  val move : G.E.label -> M.t

  (** add an edge to the arena *)
  val add_edge : t -> G.V.t -> M.t -> G.V.t -> t

end

(** Build a module for arenas from a module for vertices 
    and a module for moves. *)
module Make :
  functor (V:Util.HASHABLE) ->
    functor (M:Move.S) ->
      S 
  with
    module M = M 
  and 
    type G.V.t = V.t
  and 
    type G.V.label = V.t
  and
    type G.E.t = V.t * M.t * V.t
  and
    type G.E.label = M.t





(** Signature for turn-based arenas. *)
module type TB =
sig
  module G : Graph.Sig.P
  module Player : Util.COMPARABLE
  type t = G.t

  (** owner of a vertex *)
  val owner : G.V.t -> Player.t

end


(** Build a module for turn-based arenas. *)
module MakeTB :
  functor (V:Util.HASHABLE) ->
    functor (P:Util.HASHABLE) ->
      TB
  with
    module Player = P
  and 
    type G.V.t = V.t * P.t
  and 
    type G.V.label = V.t * P.t
  and
    type G.E.t = (V.t * P.t) * (V.t * P.t)

module DotPrinter :
  functor (A:S) ->
    functor (P:
      sig 
	val state : A.G.V.t -> string
	val action : A.M.Action.t -> string
      end ) ->
sig
    (** fprint_graph ppf graph pretty prints the graph graph in the CGL language on the formatter ppf.*)
  val fprint_graph : Format.formatter -> A.G.t -> unit
    
    (** output_graph oc graph pretty prints the graph graph in the dot language on the channel oc.*)
  val output_graph : Pervasives.out_channel -> A.G.t -> unit
    
end


module Util : 
  functor (A:S) ->
sig
  module PlayerSet : Set.S with type elt = A.M.Player.t

  module StateSet : Set.S with type elt = A.G.V.t


  val fold_move : (A.M.t -> 'a -> 'a) -> A.t -> A.G.V.t -> 'a -> 'a
  val exists_move : A.t -> A.G.V.t -> (A.M.t -> bool) -> bool

  (** successor of a state by a given move *)
  val successor : A.t -> A.G.V.t -> A.M.t -> A.G.V.t

  (** [suspect g a b m] computes the set of suspects in the game [g],
      for a transition [(a -> b)] and a move [m]. *)   
  val suspect : 
    A.t -> A.G.V.t -> A.G.V.t -> A.M.t -> PlayerSet.t
 
end

(*
module SuspectArena :
  functor (A:S) ->
sig
  module Arena : S
  val make : A.t -> Arena.t
end
*)
