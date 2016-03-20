(*
 * Praline
 * Copyright 2011 LSV, CNRS & ENS de Cachan, France
 * Author: Romain Brenguier <brenguier@lsv.ens-cachan.fr>
 * File: auto.mli
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

(** Deterministic automata. *)


(** Generic signature for deterministic automata *)
module type DET =
sig 
  module States : Set.S
  module Alphabet : Set.S

  (** raised when the transition function is not total *)
  exception UndefinedTransition of (States.elt * Alphabet.elt)

  (** raised when the initial state is not defined *)
  exception NoInitialState

  type t
  val states : t -> States.t
  val alphabet : t -> Alphabet.t
  val initial : t -> States.elt
  val final : t -> States.t
  val delta : t -> States.elt -> Alphabet.elt -> States.elt
end

(** Persistent implementations *)
module type P =
sig
  include DET
  val empty : t
  val add_state : t -> States.elt -> t
  val set_initial : t -> States.elt -> t
  val add_final : t -> States.elt -> t
  val add_transition : t -> States.elt -> Alphabet.t -> States.elt -> t
  val add_to_transition : t -> States.elt -> Alphabet.elt -> States.elt -> t
end

(** Imperative implementations *)
module type I =
sig
  include DET
  val clear : t -> unit
  val add_state : t -> States.elt -> unit
  val set_initial : t -> States.elt -> unit
  val add_final : t -> States.elt -> unit
  val add_transition : t -> States.elt -> Alphabet.t -> States.elt -> unit
end

(** Build automata from a module for states 
    and one for the alphabet *)
module Make :
  functor (S:Util.HASHABLE) ->
    functor (A:Set.S) -> 
      P
  with 
    type States.elt = S.t
  and
    module Alphabet = A
    
(** Provide printers for the dot file format *)
module Dot : 
  functor (A:P) ->
    functor 
      (P: sig val state : A.States.elt -> string
	      val label : A.Alphabet.elt -> string
	      val state_of_string : string -> A.States.elt
	      val label_of_string : string -> A.Alphabet.elt

      end) ->
sig

  module G : Graph.Sig.P

  (** build a graph from an automaton *)
  val build : A.t -> G.t 

  val of_graph : G.t -> A.t

  val fprint_graph : Format.formatter -> A.t -> unit

  val output_graph : Pervasives.out_channel -> A.t -> unit

  val parse_graph : string -> G.t 

  val parse : string -> A.t 
end
