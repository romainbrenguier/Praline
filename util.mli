(*
 * Praline
 * Copyright 2011 LSV, CNRS & ENS de Cachan, France
 * Author: Romain Brenguier <brenguier@lsv.ens-cachan.fr>
 * File: util.mli
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

(** Some usefull modules. *)
module type SIMPLE =
sig
  type t
end

module type COMPARABLE =
sig 
  type t 
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module type HASHABLE =
sig
  include COMPARABLE
  val hash : t -> int
end

module type PARSABLE =
sig
  include HASHABLE
  val of_string : string -> t
  val to_string : t -> string
end

(*
module MakeComparable :
  functor (S:sig type t end) ->
    COMPARABLE with type t = S.t
*)

(** usefull functions on graphs *)
module Graph : 
  functor (G:Graph.Sig.P) ->
sig

  exception Invalid_vertex of G.V.t

  (** [for_all_vertex g p] checks if all elements of the graph [g]
      satisfy the predicate [p].*)
  val for_all_vertex : G.t -> (G.V.t -> bool) -> bool

  (** [for_all_succ g s p] checks if all successors of [s]
      satisfy the predicate [p].*)
  val for_all_succ : G.t -> G.V.t -> (G.V.t -> bool) -> bool
  
  (** [exists_vertex g p] checks if an elements of the graph [g]
      satisfies the predicate [p].*)
  val exists_vertex : G.t -> (G.V.t -> bool) -> bool

  (** [exists_vertex g v p] checks if an edge in the graph [g] 
      from the vertex [v] satisfies the predicate [p].*)
  val exists_edge : G.t -> G.V.t -> (G.E.t -> bool) -> bool

  (** gives the number of edges of the graph *)
  val nb_edges : G.t -> int

  (** [subgraph g l] returns the subgraph of [g], keeping only the states in the list [l] *)
  val subgraph : G.t -> G.V.t list -> G.t

  (** [is_subgraph g h] tells if the graph [g] is a subgraph of [h] *)
  val is_subgraph : G.t -> G.t -> bool
    
(** [accessible g v] constructs the subgraph of [g] that is accessible from the vertex [v] *)
  val accessible : G.t -> G.V.t -> G.t
end

module Int : PARSABLE with type t = int

module IntMap : Map.S with type key = Int.t

module String : PARSABLE with type t = string

module Unit : PARSABLE with type t = unit

module Set :
  functor (S:Set.S) ->
sig
  val to_string : (S.elt -> string) -> S.t -> string
  val to_code : (S.elt -> string) -> S.t -> string
  val to_int : S.t -> (S.elt -> string) -> S.t -> int
  val hash : S.t -> int

end

