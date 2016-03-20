(*
 * Praline
 * Copyright 2011 LSV, CNRS & ENS de Cachan, France
 * Author: Romain Brenguier <brenguier@lsv.ens-cachan.fr>
 * File: gml.mli
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

(** Parser and printers for arenas given in the 
    gml and dot file formats *)

module Point : 
sig 
  type t 
  val x : t -> int
  val y : t -> int
  val set_x : t -> int -> t
  val set_y : t -> int -> t
  val make : x:int -> y:int -> t

  val print : t -> (string * Graph.Gml.value)
  val parse : (string * Graph.Gml.value) -> t
end

module type N =
sig
  type t
  val empty : unit -> t
  val id : t -> string
  val label : t -> string
  val x : t -> int
  val y : t -> int
  val w : t -> int
  val h : t -> int
  val fill : t -> string
  val _type : t -> string
  val outline : t -> string
  val compare : t -> t -> int 
  val hash : t -> int
  val equal : t -> t -> bool 
  val of_id : string -> t
  val print : t -> Graph.Gml.value_list
  val parse : Graph.Gml.value_list -> t
end

module Node :
sig
  include N
  val set_id : t -> string -> t
  val set_x : t -> int -> t
  val set_y : t -> int -> t
  val set_w : t -> int -> t
  val set_h : t -> int -> t
  val set_label : t -> string -> t
  val set_fill : t -> string -> t
  val set_type : t -> string -> t
  val set_outline : t -> string -> t
  val make : ?id:string -> ?label:string -> ?x:int -> ?y:int -> ?w:int -> ?h:int -> ?fill:string -> ?_type:string -> ?outline:string -> unit -> t
end

(** Imperative version of nodes *)
module INode :
sig
  include N
  val set_id : t -> string -> unit
  val set_x : t -> int -> unit
  val set_y : t -> int -> unit
  val set_w : t -> int -> unit
  val set_h : t -> int -> unit
  val set_label : t -> string -> unit 
  val set_fill : t -> string -> unit
  val set_type : t -> string -> unit
  val set_outline : t -> string -> unit
  val make : ?id:string -> ?label:string -> ?x:int -> ?y:int -> ?w:int -> ?h:int -> ?fill:string -> ?_type:string -> ?outline:string -> unit -> t
end

module type E =
sig
  type t 
  type node 
  val default : t
  val compare : t -> t -> int

  val source : t -> node
  val target : t -> node
  val label : t -> string
  val fill : t -> string
  val line : t -> Point.t list

  val set_source : t -> node -> t
  val set_target : t -> node -> t
  val set_label : t -> string -> t
  val set_fill : t -> string -> t
  val set_line : t -> Point.t list -> t

  val make : source:node -> target:node -> ?label:string -> ?fill:string ->  ?line:(Point.t list) -> unit -> t

  val print : t -> Graph.Gml.value_list
  val parse : Graph.Gml.value_list -> t
end

module Edge : E with type node = Node.t
module IEdge : E with type node = INode.t

(** graphs containing all the usefull GML information *)
module Persistent :
sig 
  module G : Graph.Sig.P
  val node : G.V.t -> Node.t
  val edge : G.E.t -> Edge.t
    
  (** print the graph in the gml file format *)
  val print : Format.formatter -> G.t -> unit
    
  (** parse a graph in the gml file format *)
  val parse : string -> G.t

  (** parse a graph in the dot file format *)
  val parse_dot : string -> G.t

end

module Imperative :
sig 
  module G : Graph.Sig.I
  val node : G.V.t -> INode.t
  val edge : G.E.t -> IEdge.t

  val of_node : INode.t -> G.V.t
  val of_edge : IEdge.t -> G.E.t
    
  (** print the graph in the gml file format *)
  val print : Format.formatter -> G.t -> unit
    
  (** parse a graph in the gml file format *)
  val parse : string -> G.t

  (** parse a graph in the dot file format *)
  val parse_dot : string -> G.t

  (** copy the first graph in the second one *)
  val copy : G.t -> G.t -> unit
end

module GmlArena :
sig 
  module A : Arena.S
    with 
      type M.Player.t = int
    and 
      type M.Action.t = string
    and 
      type G.V.t = Node.t
    and 
      type G.V.label = Node.t

  (** print the arena in the gml file format *)
  val print : Format.formatter -> A.G.t -> unit

  (** parse an arena in the gml file format *)
  val parse : string -> A.G.t

  (** parse an arena in the dot file format *)
  val parse_dot : string -> A.G.t
end

module GmlGame : 
sig
  include Game.BUCHI_DEFINABLE
  val reach : (A.G.V.t -> int) -> objective
  val buchi : (A.G.V.t -> int) -> objective
  val safety : Arena.Util(A).StateSet.t -> objective
  val automaton : Auto.t -> objective
end
  with       
    module A = GmlArena.A
  and type Auto.States.elt = int
  and type Auto.Alphabet.elt = GmlArena.A.G.V.t



