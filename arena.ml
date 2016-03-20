(*
 * Praline
 * Copyright 2011 LSV, CNRS & ENS de Cachan, France
 * Author: Romain Brenguier <brenguier@lsv.ens-cachan.fr>
 * File: arena.ml
 * Created: Wed Aug  3 2011
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

module type S =
sig
  module G : Graph.Sig.P
  module M : Move.S
  type t = G.t
  val move : G.E.label -> M.t
  val add_edge : G.t -> G.V.t -> M.t -> G.V.t -> G.t
end


module Make = 
  functor (V:Util.HASHABLE) ->
    functor (M:Move.S) ->
struct
  module G = Graph.Persistent.Digraph.ConcreteLabeled(V)(M)
  module M = M
  type t = G.t
  let move l = l

  let add_edge g a m b = 
    G.add_edge_e g
      (G.E.create a m b)
end



module MakeTB = 
  functor (V:Util.HASHABLE) ->
    functor (P:Util.HASHABLE) ->
struct
  module VP = Graph.Util.CMPProduct(V)(P)
  module G = Graph.Persistent.Digraph.Concrete(VP)
  module Player = P
  type t = G.t
  let owner v = 
    let (w,p) = G.V.label v in p
end

      

module DotPrinter =
  functor (A:S) ->
    functor (P:
      sig 
	val state : A.G.V.t -> string
	val action : A.M.Action.t -> string
      end ) ->
struct 
  module M = 
  struct
    type t = A.G.t
    module V = A.G.V
    module E = A.G.E
    module MUtil = Move.Util(A.M)
    let iter_vertex = A.G.iter_vertex
    let iter_edges_e = A.G.iter_edges_e
    let graph_attributes i = []
    let default_vertex_attributes i =  []
    let vertex_name v = P.state v
    let vertex_attributes v = []
    let get_subgraph v = None
    let default_edge_attributes i = []
    let edge_attributes e = 
      [ `Label (MUtil.to_string P.action (A.move (E.label e))) ]
  end
  module Print = Graph.Graphviz.Dot(M)
  let fprint_graph = Print.fprint_graph
  let output_graph = Print.output_graph
end 


module type TB =
sig
  module G : Graph.Sig.P
  module Player : Util.COMPARABLE
  type t = G.t
  val owner : G.V.t -> Player.t
end

module OldUtil = Util

module Util =
  functor (A:S) ->
struct
  module PlayerSet = Set.Make(A.M.Player)
  module StateSet = Set.Make(A.G.V)
  module UG = Util.Graph(A.G)
  module MUtil = Move.Util(A.M)

  let fold_move f g s =
    A.G.fold_succ_e 
      (fun e res -> f (A.move (A.G.E.label e)) res) g s

  let exists_move g s f =
    UG.exists_edge g s
      (fun e -> f (A.move (A.G.E.label e)))

  let successor arena s m =
    match 
      A.G.fold_succ_e
	(fun e res ->
	  if A.M.compare m (A.move (A.G.E.label e)) = 0
	  then Some (A.G.E.dst e)
	  else res
	) arena s None
    with None -> failwith "no corresponding edge" | Some x -> x
	
  let suspect arena a b m = 
    let players move = 
      A.M.fold 
	(fun player action set -> PlayerSet.add player set) 
	move PlayerSet.empty
    in
    let diff v w = 
      A.M.fold
	(fun player action list -> 
	  if action = A.M.get_action player w
	  then list else player :: list) 
	v []
    in
    A.G.fold_succ_e
      (fun e susp ->
	if A.G.V.equal (A.G.E.dst e) b 
	then 
	  match diff m (A.move (A.G.E.label e)) with 
	    | [] -> players m
	    | [p] -> PlayerSet.add p susp
	    | _ -> susp
	else susp)
      arena a PlayerSet.empty
      
end


(*
module SuspectArena =
  functor (A:S) ->
struct
  module U = Util(A)
  module SU = OldUtil.Set(U.PlayerSet)
  module V = 
  struct 
    type t = 
      | V1 of V.t * U.PlayerSet.t
      | V2 of V.t * U.PlayerSet.t * A.M.t
    let hash x =
    let compare x y = compare x y
    let equal x y = x = y
  end



  module Arena = Make(V)(M)
end
*)
