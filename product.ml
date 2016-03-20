(*
 * Praline
 * Copyright 2011 LSV, CNRS & ENS de Cachan, France
 * Author: Romain Brenguier <brenguier@lsv.ens-cachan.fr>
 * File: product.ml
 * Created: Mon Jul 11 2011
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

module Make =
  functor (A:Arena.S) -> 
    functor (D: Auto.DET with type Alphabet.elt = A.G.V.t) ->
      functor (I: Map.OrderedType) ->
struct

  module A = A
  module D = D  
  module I = I
  module Map = Map.Make(I)

  (* missing function in ocaml version < 3.12.0 *)
  let map_for_all f m = 
    Map.fold (fun i x b -> b && f i x) m true

  module State = 
  struct
    type t = A.G.V.t * D.States.elt Map.t
    let config (c,_) = c
    let states (c,l) = l
    let of_config c = (c, Map.empty)
    let product (c,x) i y = (c, Map.add i y x)      
    let proj (c,x) i = Map.find i x 
    let hash = Hashtbl.hash
    let compare a b =
      let cmpr = A.G.V.compare (config a) (config b) in
      if cmpr <> 0
      then cmpr
	(* it would be better to have a specific function
	   to compare automaton states *)
      else Map.compare compare (states a) (states b)

    let rec equal a b = 
      A.G.V.equal (config a) (config b) &&
	Map.equal (=) (states a) (states b)
  (* (c,l) (d,m) = 
      A.G.V.equal c d && 
	map_for_all (fun i x -> x = Map.find i m) l*)

  end 

  module P = Arena.Make(State)(A.M)
  module States = Set.Make(P.G.V)

  let inj conf = State.of_config conf
  let state_product conf index state = State.product conf index state

  let of_graph a =
    let g = P.G.empty in 
    let transition_of_edge e g = 
      P.G.add_edge_e g
	(State.of_config (A.G.E.src e), 
	 A.move (A.G.E.label e), 
	 State.of_config (A.G.E.dst e))
    in A.G.fold_edges_e transition_of_edge a g

  let projection a =
    let g = 
      P.G.fold_vertex 
	(fun c g -> A.G.add_vertex g (State.config c)) 
	a A.G.empty
    in
    P.G.fold_edges_e
      (fun e g -> 
	A.add_edge g
	  (State.config (P.G.E.src e))
	  (P.G.E.label e)
	  (State.config (P.G.E.dst e))
      ) a g
      
  let product arena index auto = 
    let initial = D.initial auto in
(*    let prod =
      P.G.fold_vertex 
	(fun c prod -> 
	  let new_state = State.product c index initial in
	  States.add new_state to_visit
	)
	arena (P.G.empty)
    in*)
    let rec visit prod prod_state proj visited = 
      if States.mem prod_state visited
      then prod,visited
      else 
	P.G.fold_succ_e
	  (fun e (prod,visited) ->
	    let source = P.G.E.src e in
	    let dest = P.G.E.dst e in
	    let new_source = prod_state in
	    let new_dest = 
	      State.product dest index
		(D.delta auto (State.proj new_source index) 
		   (State.config source))
	    in
	    let prod = P.G.add_edge_e prod
	      (new_source, P.G.E.label e, new_dest)
	    in 
	    let visited = States.add new_source visited in 
	    visit prod new_dest dest visited
	  ) arena proj (prod,visited)
	  
    in
	
    let (prod,visited) =
      P.G.fold_vertex 
	(fun s (prod,visited) -> 
	  let new_state = State.product s index initial in
	  visit prod new_state s visited 
	) arena (P.G.empty(*prod*),States.empty)
    in prod

  exception Stop of int

  let image g index states =
    P.G.fold_vertex
      (fun (c,l) set -> 
	let state = Map.find index l in
	if D.States.mem state states
	then States.add (c,l) set
	else set) g States.empty
	  
      
  let get state index =
    Map.find index (State.states state)

  let proj state = 
    State.config state

  module DotPrinter =
    functor (Pr:
      sig 
	val conf : A.G.V.t -> string
	val state : D.States.elt -> string
	val action : A.M.Action.t -> string
      end ) ->
  struct 

    let string_of_state v = 
      Map.fold 
	(fun i state string ->
	  string ^ Pr.state state)
	(State.states v) (Pr.conf (State.config v))

    module M = 
    struct
      type t = P.G.t
      module V = P.G.V
      module E = P.G.E
      module MUtil = Move.Util(A.M)
      let iter_vertex = P.G.iter_vertex
      let iter_edges_e = P.G.iter_edges_e
      let graph_attributes i = []
      let default_vertex_attributes i =  []
      let vertex_name v = 
	Printf.sprintf "%S" (string_of_state v)

      let vertex_attributes v = []
      let get_subgraph v = None
      let default_edge_attributes i = []
      let edge_attributes e = 
	[ `Label (MUtil.to_string Pr.action (P.move (P.G.E.label e)))]
    end
	
    module Print = Graph.Graphviz.Dot(M)
    let fprint_graph = Print.fprint_graph
    let output_graph = Print.output_graph

  end 

  
end
