(*
 * Praline
 * Copyright 2011 LSV, CNRS & ENS de Cachan, France
 * Author: Romain Brenguier <brenguier@lsv.ens-cachan.fr>
 * File: auto.ml
 * Created: Tue Jul 12 2011
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


module type DET =
sig 
  module States : Set.S
  module Alphabet : Set.S
  
  exception UndefinedTransition of (States.elt * Alphabet.elt)
  exception NoInitialState

  type t
  val states : t -> States.t
  val alphabet : t -> Alphabet.t
  val initial : t -> States.elt
  val final : t -> States.t
  val delta : t -> States.elt -> Alphabet.elt -> States.elt
end

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

module type I =
sig
  include DET
  val clear : t -> unit
  val add_state : t -> States.elt -> unit
  val set_initial : t -> States.elt -> unit
  val add_final : t -> States.elt -> unit
  val add_transition : t -> States.elt -> Alphabet.t -> States.elt -> unit
end


module Make =
  functor (S:Util.HASHABLE) ->
    functor (A:Set.S) ->
struct
  module States = Set.Make(S)
  module Label = 
    (struct open A 
    type t = A.t
    let compare = compare 
    let default = empty end)

  module Alphabet = A

  (* transitions are labeled by set of letters *)
  module G = Graph.Persistent.Digraph.ConcreteLabeled(S)(Label)

  exception UndefinedTransition of (States.elt * Alphabet.elt)
  exception NoInitialState

  type t = 
      { 
	graph : G.t; 
	initial : States.elt option; 
	final : States.t}

  let graph a = a.graph 

  let states a = 
    G.fold_vertex 
      (fun v s -> States.add v s) 
      a.graph States.empty

  let initial a = match a.initial with
    | Some x -> x
    | None -> raise NoInitialState

  (* since the automaton is deterministic we only have to look at the initial state *)
  let alphabet auto =
    G.fold_succ_e
      (fun e alpha ->
	A.union (G.E.label e) alpha) 
      auto.graph  (initial auto) A.empty
	
  let final a = a.final
  let delta auto state label =
    let edges = G.succ_e auto.graph state in 
    match List.filter (fun (s,l,t) -> A.mem label l) edges
    with
      | (_,_,t) :: [] -> t
      | _ -> raise (UndefinedTransition (state,label))

  let empty = {graph = G.empty; initial = None; final = States.empty}
  let set_initial a s = {a with initial = Some s}
  let add_final a s = {a with final = States.add s (final a)}
  let add_state a s = {a with graph = G.add_vertex a.graph s}
  let add_transition a s x t = 
    {a with graph = G.add_edge_e a.graph (s,x,t)}

  let add_to_transition a s x t = 
    try 
      let g = graph a in
      let e = G.find_edge g s t in
      let g = G.remove_edge_e g e in
      let new_e = G.E.create s (Alphabet.add x (G.E.label e)) t in
      let g = G.add_edge_e g new_e in 
      {a with graph = g}
    with Not_found -> add_transition a s (Alphabet.singleton x) t


end




module Dot = 
  functor (A:P) ->
    functor 
      (P: sig val state : A.States.elt -> string
	      val label : A.Alphabet.elt -> string
	      val state_of_string : string -> A.States.elt
	      val label_of_string : string -> A.Alphabet.elt
      end) ->
struct 

  module V = 
  struct
    type t = {state: A.States.elt; initial:bool; final:bool}
    let state a = a.state
    let initial a = a.initial
    let final a = a.final
    let make s i f = {state=s; initial=i; final=f}
    let set_initial a = {a with initial = true}
    let set_final a = {a with final = true}

    let compare x y = 
      A.States.compare (A.States.singleton (state x))
	(A.States.singleton (state y))
    let equal x y = compare x y = 0

    (* !!! *)
    let hash x = Hashtbl.hash (A.States.singleton (state x))
  end

  module G = Graph.Persistent.Digraph.ConcreteLabeled(V)
    (struct open A.Alphabet 
    type t = A.Alphabet.elt option
    let compare = Pervasives.compare 
    let default = None end)

  module M = 
  struct
    type t = G.t
    module Vertex = V
    module V = G.V
    module E = G.E
    let iter_vertex = G.iter_vertex
    let iter_edges_e = G.iter_edges_e
    let graph_attributes i = []
    let default_vertex_attributes i =  []
    let vertex_name v = P.state (Vertex.state v)
	
    let vertex_attributes v = 
      match Vertex.initial v, Vertex.final v with 
      | true,false -> [`Shape `Box]
      | false,true -> [`Shape `Doublecircle]
      | true,true -> [`Shape `Diamond]
      | _ -> [`Shape `Circle]


    let get_subgraph v = None
    let default_edge_attributes i = []
    let edge_attributes e = match G.E.label e with
      | Some label -> [ `Label (P.label label)]
      | None -> []
  end
  module Print = Graph.Graphviz.Dot(M)


  (* build a graph from an automaton *)
  let build auto =
    let is_initial x = A.States.mem x
      (A.States.singleton (A.initial auto)) in
    let is_final x = A.States.mem x (A.final auto) in
    let make_vertex s = 
      V.make s (is_initial s) (is_final s)
    in
    let g = 
      A.States.fold 
	(fun s g ->
	  G.add_vertex g (make_vertex s)) (A.states auto) G.empty
    in
    A.States.fold (fun s g ->
      A.Alphabet.fold (fun l g ->
	let t = A.delta auto s l
	in G.add_edge_e g (make_vertex s,Some l,make_vertex t))
	(A.alphabet auto) g
    ) (A.states auto) g

  (* build a automaton from a graph *)
  let of_graph graph = 
    let auto = 
      G.fold_vertex 
	(fun state auto -> 
	  let auto = A.add_state auto (V.state state) in 
	  let auto = 
	    if V.initial state
	    then A.set_initial auto (V.state state)
	    else auto in
	  if V.final state 
	  then A.add_final auto (V.state state)
	  else auto) 
	graph A.empty 
    in
    G.fold_edges_e 
      (fun edge auto -> match G.E.label edge with 
	| None -> Printf.eprintf "warning: edge with no label\n"; auto 
	| Some l -> A.add_transition auto (V.state (G.E.src edge)) (A.Alphabet.singleton l) (V.state (G.E.dst edge))) 
      graph auto

  let fprint_graph x a = Print.fprint_graph x (build a)
  let output_graph x a = Print.output_graph x (build a)

  module DP =
  struct
    let node id atr_list = 
      let id = match id with
	| (Graph.Dot_ast.Ident s,_)
	| (Graph.Dot_ast.Number s,_)
	| (Graph.Dot_ast.String s,_)
	| (Graph.Dot_ast.Html s,_) -> s
      in 
      let n = P.state_of_string id in
      let rec aux n = function
	| [] -> n
	| (Graph.Dot_ast.Ident "shape", 
	   Some (Graph.Dot_ast.String "doublecircle")) :: s 
	  -> aux (V.set_final n) s
	| (Graph.Dot_ast.Ident "shape", 
	   Some (Graph.Dot_ast.String "diamond")) :: s 
	  -> aux (V.set_final (V.set_initial n)) s
	| (Graph.Dot_ast.Ident "shape", 
	   Some (Graph.Dot_ast.String "box")) :: s 
	  -> aux (V.set_initial n) s

	| (Graph.Dot_ast.Ident "shape", 
	   Some (Graph.Dot_ast.String "circle")) :: s 
	  -> aux n s

	| (Graph.Dot_ast.String string1, _) :: s 
	| (Graph.Dot_ast.Ident string1, _) :: s 
	| (Graph.Dot_ast.Number string1, _) :: s 
	| (Graph.Dot_ast.Html string1, _) :: s 
	  -> 
	  Printf.eprintf "ignored field on vertex:%s\n" string1;
	    aux n s
      in
      List.fold_left aux (V.make n false false) atr_list
      
    let rec edge = 
      let rec aux m = function
	| [] -> m
	| (Graph.Dot_ast.Ident "label", 
	   Some(Graph.Dot_ast.String st) ) :: s -> 
	  aux (Some (P.label_of_string st)) s

	| (Graph.Dot_ast.String string1, _) :: s 
	| (Graph.Dot_ast.Ident string1, _) :: s 
	| (Graph.Dot_ast.Number string1, _) :: s 
	| (Graph.Dot_ast.Html string1, _) :: s 
	  -> 
	  Printf.eprintf "ignored field on edge:%s\n" string1;
	    aux m s

      in List.fold_left aux None
  end

  module Builder = Graph.Builder.P(G)
  module DotParser = Graph.Dot.Parse(Builder)(DP)
  let parse_graph = DotParser.parse

  let parse s = of_graph (parse_graph s)

end 
