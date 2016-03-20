(*
 * Praline
 * Copyright 2011 LSV, CNRS & ENS de Cachan, France
 * Author: Romain Brenguier <brenguier@lsv.ens-cachan.fr>
 * File: util.ml
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

module MakeComparable =
  functor (S:sig type t end) ->
struct
  type t = S.t
  let compare = compare
  let equal a b = a = b
end

module MakeHashable = 
  functor (S:sig type t end) ->
struct
  type t = S.t
  let compare = compare
  let equal a b = a = b
  let hash = Hashtbl.hash
end

module Graph =
  functor (G:Graph.Sig.P) ->
struct
  exception Invalid_vertex of G.V.t

  let for_all_vertex g p =
    G.fold_vertex (fun v b -> b && p v) g true

  let for_all_succ g s p =
    G.fold_succ (fun v b -> b && p v) g s true

  let exists_vertex g p =
    G.fold_vertex (fun v b -> b || p v) g false

  let exists_edge g s p =
    try 
      G.fold_succ_e (fun e b -> b || p e) g s false
    with _ -> raise (Invalid_vertex s)

  let nb_edges g =
    (* G.Arena.fold_edges_e (fun e i -> i+1) g 0*)
    G.nb_edges g

  let subgraph g states =
    G.fold_edges_e
      (fun e ng ->
	let m = G.E.label e
	and v = G.E.src e
	and w = G.E.dst e
	in
	if G.mem_vertex ng v && G.mem_vertex ng w
	then G.add_edge_e ng 
	  (G.E.create v m w)
	else ng)	
      g
      (List.fold_left G.add_vertex G.empty states)
      
  let is_subgraph g h =
    for_all_vertex g (fun v -> G.mem_vertex h v)
    && 
      G.fold_edges_e (fun e b -> G.mem_edge_e h e && b) g true

  module States = Set.Make(G.V)

  let accessible g v = 
    let rec step to_visit h = 
      if States.is_empty to_visit then h
      else 
	let s = States.choose to_visit in
	let h, to_visit = 
	  try
	    G.fold_succ_e 
	      (fun e (h,to_visit) -> 
		let dest = G.E.dst e in
		(G.add_edge_e h e, 
		 if not (G.mem_vertex h dest)
		 then
		   States.add dest to_visit
		 else to_visit)
	      ) g s (h,to_visit)
	  with _ -> raise (Invalid_vertex s)
	in step (States.remove s to_visit) h
    in step (States.singleton v) G.empty

end


module Int =  
struct
  type t = int
  let compare a b = a - b
  let default = 0
  let hash i = i
  let equal a b = (a = b)
  let of_string a = int_of_string a
  let to_string a = string_of_int a
end

(** Maps indexed by integers. *)
module IntMap = Map.Make(Int)




module String =  
struct
  type t = string
  let compare a b = compare a b
  let default = ""
  let hash i = Hashtbl.hash i
  let equal a b = (a = b)
  let of_string a = a
  let to_string a = a
end

module Unit = 
struct 
  type t = unit
  let compare a b = 0
  let default = ()
  let equal a b = true
  let hash a = 0
  let of_string a = ()
  let to_string a = "()"
end


module Set = 
  functor (S:Set.S) ->
struct
  let to_string elt set =
    if S.is_empty set then "{}"
    else
      let fst = S.choose set in
      let set = S.remove fst set in
      "{"^S.fold
	(fun e string -> elt e^","^string)
	set (elt fst^"}")

  let to_code elt set =
    if S.is_empty set then "_empty_"
    else
      let fst = S.choose set in
      let set = S.remove fst set in
      "_"^S.fold
	(fun e string -> elt e^"_"^string)
	set (elt fst^"_")

  let to_int players elt set =
    S.fold
      (fun e int -> 10 * int + if S.mem e set then 1 else 0)
      players 0
    
  let hash set = 
    Hashtbl.hash (S.elements set)

end
