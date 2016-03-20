(*
 * Praline
 * Copyright 2011 LSV, CNRS & ENS de Cachan, France
 * Author: Romain Brenguier <brenguier@lsv.ens-cachan.fr>
 * File: repellor.ml
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

let compute_strategy = ref true
let print_found_solution = ref false
let verbose = ref false
let time_infos = ref false

let start_clock () = Unix.gettimeofday ()
let clock_value x = Unix.gettimeofday () -. x
let display_clock string clock =
  if !time_infos 
  then 
    (
      Printf.printf "%s %f sec." string (clock_value clock);
      print_newline ()
    )

 
module Make =
  functor (G:Game.BUCHI) ->
struct

  module U = Util.Graph(G.A.G)
  module StronglyConnected = Graph.Components.Make(G.A.G)
  module Operations = Graph.Oper.P(G.A.G)
  module PlayerMap = Map.Make(G.A.M.Player)
  module AU = Arena.Util(G.A)
  module PlayerSet = AU.PlayerSet
  module StateSet = AU.StateSet
  module Payoff = Payoff.Make(G.A.M.Player)    

  type constr = int PlayerMap.t

  type shape = 
      { arena : G.A.t;
	payoff : Payoff.t;
	scc : G.A.t}

  type repellors = (G.A.M.Player.t list * int * Payoff.t,G.A.t) Hashtbl.t

  type solution = 
      { 
	shapes : shape list;
	computed : repellors;
      }

  let shapes s = s.shapes
  let payoff_of_shape s = s.payoff
  let arena_of_shape s = s.arena
  let scc_of_shape s = s.scc
  let repellors s = s.computed

  let min_constr constr player = 
    try PlayerMap.find constr player
    with Not_found -> 0
      
  let make_constr g fn = 
    let map = PlayerMap.empty in
    PlayerSet.fold (fun p m -> PlayerMap.add p (fn p) m) (G.players g) map 

  let empty_constr = PlayerMap.empty

  let satisfy_min_constr constr payoff =
    PlayerMap.fold 
      (fun player min res -> 
	res && min <= Payoff.get player payoff)
      constr true


  exception Found of G.A.t

  (** Compute the repellor and the secure transitions in the game g
      for a set of player p, as well as witness moves for all the
      states inside of the repellor *)
  let iterated_repellor computed game p payoff = 
    
    let suspect = AU.suspect (G.arena game) in

    let rec repellor p n = 
      if n = 0
      then G.A.G.empty
      else if AU.PlayerSet.is_empty p
      then G.arena game
      else 
	let elements = PlayerSet.elements p in
	if Hashtbl.mem computed (elements,n,payoff)
	then Hashtbl.find computed (elements,n,payoff)
	else
	  (
	    
	    let c = start_clock () in
	    let start,_ =
	      try 
		Hashtbl.fold 
		  (fun (suspp,np,payp) set (accu,nb) ->
		    let cond = 
		      List.fold_left 
			(fun accu a -> 
			  accu &&
			    ((Payoff.get a payoff <= 
				Payoff.get a payp)
			     || not (List.mem a suspp))
			) 
			true elements
		    in
		    let cond2 = 
		      List.fold_left 
			(fun accu a -> accu && PlayerSet.mem a p)
			cond suspp
		    in
		    if np = n && cond2
		    then 
		      let nbp = G.A.G.nb_vertex set in 
		      if nbp < nb 
		      then 
			(*Operations.intersect set accu,nbp*)
			set,nbp
		      else accu,nb

		    (*raise (Found set)*)
		    else (accu,nb)
		  ) computed (G.arena game,G.A.G.nb_vertex (G.arena game))
	      with Found set -> set,0
	    in
	    if !time_infos 
	    then 
	      Printf.printf "number of states %d\n" 
		(G.A.G.nb_vertex start);
	    display_clock "antichain condition computed in " c;
	    
	    (* the condition that has to be respected for an edge to be
	       in the repellor *)
	    let condition rep s m t = 
	      let susp = suspect s t m in
	      let p_inter_susp = PlayerSet.inter p susp in 
	      if PlayerSet.exists 
		(fun b -> 
		  (G.buchi_objective (G.objective game b)) t > Payoff.get b payoff)
		p_inter_susp
	      then
		G.A.G.mem_vertex 
		  (repellor p_inter_susp (n-1)) t
	      else if PlayerSet.subset p susp 
	      then G.A.G.mem_vertex rep t 
	      else 
		G.A.G.mem_vertex
		  (repellor p_inter_susp n)
		  t
	    in
	    
	    (* we loop until finding a fix point *)
	    let c = start_clock () in
	    let rec loop rep =
	      let rep,forall =
		G.A.G.fold_vertex
		  (fun s (rep,forall) ->
		    let rep,exists =
		      G.A.G.fold_succ_e
			(fun edge (rep,exists) -> 
			  let move = G.A.move (G.A.G.E.label edge) in
			  if U.for_all_succ (G.arena game) s
			    (condition rep s move) 
			  then rep,true
			  else G.A.G.remove_edge_e rep edge, exists
			)  rep s (rep,false)
		    in
		    if exists then (rep,forall)
		    else (G.A.G.remove_vertex rep s, false)
		  ) rep (rep, true)
	      in
	      if forall then rep else loop rep
	    in
	    let rep = loop (*G.arena game*) start in
	    display_clock "attractor computed in " c;
	    Hashtbl.add computed (elements,n,payoff) rep;
	    rep
	  )
    in
    
    let rec iterate rep n =
      let nrep = repellor p n in 

      (* to find a fix point we count the number of edges 
	 in Rep for all the set of suspects *)
      let fixpoint = Hashtbl.fold 
	(fun (p,m,pay) rep same -> 
	  same && 
	    if m = n 
	    then 
	      try U.nb_edges rep = U.nb_edges (Hashtbl.find computed (p,(m-1),pay))
	      with Not_found -> false
	    else true)
	computed true
      in 
      if fixpoint then rep
      else iterate nrep (n+1)
	
    in iterate G.A.G.empty 1 
    

  (** gives a list of the "maximal" equilibria *)
  let buchi_equilibria ?constr:(constr=empty_constr) g =
    
    let all_players = G.players g in

    let computed = Hashtbl.create 100 in 
    (* we keep a table containing the repellors for the set of 
       players, the integer n and the payoff for which it has
       already be computed *)

    (** compute the coreachable state of the subarena *)
    let coreachable sub arena =
      let to_visit = 
	G.A.G.fold_vertex 
	  (fun v s -> StateSet.add v s) sub StateSet.empty in 
      let visited = sub in
      let rec aux tov vis = 
	let tov,vis,changed =
	  StateSet.fold
	    (fun state (tov,vis,changed) -> 
	      List.fold_left
		(fun (tov,vis,changed) e -> 
		  let v,m,w = G.A.G.E.src e, 
		    G.A.G.E.label e, G.A.G.E.dst e in
		  if G.A.G.mem_vertex vis v
		  then (tov,vis,changed)
		  else 
		    (StateSet.add v tov, 
		     G.A.G.add_edge_e vis (G.A.G.E.create v m w), true)
		)  (tov,vis,changed)  (G.A.G.pred_e arena state)
	    ) tov (StateSet.empty,vis,false) 
	in if StateSet.is_empty tov then vis else aux tov vis
      in aux to_visit visited
    in

    let rec solve_subgame sub_game =
      if !verbose 
      then print_endline "computing strongly connected components";
      let c = start_clock () in
      let scc = StronglyConnected.scc_list (G.arena sub_game) in
      display_clock "strongly connected componnents computed in " c;

      if !verbose 
      then Printf.printf "%d found\n" (List.length scc);

      let payoff g state_list =
	PlayerSet.fold
	  (fun player map ->
	    let payoff = List.fold_left 
	      (fun payoff s -> 
		max payoff ((G.buchi_objective (G.objective g player)) s))
	      0 state_list
            in Payoff.set player payoff map 
	  )  (G.players g) Payoff.default
      in
      
      List.fold_left
	(fun result component ->
	  if !verbose 
	  then print_endline "starting computation for one component";
	  let subg = U.subgraph (G.arena sub_game) component in
	  (* I think that the function provided by StronglyConnected 
	     considers a single state with no edges a scc, 
	     but we do not, so we have to exclude the scc with
	     no edges *) 
	  if G.A.G.nb_edges subg = 0
	  then result
	  else

	    let _ = if !verbose 
	      then print_endline "computing payoff"
	    in

	    let payoff = payoff g component in
	    
	    if satisfy_min_constr constr payoff 
	    then 
	      
	      let c = start_clock () in
	      let rep_trans_sys = 
		iterated_repellor computed g all_players payoff in
	      display_clock "repellor computed in " c;
	      
	      if U.is_subgraph subg rep_trans_sys
	      then
		(if !verbose 
		 then Printf.printf "computing coreachable states\n";
		 let coreach = coreachable subg rep_trans_sys in
		 if !verbose
		 then print_endline "found 1 solution";
		 {arena=coreach;payoff=payoff;scc=subg} :: result)
	      else 
		List.rev_append 
		  (solve_subgame
		     (G.set_arena sub_game
			(Operations.intersect subg rep_trans_sys)))
		  result
	    else result
	) [] scc 

    in 
    let sol = solve_subgame g in
    if !verbose then print_endline "computation of the solution for the subgame finished";
    {shapes = sol; computed = computed}
 
end
