(*open Util*)

module type S =
sig
  module A : Arena.S
  type t 
  type objective
    
  val set_arena : t -> A.t -> t
  val make : A.t -> t
  val arena : t -> A.t
  val objective : t -> A.M.Player.t -> objective
  val set_objective : t -> A.M.Player.t -> objective -> t
  val players : t -> Arena.Util(A).PlayerSet.t
  val start : t -> A.G.V.t option
  val set_start : t -> A.G.V.t -> t
end

module type BUCHI =
sig 
  include S
  val buchi_objective : objective -> (A.G.V.t -> int)
end


module type BUCHI_DEFINABLE =
sig 
  include S
  module Auto : Auto.P
    with type Alphabet.elt = A.G.V.t

  type buchi_objective =
    | Internal of (A.G.V.t -> int)
    | Automaton of (Auto.t * (Auto.States.elt -> int))
	
  val to_buchi : t -> objective -> buchi_objective

end 

module MakeBuchi = 
  functor (A:Arena.S) ->
struct
  module A = A
  module AU = Arena.Util(A)
  module PlayerSet = AU.PlayerSet
  module StateSet = AU.StateSet
  module PlayerMap = Map.Make(A.M.Player)

  type objective = A.G.V.t -> int

  type t =
      { arena: A.t; 
	players: PlayerSet.t; 
	objectives: objective PlayerMap.t;
	start: A.G.V.t option
      }
    
  let start g = g.start
  let set_start g s = {g with start= Some s}
  let arena g = g.arena    

  let objective g p = 
    try PlayerMap.find p g.objectives
    with _ -> (fun x -> 0)

  let states g = 
    A.G.fold_vertex (fun s v -> StateSet.add s v)
      (arena g) StateSet.empty
 
  let players g = g.players

  let set_objective g p ol =
    { g with 
      players = PlayerSet.add p (players g);
      objectives = PlayerMap.add p ol g.objectives}
      
  let make arena =
    { arena = arena ; 
      players = PlayerSet.empty (* fix this? *);
      objectives = PlayerMap.empty;
      start = None
    }

  let set_arena g a = {g with arena = a }

  let add_edge = A.G.add_edge_e
  let add_vertex = A.G.add_vertex

  let make_objective o = o
  let buchi_objective o = o
end


module Make =
  functor (A:Arena.S) ->
struct
  module A = A
  module AU = Arena.Util(A)
  module StateSet = AU.StateSet
  module PlayerSet = AU.PlayerSet
  module PlayerMap = Map.Make(A.M.Player)
  module Auto = Auto.Make(Util.Int)(StateSet)

  type target = A.G.V.t -> int

  type objective = 
    | Buchi of target
    | Reach of target
    | Safety of StateSet.t
    | OAuto of Auto.t

  type buchi_objective =
    | Internal of (A.G.V.t -> int)
    | Automaton of (Auto.t * (Auto.States.elt -> int))

  let buchi t = Buchi t
  let reach t = Reach t
  let safety t = Safety t
  let automaton a = OAuto a


  type t =
      { arena: A.t; 
	players: PlayerSet.t; 
	objectives: objective PlayerMap.t;
	start: A.G.V.t option
      }
    
  let arena g = g.arena    

  let objective g p = 
    try PlayerMap.find p g.objectives
    with _ -> buchi (fun x -> 0) 

  let states g = 
    A.G.fold_vertex (fun s v -> StateSet.add s v)
      (arena g) StateSet.empty
 
  let to_buchi g = function
    | Buchi target ->
      Internal target

    | Reach payoff ->
      let a = Auto.add_state Auto.empty 0 in
      let a = Auto.set_initial a 0 in      
      let a = A.G.fold_vertex
	(fun s a -> Auto.add_state a (payoff s)) (arena g) a
      in
      let a = 
	A.G.fold_vertex
	  (fun s a ->
	    let p = payoff s in 
	    Auto.States.fold
	      (fun t a -> 
	        Auto.add_to_transition a t s (max p t)) 
	      (Auto.states a) a
	  ) (arena g) a
      in Automaton (a, fun t -> t)
    | Safety target ->
      let a = Auto.add_state (Auto.add_state Auto.empty 1) 2 in
      let a = Auto.set_initial a 1 in
      let a = Auto.add_final a 1 in
      let a = Auto.add_transition a 2 (states g) 2 in
      let a = Auto.add_transition a 1 target 2 in
      let a = Auto.add_transition a 1
	(StateSet.diff (states g) target) 1 in
      Automaton (a, function 2 -> 0 | 1 -> 1 | x -> failwith ("Found value "^string_of_int x^" in the automaton"))
    | OAuto a -> Automaton (a, 
			    (fun s -> 
			      if Auto.States.mem s (Auto.final a)
			      then 1 else 0))

  let players g = g.players

  let set_objective g p ol =
    { g with 
      players = PlayerSet.add p (players g);
      objectives = PlayerMap.add p ol g.objectives}
      
  let make arena =
    { arena = arena ; 
      players = PlayerSet.empty (* fix this? *);
      objectives = PlayerMap.empty;
      start = None
    }

  let set_arena g a = {g with arena = a }

  let add_edge = A.G.add_edge_e
  let add_vertex = A.G.add_vertex


  let start g = g.start
  let set_start g i = { g with start = Some i}

end


module Output =
  functor (G:BUCHI) ->
    functor (P:
      sig 
	val state : G.A.G.V.t -> string
	val action : G.A.M.Action.t -> string
	val player : G.A.M.Player.t -> int
      end) ->
struct

  module AU = Arena.Util(G.A)
  module PlayerSet = AU.PlayerSet

  module ArenaPrinter = Arena.DotPrinter(G.A)(P)
  let output_arena filename game = 
    let out_channel = open_out filename in
    ArenaPrinter.output_graph out_channel (G.arena game);
    close_out out_channel

  let output filename game =
    let arena_file = filename^".dot" in
    let game_file = filename^".game" in
    output_arena arena_file game;
    let out_channel = open_out game_file in
    Printf.fprintf out_channel "arena %S\n" (Filename.basename arena_file);
    (match G.start game with
      | None -> ()
      | Some v -> 
	Printf.fprintf out_channel "start %s\n" 
	  (P.state v));
    PlayerSet.iter 
      (fun player1 ->
	let objective = 
	  G.buchi_objective (G.objective game player1) in
	Printf.fprintf out_channel "objective %d buchi" (P.player player1); 
	let _ = 
	  G.A.G.fold_vertex
	    (fun vertex is_first ->
	     let value = objective vertex in
	     if value <> 0 || is_first
	     then 
	       (if not is_first
		then
		  Printf.fprintf out_channel " ; ";
		Printf.fprintf out_channel 
			       " %s -> %d " (P.state vertex) value;
		false)
	     else is_first
	  ) (G.arena game) true
	in Printf.fprintf out_channel "\n"
      ) (G.players game);
    close_out out_channel
end



module ToBuchi =
  functor (G:BUCHI_DEFINABLE) ->
struct

  module PlayerMap = Map.Make(G.A.M.Player)
  module Prod = Product.Make(G.A)(G.Auto)(G.A.M.Player)
  module OldAU = Arena.Util(G.A)
  module OldPlayerSet = OldAU.PlayerSet


  module A = Prod.P
  module AU = Arena.Util(A)
  module StateSet= AU.StateSet
  module PlayerSet = AU.PlayerSet

  type objective = StateSet.elt -> int

  type t =
      { arena: A.t; 
	players: PlayerSet.t; 
	objectives: objective PlayerMap.t;
	start: A.G.V.t option
      }

  let arena g = g.arena    
  let players g = g.players
  let objective g p = 
    try PlayerMap.find p g.objectives
    with _ -> (fun x -> 0)


  let make arena =
    { arena = arena ; 
      players = PlayerSet.empty (* !!! fix this *);
      objectives = PlayerMap.empty; 
      start = None
    }

  let start g = g.start
  let set_start g s = {g with start= Some s}

  let set_arena g a = {g with arena = a }

  let add_edge = A.G.add_edge_e
  let add_vertex = A.G.add_vertex

  let set_objective g p ol =
    { g with 
      players = PlayerSet.add p (players g);
      objectives = PlayerMap.add p ol g.objectives}

  let buchi_objective obj = obj
  let from_buchi g = failwith "unimplemented from_buchi"
  let arena_from_buchi a = 
    let p = Prod.projection a in
    p

  let player_list g = 
    OldPlayerSet.fold (fun p list -> p :: list) (G.players g) [] 

  let to_buchi g = 
    let arena = Prod.of_graph (G.arena g) in
    let arena =
      OldPlayerSet.fold  
	(fun p arena ->
	  match G.to_buchi g (G.objective g p) with
	    | G.Automaton (auto,_) -> Prod.product arena p auto
	    | G.Internal _ -> arena)
	(G.players g) arena 
    in
    let game = 
      OldPlayerSet.fold  
	(fun p game ->
	  match G.to_buchi g (G.objective g p) with
	    | G.Automaton (_,payoff) ->
	    (* !! repetion of the computation of the product *)
	      let new_payoff s = 
		payoff (Prod.get s p) 
	      in set_objective game p new_payoff
	    | G.Internal payoff -> 
	      let new_payoff s = payoff (Prod.proj s) 
	      in set_objective game p new_payoff
	) (G.players g) (make arena) 
    in
    match G.start g with
      | None -> game
      | Some start ->
	let prod player s = 
	  match G.to_buchi g (G.objective g player) with
	    | G.Automaton (auto,_) ->
	      Prod.state_product s player (G.Auto.initial auto) 
	    | G.Internal _ -> s
	in 
	let i = OldPlayerSet.fold prod (G.players g) (Prod.inj start)
	in set_start game i


  let initial g = 
    let prod player s = 
      match G.to_buchi g (G.objective g player) with
	 | G.Automaton (auto,_) ->
	   Prod.state_product s player (G.Auto.initial auto) 
	 | G.Internal _ -> s
    in 
    let add_vertex state set = 
      StateSet.add 
	(OldPlayerSet.fold prod (G.players g) (Prod.inj state))
	set
    in
    G.A.G.fold_vertex add_vertex (G.arena g) 
      (StateSet.empty)

  module DotPrinter = 
    functor (P:
      sig 
	val conf : G.A.G.V.t -> string
	val state : G.Auto.States.elt -> string
	val action : G.A.M.Action.t -> string
      end ) ->
  struct 
    module PP = Prod.DotPrinter(P)
    open PP

    let string_of_state = string_of_state
    let fprint_graph = fprint_graph
    let output_graph = output_graph

    let objectives game player = 
      PlayerSet.fold
	(fun p string -> 
	  let payoff = buchi_objective 
	    (objective game p) in
	  let string = 
	    Printf.sprintf "%splayer %s : \n"
	      string (player p)
	  in
	  A.G.fold_vertex 
	    (fun s string ->
	      let pay = payoff s
	      in if pay > 0 then
		  Printf.sprintf "%s%s -> %d\n" 
		    string (string_of_state s) pay
		else string)
	    (arena game) string
	) 
	(players game) ""
  end

    
end
