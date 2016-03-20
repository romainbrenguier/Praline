(*
 * Praline
 * Copyright 2011 LSV, CNRS & ENS de Cachan, France
 * Author: Romain Brenguier <brenguier@lsv.ens-cachan.fr>
 * File: strategy.ml
 * Created: Wed Sep 21 2011
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

let verbose = ref false

module MakeState = 
  functor (G:Game.BUCHI) ->
struct
  module AU = Arena.Util(G.A)
  module PlayerSet = AU.PlayerSet

  type outside_path = 
      { 
	n: int; 
	suspects: PlayerSet.t; 
	current_state: G.A.G.V.t
      }
	
  let get_n s = s.n

  type t = 
    | Initial
    | OnPath  of G.A.G.V.t (* current state *)
    | Loop of G.A.G.V.t * G.A.G.V.t (* current state and next target *)
    | Outside of outside_path
    | Played of G.A.M.t * t * G.A.G.V.t (* played m in order to reach v *)

  let rec current_state = function 
    | Initial -> failwith "initial memory : no current state recorded"
    | Outside s -> s.current_state
    | OnPath s -> s
    | Loop (s,t) -> s
    | Played (m,t,v) -> current_state t
	
  let make_outside n suspects s = 
    Outside { n=n; suspects=suspects; current_state = s;}
      
  let compare_outside a b =
    if not (a.n = b.n )
    then a.n - b.n
    else if not (G.A.G.V.equal a.current_state b.current_state)
    then G.A.G.V.compare a.current_state b.current_state
    else PlayerSet.compare a.suspects b.suspects

  let is_outside = function 
    | Outside s -> Some s
    |  _ -> None
      
  let initial = Initial
  let on_path s = OnPath s
  let loop s t = Loop (s,t)
    

  let compare = 
    let rec aux a b = match a,b with 
      | Initial , Initial -> 0
      | Initial , _ -> 1
      | _ , Initial -> -1
      | OnPath x, OnPath y -> G.A.G.V.compare x y
      | OnPath x, _ -> 1
      | _ , OnPath x -> -1
      | Loop (x,y) , Loop (w,z) -> if G.A.G.V.compare x w = 0 then G.A.G.V.compare y z else G.A.G.V.compare x w
      | Loop (x,y) , _ -> 1
      | _, Loop (x,y) -> -1
      | Outside x, Outside y -> compare_outside x y
      | Outside x, _ -> 1
      | _ , Outside x -> -1
      | Played (x1,x2,x3) , Played (y1,y2,y3) -> compare (x1,x2,x3) (y1,y2,y3)  (* !!! Not correct *)
    in fun a b -> aux b a
   
  module PSP = Util.Set(PlayerSet)
    
  let rec to_string s of_state of_player of_move = 
    match s with 
      | Initial -> "initial"
      | OnPath v -> "on_path "^of_state v
      | Loop (v,t) -> "loop "^of_state v^" to "^of_state t
      | Outside o ->
	Printf.sprintf "out %s, Susp=%s, n=%d"
 	  (of_state (current_state s))
	  (PSP.to_string of_player o.suspects)
	  (get_n o)
      | Played (m,t,v) -> "played "^of_move m^" ("^to_string t of_state of_player of_move^") "^of_state v


  let rec to_code s of_state players of_player of_move = 
    match s with 
      | Initial -> "initial == 1"
      | OnPath v -> "on_path == 1 && ("^of_state v^")"
      | Loop (v,t) -> "loop == 1 && ("^of_state v^") && ("^(fun x -> "next_"^of_state x) t^")"
      | Outside o ->
	Printf.sprintf "out == 1 && (%s) && susp == %d && n=%d"
 	  (of_state (current_state s))
	  (PSP.to_int players of_player o.suspects)
	  (get_n o)
      | Played (m,t,v) -> "played == ("^of_move m^") && ("^to_code t of_state players of_player of_move^") "^of_state v

  let move_to_int of_player of_action m = 
    G.A.M.fold (fun p a x -> (of_player p, of_action a) :: x) m []

  let rec gen_code s of_state players of_player of_action =
    let atom_of_state x = CodeGenerator.Expr.And (List.map (fun (x,i) -> CodeGenerator.Expr.Atom (x,i)) (of_state x)) in
    match s with 
    | Initial -> CodeGenerator.Expr.Atom("initial",1)
    | OnPath v -> CodeGenerator.Expr.conj (CodeGenerator.Expr.Atom("on_path",1)) (atom_of_state v)
    | Loop (v,t) -> CodeGenerator.Expr.And (CodeGenerator.Expr.Atom("loop",1) :: atom_of_state v :: [CodeGenerator.Expr.map (function (x,i) -> "next_"^x,i) (atom_of_state t)])
    | Outside o ->
       CodeGenerator.Expr.And 
	 (CodeGenerator.Expr.Atom("out",1) 
	  :: CodeGenerator.Expr.Atom("susp", PSP.to_int players of_player o.suspects)
	  :: CodeGenerator.Expr.Atom( "n",(get_n o)) :: [atom_of_state (current_state s)])
 				 
    | Played (m,t,v) -> 
       let played = move_to_int of_player of_action m in
       let played_list = List.map (fun (x,i) -> CodeGenerator.Expr.Atom(x^"_played",i)) played in
       CodeGenerator.Expr.And ( atom_of_state v :: gen_code t of_state players of_player of_action :: played_list)

  let rec gen_assign_code s of_state players of_player of_action =
    match s with 
    | Initial -> ["initial",1]
    | OnPath v -> ("on_path",1) :: of_state v
    | Loop (v,t) -> ("loop",1) :: (CodeGenerator.Assign.map (function (x,i) -> "next_"^x,i) (of_state t)) @ of_state v
    | Outside o ->
       ("out",1) :: ("susp", PSP.to_int players of_player o.suspects) :: ("n",(get_n o)) :: of_state (current_state s)
 				 
    | Played (m,t,v) -> 
       let played = move_to_int of_player of_action m in
       let played_list = List.map (fun (x,i) -> (x^"_played",i)) played in
       played_list @ (gen_assign_code t of_state players of_player of_action) @ of_state v



  exception AllSuspects
  let rec suspects = function
    | Outside o -> PlayerSet.elements o.suspects
    | Played (_,t,_) -> suspects t
    | _ -> raise AllSuspects

end


module Make = 
  functor (G:Game.BUCHI) ->
struct
  module AU = Arena.Util(G.A)
  module Memory = MakeState(G)
  module PlayerSet = AU.PlayerSet
  module StateSet = AU.StateSet
  module MemorySet = Set.Make(Memory)
  module Payoff = Payoff.Make(G.A.M.Player)

  module Dij = Graph.Path.Dijkstra(G.A.G)
    (struct type t = int 
	    type label = G.A.G.E.label
	    let weight x = 1
	    let compare = compare
	    let add x y = x + y
	    let zero = 0 end)

  module MoveMap = Map.Make(Memory)
  module UpdateMap = Map.Make
    (struct type t = Memory.t * G.A.G.V.t 
	    let compare (a,b) (c,d) = 
	      if G.A.G.V.compare b d <> 0
	      then G.A.G.V.compare b d
	      else Memory.compare a c end)

    (* A strategy, given a memory state and a configuration of the game, 
       gives the next action.
       And given the new state, update the memory state.
       We have two tables for that.
    *)
  type t = 
      {
	(*move : (Memory.t, G.A.M.t * Memory.t) Hashtbl.t;
	update : (Memory.t * G.A.G.V.t, Memory.t) 
	  Hashtbl.t;*)
	move : (G.A.M.t * Memory.t) MoveMap.t;
	update : Memory.t UpdateMap.t;
	players : PlayerSet.t;
      }

  let empty () = 
    (*{move = Hashtbl.create 100; 
     update  = Hashtbl.create 100;}*)
    {move = MoveMap.empty; 
     update  = UpdateMap.empty;
     players = PlayerSet.empty;
    }

  exception MoveNotFound of Memory.t
  let move strat memory = 
    try (*Printf.printf "find : %d\n" (Hashtbl.hash memory); Hashtbl.find strat.move memory*)
      MoveMap.find memory strat.move
    with Not_found -> raise (MoveNotFound (memory))
    
  exception UpdateNotFound of (Memory.t * G.A.G.V.t)
  let update strat memory state = 
    try (*Hashtbl.find strat.update (memory,state)*)
      UpdateMap.find (memory,state) strat.update
    with Not_found -> raise (UpdateNotFound (memory,state))

  let add_move strat a move t = 
    (*Printf.printf "add : %d\n" (Hashtbl.hash a);*)
    (*Hashtbl.add strat.move a (move, Memory.Played (move, a,t))*)
    { strat with move = MoveMap.add a (move, Memory.Played (move, a,t)) strat.move}

  let add_update strat a s b = 
    (*Hashtbl.add strat.update (a,s) b*)
    { strat with update = UpdateMap.add (a,s) b strat.update}

  (* Add moves to go to the scc *)
  let path strat scc arena =
    let rec loop (connected,strat) =
      (* add edges that go from a state not connected to the scc to one that is *)
      let (changed,connected,strat) =
	G.A.G.fold_edges_e 
	  (fun edge (changed,connected,strat) -> 
	    let src = G.A.G.E.src edge in
	    let dst = G.A.G.E.dst edge in
	    if not (G.A.G.mem_vertex connected src) && (G.A.G.mem_vertex connected dst)
	    then 
	      let move = G.A.move (G.A.G.E.label edge) in
	      (true, G.A.G.add_vertex connected src, add_move strat (Memory.on_path src) move dst)
	    else (changed, connected,strat)
	  ) arena (false,connected,strat)
      in 
      if changed then loop (connected,strat)
      else connected,strat
    in snd (loop (scc,strat))
      

  let elt sub = 
    G.A.G.fold_vertex (fun v l -> v :: l) sub []

  let first_target elt = List.hd elt

      
  let next_target v elt = 
    let rec loop = function 
      | [] -> failwith "no target in the strategy"
      | x :: [] -> List.hd elt
      | x :: y :: s when G.A.G.V.equal x v  -> y
      | x :: s -> loop s
    in loop elt

    
    
  let initial_moves strat scc elt arena start =
    let treat_vertex v strat = 
      if (G.A.G.mem_vertex scc v)
      then add_update strat Memory.initial v (Memory.loop v (next_target v elt))
      else add_update strat Memory.initial v (Memory.on_path v) 
    in
    (*match start with 
      | Some v -> treat_vertex v strat
      | None -> *) G.A.G.fold_vertex treat_vertex arena strat


  let loop (strat:t) elt sub = 
    let visited = StateSet.empty in
    
    let find_path w v visited strat = 
      let path, weight = Dij.shortest_path sub w v in
      (*Printf.printf "Length of the path (nb of edges): %d \n" (List.length path);*)
      (* We have to be carrefull with self loops *)
      if List.length path = 0 
      then 
	let edge = G.A.G.find_edge sub w v in
	let move = G.A.move (G.A.G.E.label edge) in
	StateSet.add v visited, 
	add_move strat (Memory.loop w v) move v
      else
	List.fold_left 
	  (fun (visited,strat) edge ->
	    let move = G.A.move (G.A.G.E.label edge) in
	    let dst = G.A.G.E.dst edge in
	    StateSet.add dst visited, add_move strat (Memory.loop (G.A.G.E.src edge) v) move dst)
	  (visited,strat) path
    in	  

    let first = first_target elt in
    let rec loop current strat =
      let next = next_target current elt in
      let new_visited,strat = find_path current next visited strat in
      if not (G.A.G.V.equal next first ) 
      then loop next strat
      else strat
    in loop first strat

 
  let set_of_list p = 
    List.fold_left (fun set x -> PlayerSet.add x set) PlayerSet.empty p
      
  let move_from_repellor strat table payoff =
    let treat_edge susp n edge strat =
      let src = G.A.G.E.src edge in
      let move = G.A.move (G.A.G.E.label edge) in
      let dst = G.A.G.E.dst edge in
      let memory = Memory.make_outside n (set_of_list susp) src in
      add_move strat memory move dst
    in 
    let treat_row (susp,n,pay) arena strat = 
      if Payoff.compare pay payoff = 0
      then G.A.G.fold_edges_e (treat_edge susp n) arena strat
      else strat
    in Hashtbl.fold treat_row table strat
    

  exception NoMove of G.A.G.V.t

  let updates_from_moves game payoff strat scc elt =
    let players = G.players game in

    let treat_move q (m,p) strat =
      let move,memory,target = match p with 
	| Memory.Played(mo,me,t) -> mo,me,t 
	| _ -> failwith "strategy in not in the right memory state"
      in
      (* look at the successors of s in the graph *)
      let s = Memory.current_state memory in
      let successors = G.A.G.succ (G.arena game) s in

      let treat_vertex strat t = 
	if G.A.G.V.equal t target && (match memory with Memory.Outside o -> false | _ -> true)
	then 
	  if not (G.A.G.mem_vertex scc t )
	  then add_update strat p t (Memory.on_path t)
	  else

	    let new_mem = 
	      match memory with 
	      | Memory.Loop (y,z) -> 
		if G.A.G.V.equal z t
		then Some (Memory.Loop (t,next_target t elt))
		else Some (Memory.Loop (t,z))
	      | _ -> 	    
		  (* look for a memory state that corresponds to a loop going through the state t *)	      
		MoveMap.fold 
		  (fun mem (m,new_mem) sol -> 
		    match sol with 
		    | Some x -> Some x
		    | None -> match mem with
		      | Memory.Loop (y,z) when G.A.G.V.equal y t -> Some mem 
		      | _ -> None)
		  strat.move None 

	    in match new_mem with
	    | Some m -> add_update strat p t m
	    | None -> raise (NoMove t)
		
	else

	  (* compute the suspects *)
	  let suspects =
	    match Memory.is_outside q with 
	      | Some o -> PlayerSet.inter (AU.suspect (G.arena game) s t m) (o.Memory.suspects)
	      | None -> (AU.suspect (G.arena game) s t m)
	    (*try PlayerSet.inter (Memory.suspects q) (AU.suspect (G.arena game) s t m)
	    with AllSuspects ->
	      (AU.suspect (G.arena game) s t m)*)
	  in  	  
	  let n = 
	    match Memory.is_outside q with
	      | None -> 
		  (* find a n for which the state [t] is in the repellor *)
		(match
		    (MoveMap.fold
		       (fun q (m,p) accu ->
			 match Memory.is_outside q with
			   | None -> accu
			   | Some o -> 
			     if G.A.G.V.equal t (Memory.current_state q) &&
			       PlayerSet.subset (* equal ?*)
			       suspects (o.Memory.suspects)
			     then Some (Memory.get_n o)
			     else accu
		       ) strat.move None)
		 with Some n -> n
		   | None -> Printf.eprintf "could not find n for which the current state belongs to the repellor\n"; -1
		(* ?  this should not happen if the strategy was
		   constructed correctly *)
		)
	      | Some o -> 
		if PlayerSet.exists 
		  (fun b -> 
		    (G.buchi_objective (G.objective game b)) t
		    > Payoff.get b payoff) 
		  suspects
		then Memory.get_n o - 1 
		else Memory.get_n o
	  in
	  add_update strat p t (Memory.make_outside n suspects t)
      in
      List.fold_left treat_vertex strat successors 
    in
    MoveMap.fold treat_move strat.move {strat with players = players}

  module Rep = Repellor.Make(G)

  let from_shape game repellors shape =       
    let strat = empty () in
    let sol = Rep.arena_of_shape shape in
    let payoff = Rep.payoff_of_shape shape in
    let scc = Rep.scc_of_shape shape in
    let elt = elt scc in
    let strat = move_from_repellor strat repellors payoff in
    let strat = loop strat elt scc in
    let strat = path strat scc sol in 
    let strat = initial_moves strat scc elt sol (G.start game) in
    let strat = updates_from_moves game payoff strat scc elt in
    strat


  let accessible strat = 
    let rec aux visited =
	let fixpoint = ref true in
	let visited = 
	  MoveMap.fold 
	    (fun m1 (move,m2) visited ->
	      try 
		if (MemorySet.mem m1 visited) && not (MemorySet.mem m2 visited)
		then (fixpoint:= false; ( MemorySet.add m2 visited ))
		else visited
	      with Not_found -> visited
	    ) strat.move visited
	in
	let visited = 
	  UpdateMap.fold 
	    (fun (m1,s) m2 visited ->
	      try 
		if (MemorySet.mem m1 visited) && not (MemorySet.mem m2 visited)
		then (fixpoint:= false; MemorySet.add m2 visited )
		else visited
	      with Not_found -> visited
	    ) strat.update visited
	in
	if !fixpoint then visited else aux visited  
    in aux (MemorySet.singleton Memory.initial)
    
  let to_string (strat:t) of_state of_player of_move = 
    let accessible = accessible strat in
    "digraph {\n"^
      (UpdateMap.fold 
	 (fun (q,st) p s -> 
	   if MemorySet.mem q accessible &&
	     match Memory.is_outside p with
	       | Some s -> 
		 not (PlayerSet.is_empty (s.Memory.suspects))
	       | _ -> true 
		 then
	       "\""^
		 Memory.to_string q of_state of_player of_move ^"\" -> \""^
		 (Memory.to_string p of_state of_player of_move )^
		 "\" [label=\""^ of_state st ^ "\"];\n"^s
		 else s
	   )
	 strat.update "") ^
      (MoveMap.fold 
	 (fun q (m,p) s -> 
	   if MemorySet.mem q accessible
	   then
	     "\""^
	       Memory.to_string q of_state of_player of_move ^"\" -> \""^
	       (Memory.to_string p of_state of_player of_move )^
	       "\" [label=\""^ 		 
	       (try of_move m with _ -> "___") 
	     ^ "\"];\n"^s
	   else s
	 )
	 strat.move ""
      ) ^"\n}"

  let to_code (strat:t) of_state of_player of_move = 
    let accessible = accessible strat in
    let players = strat.players in
    let code_update = 
      UpdateMap.fold 
	 (fun (q,st) p s -> 
	   if MemorySet.mem q accessible &&
	     match Memory.is_outside p with
	       | Some s -> 
		 not (PlayerSet.is_empty (s.Memory.suspects))
	       | _ -> true 
	   then 
	     (CodeGenerator.Expr.conj 
		(CodeGenerator.Expr.map (fun (x,i) -> ("old_"^x,i)) (Memory.gen_code q of_state players of_player of_move))
		(CodeGenerator.Expr.And (List.map (fun (s,i) -> CodeGenerator.Expr.Atom (s,i)) (of_state st))),
	      Memory.gen_assign_code p of_state players of_player of_move) :: s
	   else s
	 )
	 strat.update []
    in 
    let code_move_common,move_players =
      MoveMap.fold 
	(fun q (m,p) (sc,sp) -> 
	 if MemorySet.mem q accessible
	 then
	   let code = Memory.gen_code q of_state players of_player of_move in
	   let actions = Memory.move_to_int of_player of_move m in
	   (code, Memory.gen_assign_code p of_state players of_player of_move) :: sc,
	   (if sp = [] 
	    then List.map (fun (p,a) -> p, [code,["return",a]]) actions
	    else List.map (fun (p,c) -> p, (code,["return",List.assoc p actions]) :: c) sp)
	 else (sc,sp)
	)
	strat.move ([],[])
	
    in 
    let buf = Buffer.create 400 in
    Printf.bprintf buf "function update () {\n";
    Printf.bprintf buf "%s\n}\n" (CodeGenerator.make code_update);
    Printf.bprintf buf "function common_move () {\n";
    Printf.bprintf buf "%s\n}\n" (CodeGenerator.make code_move_common);
    List.iter 
      (fun (p,c) -> 
       Printf.bprintf buf "function move_%s () {\n" p;
       Printf.bprintf buf "%s\n}\n" (CodeGenerator.make c);
      ) move_players;
    Buffer.contents buf
	   
  exception NoSuspect
  exception Stop



  let play game strat of_state of_player of_move evaluate payoff =
    let parse_command state move cmd =
      let lexbuf = Lexing.from_string cmd in
      let list = GeneratorParser.instruction_list GeneratorLexer.token lexbuf in
      evaluate state move list
    in
    
    let players = AU.PlayerSet.elements (G.players game) in

    let rec loop state mem = 
      try 
	print_endline "The current state is :";
	print_endline (of_state state);	    
	let updated = update strat mem state in
	(*print_endline "previous memory:";
	print_endline (Memory.to_string mem of_state of_player of_move);
	print_endline "new memory:";
	print_endline (Memory.to_string updated of_state of_player of_move);
	*)
		
	(match updated with 
	  | Memory.OnPath _ | Memory.Loop _ ->
	    print_endline "All the players are suspect.";
	    let string =
	      List.fold_left
		(fun s x -> s^" ; "^of_player x^" : "^string_of_int (payoff x state))
		(of_player (List.hd players)^" : "^string_of_int
		   (payoff (List.hd players) state)) (List.tl players)
	    in  print_endline string
	    
	  | _ ->
	    let suspects = Memory.suspects updated in
	    match suspects with 
	      | [] -> raise NoSuspect
	      | a :: [] -> 
		print_string ("The suspect is "^ of_player a^" and ");
		print_endline ("her current payoff is "^string_of_int (payoff a state))
	      | a :: s ->
		print_string "The suspects are ";
		let string =
		  List.fold_left
		    (fun s x -> s^" ; "^of_player x^" : "^string_of_int (payoff x state)) (of_player a^" : "^string_of_int (payoff a state)) s
		in print_endline string);
	       
	     

	let move,new_mem = move strat updated in 
	(*print_endline "Move found for memory:";
	print_endline (Memory.to_string updated of_state of_player of_move);
	*)

	(*print_endline (Memory.to_string new_mem of_state of_player of_move);*)
	print_string "Eve plays : ";
	print_endline (of_move move);
	let new_state = AU.successor (G.arena game) state move in
	print_endline "The natural outcome of this move is :";
	print_endline (of_state new_state);
	let rec aux new_state =
(*	  print_endline "Which variable do you want to change ? (press enter to leave unchanged, enter stop to quit the game)";*)
	  Printf.printf " > ";
	  flush stdout;
	  match read_line () with
	    | "" -> new_state
	    | "stop" | "quit" -> raise Stop
	    | "exit" -> exit 0
	    | cmd -> 
	      let update = parse_command new_state move cmd in
	      (*let update = 
		List.fold_left 
		  (fun state (var,i) -> 
		    Printf.printf "setting %s to %d\n" var i;
		    set state var i) new_state list
	      in*)
	      aux update
	in 
	let new_state = aux new_state in
	loop new_state new_mem
      with 
	| MoveNotFound mem -> failwith ("move not found for memory : "^ Memory.to_string mem of_state of_player of_move)
	| UpdateNotFound (mem,state) -> 
	  (*failwith ("update not found for memory : "^ Memory.to_string mem of_state of_player of_move^" in state "^of_state state)*) print_endline "this transition does not exist."
	| NoSuspect -> 
	  print_endline "The set of suspects is now empty.";
	  print_endline "Adam can no longer win.";
	| Stop -> print_endline "game stopped";

    in 
    match G.start game with 
      | Some state -> loop state Memory.initial
      | None -> failwith "no initial state provided"

end


 
