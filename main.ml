(*
 * Praline
 * Copyright 2011 LSV, CNRS & ENS de Cachan, France
 * Author: Romain Brenguier <brenguier@lsv.ens-cachan.fr>
 * File: main.ml
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

open Util

module Rep = Repellor.Make(Generator.Generic.G)
module AUtil = Arena.Util(Generator.Generic.G.A)
module GraphUtil = Util.Graph(Generator.Generic.G.A.G)
module AU = Arena.Util(Generator.Generic.G.A)
module Strategy = Strategy.Make(Generator.Generic.G)
module Payoff = Payoff.Make(Generator.Generic.G.A.M.Player)
module MUtil = Move.Util(Generator.Generic.G.A.M)

(* Options *)
let time_infos = ref false
let constr = ref (fun x -> 0)
let player = ref (-1)
let file_name = ref None
let invite () = 
  Printf.printf " > ";
  flush stdout
  
let arg = 
  [ "-ti" , Arg.Bool(fun b -> time_infos := b), "true : display information about performances";
    "-c", Arg.Tuple([Arg.Int(fun x -> player := x); 
		     Arg.Int(fun y -> 
		       let player = !player in
		       let old_constr = !constr in
		       constr := (fun p -> if p = player 
			 then y else old_constr p))]), "player minimum-payoff : add a constraint on the payoff";
    "-f", Arg.String (fun f -> file_name := Some f), "input file"]

let anon_fun = (fun f -> file_name := Some f)
let usage_msg =  "usage: "^Sys.argv.(0)^" <file>"
let verbose = ref false

let start_clock () = Unix.gettimeofday ()
let clock_value x = Unix.gettimeofday () -. x
let display_clock string clock =
  (if !time_infos then 
      Printf.printf "%s %f sec.\n" string (clock_value clock))


let parse_gen file_name = 
  try
    let file = open_in file_name in
    let lexbuf = Lexing.from_channel file in
    snd (GeneratorParser.main GeneratorLexer.token lexbuf)
  with
    | Parsing.Parse_error ->
      Printf.printf "parsing error line %d\n" !GeneratorLexer.nb_line;
      raise Parsing.Parse_error
    | x ->
      Printf.printf "error line %d\n" !GeneratorLexer.nb_line;
      raise x
	
let display_infos name game = 
  let arena = Generator.Generic.G.arena game in
  (*Printf.printf "file : %s\n" name;*)
  Printf.printf "number of players : %d\n" 
    (AU.PlayerSet.cardinal (Generator.Generic.G.players game));
  Printf.printf "number of states : %d\n" 
    (Generator.Generic.G.A.G.nb_vertex arena);
  Printf.printf "number of edges : %d\n" 
    (Generator.Generic.G.A.G.nb_edges arena)
  
(* Main function *)
let main =

  (* parse the arguments *)
  Arg.parse arg anon_fun usage_msg;
  let name = match !file_name with
    | Some f -> f
    | None -> Arg.usage arg usage_msg; exit 0
  in

  (* parse the input *)
  let game = parse_gen name in

  let constr = Rep.make_constr game 
    (fun p -> !constr (Generator.Player.hash p)) in
  
  display_infos name game;
  
  let solution = Rep.buchi_equilibria ~constr game in
  let shapes = Rep.shapes solution in
  let repellors = Rep.repellors solution in
  let initials = match Generator.Generic.G.start game with
    | Some x -> AUtil.StateSet.singleton x
    | _ -> failwith "no initial states"
  in

  (* first select the solutions with the initial state inside *)
  let nb,selection = 
    List.fold_left
      (fun (i,list) shape -> 
	let sol = Rep.arena_of_shape shape in
	let payoff = Rep.payoff_of_shape shape in
	(*let scc = Rep.scc_of_shape shape in*)
	if Generator.Generic.G.A.G.fold_vertex 
	  (fun v b -> 
	    b || AUtil.StateSet.mem v initials) sol false
	then 
	  (
	    let sol,cont = 
	      match Generator.Generic.G.start game with
		| None -> sol,true
		| Some v ->
		  try 
		    GraphUtil.accessible sol v,true
		  with _ -> sol,false
	    in
	    if cont
	    then
	      (Printf.printf "payoff of solution %d : " (i+1);
	       AU.PlayerSet.iter (fun player -> 
		 Printf.printf "%s : %d "
		   (Generator.Player.to_string player)
		   (Payoff.get player payoff))
		 (Generator.Generic.G.players game);
	       print_newline ();
	       (i+1,(shape,sol) :: list)
	      )
	    else (i,list)
	  )
	else (i,list)
      )
      (0,[]) shapes
  in 

  if selection = [] 
  then (print_endline "no solution"; exit 0);

  let selection = List.rev selection in
  
  let rec loop () = 
    let i = 
      if nb > 1 
      then
	try 
	  Printf.printf "select a solution in [1-%d] or 0 to quit\n" nb;
	  invite ();	  
	  read_int ()
	with _ -> 
	  Printf.printf "enter a number between 1 and %d\n" nb;
	  loop ();
	  0
      else 1
    in

    if i = 0 then exit 0;

    let shape,sol = List.nth selection (i-1) in

    print_endline "(0) exit;";
    print_endline "(1) display the shape of the solution;";
    print_endline "(2) output the shape in a file;";
    print_endline "(3) output the strategy in a file;";
    print_endline "(4) play against the strategy;";
    print_endline "(5) generate code from the strategy;";
    print_endline "(6) output the game in a file;";
    invite ();
    (   
      match read_line () with
	| "0" -> exit 0

	| "1" ->
	  (Generator.Generic.G.A.G.iter_edges
	     (fun a b -> Printf.printf "%s --> %s;\n"
	       (Generator.Parser.string_of_state a) 
	       (Generator.Parser.string_of_state b)) sol; 
	   print_newline ());

	| "2" ->
	  print_endline "file name:";
	  let name = read_line () in
	  let dot_file = open_out name in
	  Generator.Generic.Print.output_graph dot_file sol;
	  close_out dot_file; 
	  print_endline ("wrote "^name);
	  
	| "3" ->
	  let strat = Strategy.from_shape game repellors shape in
	  let strat_string =
	    Strategy.to_string strat Generator.Parser.string_of_state
	      Generator.Player.to_string
	      (MUtil.to_string string_of_int)
	  in
	  let strat_file_name = name^"_strategy_"^string_of_int i^".dot" in
	  let strat_file = open_out strat_file_name in
	  output_string strat_file strat_string;
	  close_out strat_file; 
	  print_endline ("wrote "^strat_file_name)

	| "4" ->
	  (
	    try 
	      let strat = Strategy.from_shape game repellors shape in
	  (*	  let set_variable state var i =
		  Generator.Map.add var i state
		  in*)
	  
	      let payoff player state =
		(Generator.Generic.G.buchi_objective (Generator.Generic.G.objective game player)) state
	      in
	      
	      Strategy.play game strat 
		Generator.Parser.string_of_state 
		Generator.Player.to_string
		(MUtil.to_string string_of_int)
		(*set_variable*) 
		Generator.Instruction.evaluate
		payoff
	    with
	      | Strategy.NoMove t -> failwith ("move not found for state : "^ Generator.Parser.string_of_state  t) 
	  )

	| "5" ->
	  let strat = Strategy.from_shape game repellors shape in
	  let strat_string =
	    Strategy.to_code strat Generator.Parser.assoc_of_state Generator.Player.to_string (fun x -> x)
	  in
	  let strat_file_name = name^"_generated_code_"^string_of_int i^".c" in
	  let strat_file = open_out strat_file_name in
	  output_string strat_file strat_string;
	  close_out strat_file; 
	  print_endline ("wrote "^strat_file_name)

	| "6" ->
	  let file = name^"_generated_game" in
	  Generator.Generic.Output.output file game;
	  print_endline ("wrote "^file^".dot|.game")
	    

	| s -> Printf.printf "%s is not a valid choice\nvalid choices are numbers between 0 and 4\n" s

    );
    loop ()
	  
  in
  loop ()


