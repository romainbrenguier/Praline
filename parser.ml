(*
 * Praline
 * Copyright 2011 LSV, CNRS & ENS de Cachan, France
 * Author: Romain Brenguier <brenguier@lsv.ens-cachan.fr>
 * File: parser.ml
 * Created: Mon Sep 19 2011
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

module AP = 
struct
  let state i = string_of_int i
  let label s = Gml.Node.id s
  let state_of_string s = Hashtbl.hash s
  let label_of_string s = Gml.Node.of_id s
end
module AutoParse = Auto.Dot(Gml.GmlGame.Auto)(AP)
module Game = Gml.GmlGame
module AU = Arena.Util(Game.A)
module StateSet = AU.StateSet

(* lexer *)
open Genlex
let lexer = Genlex.make_lexer
  ["arena"; "auto"; "buchi"; "objective"; 
   "reach"; "safety"; "start"; ","; ";"; "->"]

let find_label g l = 
  Game.A.G.fold_vertex
    (fun v s ->
      if Gml.Node.label (Game.A.G.V.label v) = l
      then StateSet.add v s
      else s)
    (Game.arena g)
    StateSet.empty 


let parse_state labels = 
  let aux s = 
    match labels with
      | Some g -> find_label g s
      | None -> 
	StateSet.add
	  (Gml.Node.of_id s)
	  StateSet.empty 
  in
  parser [< 'Int i >] -> aux (string_of_int i)
    | [< 'String s >] -> aux s
    | [< 'Ident s >] -> aux s

let parse_set labels = 
  let rec parse_set_s =
    parser [< 'Kwd ","; s = (parse_state labels); x=parse_set_s >] -> 
      StateSet.union s x
      | [< >] -> StateSet.empty
  in
  parser 
  [<  s = (parse_state labels); x=parse_set_s >] -> 
    StateSet.union s x
    | [< >] -> failwith "parser error 3"

let parse_payoff labels = 
  let tab = Hashtbl.create 10 in
  
  let parse_one_payoff = 
    parser 
    [< set = parse_set labels; stream>] ->
      (parser
      [< 'Kwd "->"; 'Int pay >] ->
	StateSet.iter 
	  (fun x -> Hashtbl.add tab x pay) set 
     | [< >] ->
       StateSet.iter 
	  (fun x -> Hashtbl.add tab x 1) set 
	) stream
  in

  let rec parse_payoff_s =
    parser (* p is not used, but it causes parse_one_payoff to be evaluated *)
      [<  'Kwd ";"; p = parse_one_payoff;  stream >] ->
    ignore p; parse_payoff_s stream
    | [< >] -> ()
      
  in
  parser 
  [< p = parse_one_payoff ; stream >] ->
	parse_payoff_s stream;
    (fun s -> 
	  (try Hashtbl.find tab s 
	   with Not_found -> 0))
    | [< >] -> failwith "parser error 5"


let parse_objective g dir labels = 
  parser
  [< 'Kwd "buchi"; p = parse_payoff labels >] ->  
      Game.buchi p
  | [< 'Kwd "reach"; p = parse_payoff labels >] ->  
    Game.reach p
  | [< 'Kwd "safety"; e = parse_set labels >] ->  
    Game.safety e
  | [< 'Kwd "auto"; 'String s >] ->  
    let auto = AutoParse.parse (Filename.concat dir s) in 
    (* AutoParse.output_graph stdout auto ;*)
    Game.automaton auto
  | [< >] -> failwith "parser error: the parser was waiting for one of the keywords: buchi, reach, safety, auto, start"


let rec parse_all_objectives g dir labels = 
  parser
  [< 'Kwd "objective"; 'Int player; 
     obj = parse_objective g dir labels; stream >] ->
    let ng = Game.set_objective g player obj
    in parse_all_objectives ng dir labels stream 
    | [< 'Kwd "start"; s = (parse_state labels); stream >] ->
    let ng = Game.set_start g (StateSet.choose s)
    in parse_all_objectives ng dir labels stream 
  
    | [< >] -> g

let parse dir = 
  parser
    [< 'Kwd "arena"; 'String file_name; stream >] ->
      let is_dot =
	Filename.check_suffix file_name ".dot" 
	|| Filename.check_suffix file_name ".gv" in
      let is_gml = Filename.check_suffix file_name ".gml" in
      let g = 
	if is_dot 
	then Game.make (Gml.GmlArena.parse_dot (Filename.concat dir file_name)) 
	else if is_gml 
	then Game.make (Gml.GmlArena.parse (Filename.concat dir file_name)) 
	else failwith ("Unknown extension for \""^file_name^"\".\nSupported extensions are: .dot .gv .gml.")
      in
      let is_labeled = 
	Game.A.G.fold_vertex (fun v b ->
	  Gml.Node.label (Game.A.G.V.label v) <> "" && b)
	  (Game.arena g) true
      in
      parse_all_objectives g dir
	(if is_labeled then Some g else None) stream


let parse_file file =
  let dir = Filename.dirname file in
  let inch = open_in file in
  let stream = Stream.of_channel inch in
  parse dir (lexer stream)


 
