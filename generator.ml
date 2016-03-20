(*
 * Praline
 * Copyright 2011 LSV, CNRS & ENS de Cachan, France
 * Author: Romain Brenguier <brenguier@lsv.ens-cachan.fr>
 * File: generator.ml
 * Created: Wed Aug 24 2011
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

module type I =
sig
  module Move : Move.S
  type state

  val equal : state -> state -> bool
  (* val players : Move.Player.t list
     val move : state -> Move.Player.t -> Move.Action.t list
    val tab : state -> Move.t -> state
    val payoff : Move.Player.t -> state -> int*)

  val string_of_state : state -> string
  val string_of_action : Move.Action.t -> string
  val int_of_player : Move.Player.t -> int
end


module type O = 
sig
  module I : I

  module G : Game.BUCHI with type A.G.V.t = I.state
			and module A.M = I.Move
  type state = I.state

  type t = 
      {
	players : I.Move.Player.t list;
	payoff : I.Move.Player.t -> I.state -> int;
	init : I.state;
	move : I.state -> I.Move.Player.t -> I.Move.Action.t list;
	tab : I.state -> I.Move.t -> I.state
      }

  val game : t -> G.t

  module Print :
  sig
    val fprint_graph : Format.formatter -> G.A.G.t -> unit
    val output_graph : Pervasives.out_channel -> G.A.G.t -> unit
  end

  module Output :
  sig
    val output : string -> G.t -> unit
    val output_arena : string -> G.t -> unit
  end

end

module Make =
  functor (I:I) ->
struct
  module I = I
  module V = 
  struct 
    type t = I.state 
    let compare x y = compare x y 
    let equal x y = compare x y = 0
    let hash x = Hashtbl.hash x
  end

  module A = Arena.Make(V)(I.Move)
  module G = Game.MakeBuchi(A)
  module AU = Arena.Util(G.A)
  module PlayerSet = AU.PlayerSet
  module StateSet = AU.StateSet

  type state = I.state

  type t = 
      {
	players : I.Move.Player.t list;
	payoff : I.Move.Player.t -> I.state -> int;
	init : I.state;
	move : I.state -> I.Move.Player.t -> I.Move.Action.t list;
	tab : I.state -> I.Move.t -> I.state
      }

  let arena game = 
    let count = ref 1 in
    let rec step to_visit g = 
      if StateSet.is_empty to_visit then g
      else 
	let s = StateSet.choose to_visit in
	let rec add_moves (g,to_visit) m_accu = function
	  | [] -> 
	    let t = game.tab s m_accu in 
	    let tv = 
	      if A.G.mem_vertex g t
	      then to_visit
	      else (
		incr count; 
		Printf.printf "\rnumber of states : %d" !count;
		StateSet.add t to_visit)
	    in
	    let g = A.add_edge g s m_accu t in
	    g,tv

	 | player :: tl ->
	   let allowed = game.move s player in
	   List.fold_left 
	      (fun (g,to_visit) a ->
		add_moves (g,to_visit)
		  (I.Move.set_action player a m_accu) tl)
	      (g,to_visit) allowed
	in

	let g, to_visit = add_moves (g,to_visit)
	  I.Move.empty game.players
	in step (StateSet.remove s to_visit) g
    in 
    let res = step (StateSet.singleton game.init) A.G.empty in
    Printf.printf "\r                       \r";
(*    print_newline ();*)
    res

  let game des = 
    let aren = arena des in
    let game = G.make aren in
    let game = G.set_start game des.init in
    List.fold_left
      (fun g p -> 
	let o = G.make_objective (des.payoff p) in
	G.set_objective g p o) game des.players

  module Rep = Repellor.Make(G)
  module Printer = 
  struct 
    let state x = "\""^I.string_of_state x ^"\"" 
    let action = I.string_of_action
    let player = I.int_of_player
  end

  module Print = Arena.DotPrinter(A)(Printer)
  module Output = Game.Output(G)(Printer)

end


module StringMap = Map.Make
  (struct 
    type t = string 
    let compare = String.compare
   end)

module Player = 
struct 
  type t = int * string
  let cmpt = ref 0
  let tbl = Hashtbl.create 10
  let compare (i,_) (j,_) = compare i j
  let equal (i,_) (j,_) = i = j
  let hash (i,_) = i
  let of_string x = 
    try Hashtbl.find tbl x 
    with Not_found -> 
      incr cmpt; 
      Hashtbl.add tbl x (!cmpt, x);
      (!cmpt, x)
  let to_string (i,s) = s
end

module Act = Util.Int
module Move = Move.Make(Player)(Act)

module Expression =
struct 

  exception Action_not_found of Player.t
  exception Variable_not_found of string

  type value = 
    | Int of int
    | Float of float
    | Action of Player.t
    | Var of string

  let value_to_string = function
    | Int i -> string_of_int i
    | Float f -> string_of_float f
    | Var v -> v
    | Action p -> "!"^Player.to_string p

  let to_int state move = function
    | Int i -> i
    | Var v -> 
      (try StringMap.find v state
       with Not_found -> raise (Variable_not_found v))
    | Action p -> 
      (try Move.get_action p move
       with Move.Action_not_found -> raise (Action_not_found p))
    | Float f ->
      Printf.eprintf "warning float %f in integer expression\n" f;
      int_of_float f


  let to_float state move = function
    | Float f -> f
    | x -> float_of_int (to_int state move x)

     
  let is_var = function 
    | Var _ | Action _ -> true
    | _ -> false
      
  let is_int x = function
    | Int _ -> true
    | _ -> false
      
  type t = 
    | Val of value
    | Apply of string * t list

  let rec evaluate_float state move = function
    | Val v -> to_float state move v
    | Apply (f,[a;b]) -> 
      let va = evaluate_float state move a in
      let vb = evaluate_float state move b in
      let vf = match f with
	| "+" -> (+.)
	| "-" -> (-.)
	| "*" -> ( *. )
	| "/" -> ( /. )
	| "max" -> max 
	| "min" -> min 
	| x -> failwith ("unimplemented float function "^x)
      in vf va vb 
    | Apply (f,[a]) ->
      let va = evaluate_float state move a in
      let vf = match f with
	| "exp" -> exp
	| x -> failwith ("unimplemented float function "^x)
      in vf va
    | Apply (f,_) -> failwith ("unknown float function "^f)

  let rec evaluate state move = function
    | Val v -> to_int state move v
    | Apply (f,[a;b]) -> 
      let va = evaluate state move a in
      let vb = evaluate state move b in
      let vf = match f with
	| "+" -> (+)
	| "-" -> (-)
	| "*" -> ( * )
	| "/" -> ( / )
	| "max" -> max 
	| "min" -> min 
	| "and" -> ( * )
	| "or" -> (fun a b -> if a <> 0 || b <> 0 then 1 else 0)
	| "<" -> (fun a b -> if a < b then 1 else 0)
	| "<=" -> (fun a b -> if a <= b then 1 else 0)
	| ">=" -> (fun a b -> if b <= a then 1 else 0)
	| ">" -> (fun a b -> if b < a then 1 else 0)
	| "==" -> (fun a b -> if a = b then 1 else 0)
	| "!=" -> (fun a b -> if a = b then 0 else 1)
	| x -> failwith ("unimplemented int function "^x)
      in vf va vb 
    | Apply (f,[a]) ->
      let va = evaluate state move a in
      let vf = match f with
	| "exp" -> (fun a -> int_of_float (exp (float_of_int a)))
	| "not" -> (fun a -> 1 - a)
	| x -> failwith ("unimplemented int function "^x)
      in vf va
    | Apply (f,_) -> failwith ("unknown int function "^f)

end      

module Instruction =
struct

  type t =
    | While of Expression.t * t
    | If of Expression.t * t * t
    | Set of string * Expression.t
    | Seq of t list
    | Legal of string * Expression.t list

  let rec evaluate state move = function
    | While (e,i) ->
      if Expression.evaluate state move e = 0
      then state
      else 
	let new_state = evaluate state move i in
	evaluate new_state move (While (e,i))

    | If (e,x,y) ->
      if Expression.evaluate state move e = 0
      then evaluate state move y
      else evaluate state move x
    | Set (v,e) ->
      let ve = Expression.evaluate state move e in
      StringMap.add v ve state

    | Seq [] -> state
    | Seq (a::s) ->
      let ns = evaluate state move a in
      evaluate ns move (Seq s)
    | Legal _ -> state

  let legal state instr = 
    let rec aux accu state = function
      | While (e,i) ->
	let eval = 
	  (try Expression.evaluate state Move.empty e
	   with Expression.Action_not_found _ -> 0)
	in
	if eval = 0 
	then accu,state
	else 
	  let new_accu,new_state = aux accu state i in
	  aux new_accu new_state (While (e,i))


      | If (e,x,y) ->
	(try 
	   if Expression.evaluate state Move.empty e = 0
	   then aux accu state y else aux accu state x
	 with Expression.Action_not_found _ -> accu,state)


      | Set (v,e) ->
	let ve = Expression.evaluate state Move.empty e in
	accu, StringMap.add v ve state

      | Seq [] -> accu,state
      | Seq (a::s) -> 
	let new_accu,new_state = aux accu state a in
	aux new_accu new_state (Seq s)
	  
      | Legal (a,e) ->
	let ve = List.map (Expression.evaluate state Move.empty) e in 
	try 
	  let old = StringMap.find a accu in
	  StringMap.add a (List.merge compare ve old) accu, state
	with Not_found -> StringMap.add a ve accu , state

    in 

    let accu, _ = aux StringMap.empty state instr in
    accu


end


module Parser = 
struct

  module Move = Move
  type state = int StringMap.t
  let int_of_player = Player.hash
    
  let string_of_action = string_of_int
  let string_of_state s =
    StringMap.fold (fun k i s -> 
      (* ignore constants (ie identifiers starting with a captital letter*)
      if k.[0] = Char.uppercase k.[0]
      then s 
      else s ^ k ^" = "^ string_of_int i^" ; ") s ""
  let equal s t = StringMap.equal (fun i j -> i = j) s t

  let assoc_of_state s =
    StringMap.fold (fun k i s -> 
      (* ignore constants (ie identifiers starting with a captital letter*)
      if k.[0] = Char.uppercase k.[0]
      then s 
      else (k,i) :: s) s []

  let state_of_string string =
    let splited = Str.split (Str.regexp " ; ") string in
    let aux accu string = match Str.split (Str.regexp " = ") string with
      | [s;i] -> StringMap.add s (int_of_string i) accu
      | [] -> accu
      | a :: [] -> failwith ("nothing after : "^a)
      | a :: l -> 
	List.iter print_endline l;
	failwith ("problem after : "^a)
    in List.fold_left aux StringMap.empty splited
    

end


module Generic = Make(Parser)
