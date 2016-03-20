(* Parser and Printers for games and automata in the gml file format *)

let verbose = false

module OldMove = Move
module Move = Move.Make(Util.Int)(Util.String)


module Point =
struct 
  type t = {x:int; y:int }
  let x a = a.x
  let y a = a.y
  let set_x a x = {a with x = x}
  let set_y a y = {a with y = y}
  let make ~x ~y = {x=x;y=y}
  let default = make ~x:0 ~y:0

  let print p = 
    ("point",Graph.Gml.List
      ["x",Graph.Gml.Int (x p);"y",Graph.Gml.Int (y p)])

  let parse v = 
    let aux p = function 
      | ("x", Graph.Gml.Int i) -> set_x p i
      | ("y", Graph.Gml.Int i) -> set_y p i
      | (x,y) -> 
	Printf.eprintf "GML: unrecognized field %s\n" x;
	p
    in match v with 
      | ("point", Graph.Gml.List l) -> 
	List.fold_left aux default l
      | (x,y) -> 
	Printf.eprintf "GML: Warning not a point : %s\n" x;
	default
end

module type N =
sig
  type t
  val empty : unit -> t
  val id : t -> string
  val label : t -> string
  val x : t -> int
  val y : t -> int
  val w : t -> int
  val h : t -> int
  val fill : t -> string
  val _type : t -> string
  val outline : t -> string
  val compare : t -> t -> int 
  val hash : t -> int
  val equal : t -> t -> bool 
  val of_id : string -> t
  val print : t -> Graph.Gml.value_list
  val parse : Graph.Gml.value_list -> t
end

(** States with label, position and color *)
module Node = 
struct 
  type t = {id:string; label:string; x:int; y:int; w:int; h:int; fill:string; _type:string;outline:string}
  let empty () = 
    {id="";label="";x=0;y=0;w=0;h=0;
     fill="";_type="";outline=""}
  let label a = a.label
  let x a = a.x
  let y a = a.y
  let w a = a.w
  let h a = a.h
  let _type a = a._type
  let fill a = a.fill
  let id a = a.id
  let outline a = a.outline
  let set_id a i = {a with id = i}
  let set_x a x = {a with x = x}
  let set_y a y = {a with y = y}
  let set_label a l = {a with label = l}
  let set_fill a f = {a with fill = f}
  let set_type a t = {a with _type = t}
  let set_w a w = {a with w = w}
  let set_h a h = {a with h = h}
  let set_outline a o = {a with outline = o}

  let make ?(id="") ?(label="") ?(x=0) ?(y=0) ?(w=0) ?(h=0) ?(fill="") ?(_type="") ?(outline="") () =
    {id=id;label=label;x=x;y=y;w=w;h=h;fill=fill;_type=_type;outline=outline}

  let equal a b = id a = id b

  (* should we use the id or the label 
   to choose if two nodes are equals ? *)
  let compare a b = compare (id a) (id b)

  let hash a = Hashtbl.hash (id a)

  let of_id l = set_id (empty()) l

  let parse = 
    let rec aux n = function 
      | ("id", Graph.Gml.Int i) -> set_id n (string_of_int i)
      | ("id", Graph.Gml.String i) -> set_id n i
      | ("label", Graph.Gml.Int i) -> 
	set_label n (string_of_int i)
      | ("label", Graph.Gml.String i) -> set_label n i
      | ("graphics", Graph.Gml.List l) -> List.fold_left aux n l
      | ("x", Graph.Gml.Float x) -> set_x n (int_of_float x)
      | ("x", Graph.Gml.Int x) -> set_x n x
      | ("y", Graph.Gml.Float y) -> set_y n (int_of_float y)
      | ("y", Graph.Gml.Int y) -> set_y n y
      | ("w", Graph.Gml.Float w) -> set_w n (int_of_float w)
      | ("w", Graph.Gml.Int w) -> set_w n w
      | ("h", Graph.Gml.Float h) -> set_h n (int_of_float h)
      | ("h", Graph.Gml.Int h) -> set_h n h
      | ("fill", Graph.Gml.String f) -> set_fill n f
      | ("type", Graph.Gml.String t) -> set_type n t
      | ("outline", Graph.Gml.String o) -> set_outline n o
      | (x,y) -> 
	Printf.eprintf "GML: unrecognized field %s\n" x;
	n
    in List.fold_left aux (empty())

  let print n = 
    ["label",Graph.Gml.String (label n);
     "graphics",
     Graph.Gml.List
       ["x",Graph.Gml.Int (x n);
	"y",Graph.Gml.Int (y n);
	"w",Graph.Gml.Int (w n);
	"h",Graph.Gml.Int (h n);
	"fill",Graph.Gml.String (fill n);
	"type",Graph.Gml.String (_type n);
	"outline",Graph.Gml.String (outline n);
       ]]

end

module INode = 
struct 
  type t = {mutable id:string; mutable label:string;
	    mutable x:int; mutable y:int; mutable w:int;
	    mutable h:int; mutable fill:string; 
	    mutable _type:string; mutable outline:string}
  let empty () =
    {id="";label="";x=0;y=0;w=0;h=0;fill="";_type="";outline=""}
  let label a = a.label
  let x a = a.x
  let y a = a.y
  let w a = a.w
  let h a = a.h
  let _type a = a._type
  let fill a = a.fill
  let outline a = a.outline
  let id a = a.id
  let set_id a i = a.id <- i
  let set_x a x = a.x <- x
  let set_y a y = a.y <- y
  let set_label a l = a.label <- l
  let set_fill a f = a.fill <- f
  let set_type a t = a._type <- t
  let set_outline a o = a.outline <- o
  let set_w a w = a.w <- w
  let set_h a h = a.h <- h

  let make ?(id="") ?(label="") ?(x=0) ?(y=0) ?(w=0) ?(h=0)
      ?(fill="") ?(_type="") ?(outline="") () =
    {id=id;label=label;x=x;y=y;w=w;h=h;fill=fill;
     _type=_type;outline=outline}

  (* should we use the id or the label 
   to choose if two nodes are equals ? *)
  let compare a b = compare (id a) (id b)
  let hash a = Hashtbl.hash (id a)
  let equal a b = id a = id b
  let of_id l = let n = empty () in set_id n l; n

  let parse l = 
    let n = empty () in
    let rec aux = function 
      | ("id", Graph.Gml.Int i) -> set_id n (string_of_int i)
      | ("id", Graph.Gml.String i) -> set_id n i
      | ("label", Graph.Gml.Int i) -> 
	set_label n (string_of_int i)
      | ("label", Graph.Gml.String i) -> set_label n i
      | ("graphics", Graph.Gml.List l) -> List.iter aux l
      | ("x", Graph.Gml.Float x) -> set_x n (int_of_float x)
      | ("x", Graph.Gml.Int x) -> set_x n x
      | ("y", Graph.Gml.Float y) -> set_y n (int_of_float y)
      | ("y", Graph.Gml.Int y) -> set_y n y
      | ("w", Graph.Gml.Float w) -> set_w n (int_of_float w)
      | ("w", Graph.Gml.Int w) -> set_w n w
      | ("h", Graph.Gml.Float h) -> set_h n (int_of_float h)
      | ("h", Graph.Gml.Int h) -> set_h n h
      | ("fill", Graph.Gml.String f) -> set_fill n f
      | (x,y) -> 
	if verbose 
	then
	  Printf.eprintf "GML: Warning unrecognized field %s\n" x
    in List.iter aux l; n

  let print n = 
    ["label",Graph.Gml.String (label n);
     "graphics",
     Graph.Gml.List
       ["x",Graph.Gml.Int (x n);
	"y",Graph.Gml.Int (y n);
	"w",Graph.Gml.Int (w n);
	"h",Graph.Gml.Int (h n);
	"fill",Graph.Gml.String (fill n);
	"type",Graph.Gml.String (_type n);
       ]]

end

module type E =
sig
  type t 
  type node 
  val default : t
  val compare : t -> t -> int

  val source : t -> node
  val target : t -> node
  val label : t -> string
  val fill : t -> string
  val line : t -> Point.t list

  val set_source : t -> node -> t
  val set_target : t -> node -> t
  val set_label : t -> string -> t
  val set_fill : t -> string -> t
  val set_line : t -> Point.t list -> t

  val make : source:node -> target:node -> ?label:string -> 
    ?fill:string ->  ?line:(Point.t list) -> unit -> t

  val print : t -> Graph.Gml.value_list
  val parse : Graph.Gml.value_list -> t
end

module FEdge = 
  functor (N:N) ->
struct 
  type t = {source:N.t; target:N.t; 
	    label:string; fill:string; line: Point.t list}
  type node = N.t
  let label a = a.label
  let source a = a.source
  let target a = a.target
  let fill a = a.fill
  let line a = a.line
  let set_label a l = {a with label = l}
  let set_source a x = {a with source = x}
  let set_target a y = {a with target = y}
  let set_line a l = {a with line = l}
  let set_fill a f = {a with fill = f}

  let make ~source ~target ?(label="") ?(fill="") ?(line=[]) () =
    {source=source;target=target;label=label;
     fill=fill;line=line}
  let default = make ~source:(N.empty ()) 
    ~target:(N.empty ()) ()
  let compare v w = compare v w

  let print e = 
    let l = if line e = [] then [] 
      else ["Line", Graph.Gml.List 
	(List.map Point.print (line e))]
    in
    let l = if fill e = "" then l
      else ("fill", Graph.Gml.String (fill e)) :: l
    in
    let l = if l = [] then []
      else ["graphics", Graph.Gml.List l]
    in
    let l = if label e = "" then l
      else ("label", Graph.Gml.String (label e)) :: l
    in l
    
  let parse l = 
    let aux e = function
      | ("source", Graph.Gml.Int id) -> 
	set_source e (N.of_id (string_of_int id))
      | ("source", Graph.Gml.String id) -> 
	set_source e (N.of_id id)
      | ("target", Graph.Gml.Int id) -> 
	set_target e (N.of_id (string_of_int id))
      | ("target", Graph.Gml.String id) -> 
	set_target e (N.of_id id)
      | ("label", Graph.Gml.String st) -> set_label e st
      | ("fill", Graph.Gml.String st) -> set_fill e st
      | ("line", Graph.Gml.List lst) -> 
	set_line e (List.map Point.parse lst)
      | (x,y) -> 
	if verbose
	then 
	  Printf.eprintf "GML: Warning unrecognized field %s\n" x;
	e
    in List.fold_left aux default l

end

module Edge = FEdge(Node)
module IEdge = FEdge(INode)

module Persistent = 
struct
  module G = Graph.Persistent.Digraph.ConcreteLabeled(Node)(Edge)
  let node v = G.V.label v
  let edge e = 
    Edge.set_target 
      (Edge.set_source
	 (G.E.label e) 
	 (node (G.E.src e)))
      (node (G.E.dst e))
  
  module L = 
  struct
    let node = Node.print	
    let edge = Edge.print
  end
    
  module Printer = Graph.Gml.Print(G)(L)
  let print = Printer.print
    
  module P = 
  struct 
    let node = Node.parse 
    let edge = Edge.parse
  end
    
  module Builder = Graph.Builder.P(G)
  module Parser = Graph.Gml.Parse(Builder)(P)
  let parse = Parser.parse
(*
  module DP =
  struct
    let node id atr_list = 
      let id = match id with
	| (Graph.Dot_ast.Ident s,_)
	| (Graph.Dot_ast.Number s,_)
	| (Graph.Dot_ast.String s,_)
	| (Graph.Dot_ast.Html s,_) -> s
      in 
      let n = Node.of_id id in
      let rec aux n = function
	| [] -> n
	| (Graph.Dot_ast.String "label", 
	   Some (Graph.Dot_ast.String l)) :: s 
	| (Graph.Dot_ast.String "label", 
	   Some (Graph.Dot_ast.Number l)) :: s 
	  -> aux (Node.set_label n l) s
	| _ :: s -> aux n s
      in
    List.fold_left aux n atr_list
      
    let rec edge = 
      let rec aux m = function
	| [] -> m
	| (Graph.Dot_ast.String "label", 
	   Some(Graph.Dot_ast.String st) ) :: s -> 
	  aux (parse_label st) s
	| _ :: s -> aux m s
      in List.fold_left aux A.M.empty 
  end

  module DotParser = Graph.Dot.Parse(Builder)(DP)
*)  
  let parse_dot s = failwith "parse_dot: unimplemented" 
(*DotParser.parse*)

end

module Imperative = 
struct
  module G = 
    Graph.Imperative.Digraph.ConcreteLabeled(INode)(IEdge)
  let node v = G.V.label v
  let edge e = 
    IEdge.set_target 
      (IEdge.set_source
	 (G.E.label e) 
	 (node (G.E.src e)))
      (node (G.E.dst e))
  
  let of_edge e = G.E.create (IEdge.source e) e (IEdge.target e)

  let of_node v = G.V.create v

  let copy g h =
    G.clear h;
    G.iter_vertex (G.add_vertex h) g;
    G.iter_edges_e (G.add_edge_e h) g

  module L = 
  struct
    let node = INode.print	
    let edge = IEdge.print
  end
    
  module Printer = Graph.Gml.Print(G)(L)
  let print = Printer.print
    
  module P = 
  struct 
    let node = INode.parse 
    let edge = IEdge.parse
  end
    
  module Builder = Graph.Builder.I(G)
  module Parser = Graph.Gml.Parse(Builder)(P)
  let parse = Parser.parse
  let parse_dot s = failwith "parse_dot unimplemented"

end

module GmlArena =
struct 
  module A = Arena.Make(Node)(Move)
  module L = 
  struct
    module MUtil = OldMove.Util(A.M)
    let node = Node.print	
    let edge l =
      (*let actions = A.M.to_actions (A.move l)
      in 
      let sorted = 
	List.map snd
	  (List.sort (fun (a,b) (c,d) -> A.M.Player.compare a c) actions)
      in*)
      ["label",
       Graph.Gml.String (MUtil.to_string (fun s -> s) (A.move l))] 
	 (*List.fold_left
	    (fun accu j -> 
	      (if accu = "" then "" else (accu^",")) ^ (*string_of_int*) j
	    ) "" sorted )]*)
  end
    
  module Printer = Graph.Gml.Print(A.G)(L)
  let print = Printer.print
    
  module P = 
  struct 
    let node = Node.parse 
      
    let parse_label s =
      let l = Str.split (Str.regexp ",") s in
      let j,res =
	List.fold_left 
	  (fun (j,lab) i -> 
	  j+1 , A.M.set_action j ((*int_of_string*) i) lab) 
	  (1, A.M.empty) l
      in res

    let edge = 
      let rec aux j = function
	| [] -> A.M.empty
	| ("label", Graph.Gml.String st) :: s ->
	  parse_label st
	| ("action", Graph.Gml.Int i) :: s ->
	  let l = aux (j+1) s in A.M.set_action j (string_of_int i) l
	| _ :: s -> aux j s
      in aux 1
  end
  module Builder = Graph.Builder.P(A.G)
  module Parser = Graph.Gml.Parse(Builder)(P)
  let parse file = 
    try Parser.parse file
    with Invalid_argument s -> 
      let inch = open_in file in
      let rec look_for_graph () = 
	try
	  let line = input_line inch in
	  try 
	    let _ = Str.search_forward (Str.regexp "graph.*$") line 0 
	    in Str.matched_string line
	  with Not_found -> look_for_graph ()
	with End_of_file -> raise Not_found
      in 
      let first = look_for_graph () in
      let tmp_file,outch = Filename.open_temp_file (Filename.basename file) "" in
      print_endline ("writing in temporary file "^tmp_file);
      Printf.fprintf outch "%s\n" first;
      let buf = String.create 256 in
      let rec write () =
	let nb_read = input inch buf 0 256 in
	output outch buf 0 nb_read;
	if nb_read <> 0 then write ()
      in 
      write ();
      close_in inch;
      close_out outch;
       Parser.parse tmp_file

  module DP =
  struct
    let node id atr_list = 
      let id = match id with
	| (Graph.Dot_ast.Ident s,_)
	| (Graph.Dot_ast.Number s,_)
	| (Graph.Dot_ast.String s,_)
	| (Graph.Dot_ast.Html s,_) -> s
      in 
      let n = Node.of_id id in
      let rec aux n = function
	| [] -> n
	| (Graph.Dot_ast.String "label", 
	   Some (Graph.Dot_ast.String l)) :: s 
	| (Graph.Dot_ast.String "label", 
	   Some (Graph.Dot_ast.Number l)) :: s 
	  -> aux (Node.set_label n l) s
	| _ :: s -> aux n s
      in
    List.fold_left aux n atr_list
      
    let parse_label s =
      let l = Str.split (Str.regexp ",") s in
      let j,res =
	List.fold_left 
	  (fun (j,lab) i -> 
	    j+1 , A.M.set_action j ((*int_of_string*) i) lab) 
	  (1,A.M.empty) l
      in res
      
    let rec edge = 
      let rec aux m = function
	| [] -> m
	| (Graph.Dot_ast.Ident "label", 
	   Some(Graph.Dot_ast.String st) ) :: s -> 
	  aux (parse_label st) s
	| _ :: s -> aux m s
      in List.fold_left aux A.M.empty 
  end

  module DotParser = Graph.Dot.Parse(Builder)(DP)
  let parse_dot = DotParser.parse

end


module GmlGame = Game.Make(GmlArena.A)


