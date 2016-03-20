(** Functor to compute the product of a game with 
    several deterministic automata *)


module Make :
  functor (A : Arena.S) -> 
    functor (D: Auto.DET
	     with type Alphabet.elt = A.G.V.t) ->
      functor (I: Map.OrderedType) ->
sig

  (** input module for arena. *)
  module A : Arena.S

  (** input module for deterministic automata. *)
  module D : Auto.DET with type Alphabet.elt = A.G.V.t

  (** input module for indexing the components *)
  module I : Map.OrderedType

  (** output module for the product *)
  module P : Arena.S

  (** Sets of vertices *)
  module States : Set.S with type elt = P.G.V.t

  (** Initialisation of the product arena from a basic arena *)
  val of_graph : A.t -> P.t

  (** Initialize a state of the product from a state of the game *)
  val inj : A.G.V.t -> P.G.V.t

  (** Product of a game configuration with a state of an automaton.
      This element will be indexed by an element of type [I.t] *)
  val state_product : P.G.V.t -> I.t -> D.States.elt -> P.G.V.t

  (** Product of a graph with an automaton reading 
      the nodes of the graph *)
  val product : P.t -> I.t -> D.t -> P.t

  (** Back projection *)
  val projection : P.t -> A.t

  (** Image of states of the automaton indexed by [i] in the product *)
  val image : P.t -> I.t -> D.States.t -> States.t

  (** Get the element of the state indexed by the given [i] *)
  val get : States.elt -> I.t -> D.States.elt
    
  (** Project one state *)
  val proj : P.G.V.t -> A.G.V.t

  (** Given a function that associate a string to a configuration 
      of the game and to a state of the automata, gives you functions
      to print the product graph *) 
  module DotPrinter :
    functor (Pr:
      sig 
	val conf : A.G.V.t -> string
	val state : D.States.elt -> string
	val action : A.M.Action.t -> string
      end ) ->
  sig
    
    val string_of_state : P.G.V.t -> string

    (** fprint_graph ppf graph pretty prints the graph graph in the CGL language on the formatter ppf.*)
    val fprint_graph : Format.formatter -> P.G.t -> unit
      
    (** output_graph oc graph pretty prints the graph graph in the dot language on the channel oc.*)
    val output_graph : Pervasives.out_channel -> P.G.t -> unit
  end

end
  with module A = A
  and module D = D
  and module I = I
  and module P.M = A.M
