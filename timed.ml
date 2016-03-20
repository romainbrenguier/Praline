module Clock =
struct
  type t = string
  let of_string x = x
  let to_string x = x
  let compare = String.compare
  let equal a b = compare a b = 0

end

module ClockSet = Set.Make(Clock)

module Constraint =
struct
  type t = 
    | Equal of Clock.t * int
    | Smaller of Clock.t * int
    | SmallerEq of Clock.t * int
    | Greater of Clock.t * int
    | GreaterEq of Clock.t * int
      
  let equal x y = Equal (x,y)
  let smaller x y = Smaller (x,y)
  let smaller_eq x y = SmallerEq (x,y)
  let greater x y = Greater (x,y)
  let greater_eq x y = GreaterEq (x,y)

  let compare x y = compare x y

  let to_string c = 
    let x,rel,i = match c with
      | Equal (x,i) -> x,"=",i
      | Smaller (x,i) -> x,"<",i
      | SmallerEq (x,i) -> x,"<=",i
      | Greater (x,i) -> x,">",i
      | GreaterEq (x,i) -> x,">=",i
    in
    Printf.sprintf "%s %s %d" (Clock.to_string x) rel i
end

module Region =
struct

  module I = 
  struct
    type t = Clock.t option * Clock.t option
    let i = fst
    let j = snd
    let make i j = (i,j)
    let compare x y = compare x y
  end

  (* Difference bounded matrice *)
  module Mat = Map.Make(I)

  module Bound = 
  struct
    type t = 
      | Strict of int
      | Equal of int 
      | Infinity

    let zero = Equal 0
    let infinity = Infinity
  end 

  type t = { m: int (* maximal constant *); 
	     bounds : Bound.t Mat.t;
	     (* x_i - x_j > m.(i).(j) *)
	     clocks : ClockSet.t
	   }

  let clocks r = r.clocks
  let m r = r.m
  let bounds (r:t) = r.bounds

  let compare (a:t) (b:t) = 
    Mat.compare compare (bounds a) (bounds b)

  let equal a b = compare a b = 0

  let initial clock_set m = 
    let mat = Mat.empty in
    let mat = Mat.add (I.make None None) Bound.zero mat in
    let aux clock1 mat = 
      let clock1 = Some clock1 in
      let mat = Mat.add (I.make None clock1) Bound.zero mat in
      let mat = Mat.add (I.make clock1 None) Bound.zero mat in
      ClockSet.fold
	(fun clock2 mat -> 
         Mat.add (I.make clock1 (Some clock2)) Bound.zero mat
	)
	clock_set mat 
    in 
    {m=m; bounds=ClockSet.fold aux clock_set mat; clocks=clock_set }

  let hash (a:t) = 
    let i = initial (clocks a) (m a) in
    compare i a


  let empty clock_set m = 
    let mat = Mat.empty in
    let mat = Mat.add (I.make None None) Bound.infinity mat in
    let aux clock1 mat = 
      let clock1 = Some clock1 in
      let mat = 
	Mat.add (I.make None clock1) Bound.infinity mat in
      let mat = 
	Mat.add (I.make clock1 None) Bound.infinity mat in
      ClockSet.fold
	(fun clock2 mat -> 
	  Mat.add (I.make clock1 (Some clock2)) Bound.infinity mat
	)
	clock_set mat
    in 
    {m=m; bounds=ClockSet.fold aux clock_set mat; clocks=clock_set }
      
  let set_bound region clock1 clock2 bound = 
    {region with 
      bounds=Mat.add (I.make (Some clock1) (Some clock2)) 
	bound region.bounds}

  let integer_value region clock = 
    match Mat.find (I.make None (Some clock)) region.bounds with
      | Bound.Equal x | Bound.Strict x  -> -x
      | Bound.Infinity -> region.m

  let is_integer region clock = 
    match Mat.find (I.make None (Some clock)) region.bounds with
      | Bound.Equal x -> true
      | _ -> false

  (** compare the fractionnal part of two clock *)
  let compare_fract region clock1 clock2 = 
    match Mat.find (I.make (Some clock1) (Some clock2)) region.bounds with
      | Bound.Equal x -> 0
      | Bound.Strict x -> 
	let d = x - integer_value region clock1 
	  + integer_value region clock2 in
	if d = 0 then -1 
	else if d = 1 then 1
	else failwith "error in compare_fract"
      | Bound.Infinity -> failwith "error : infinity in dbm"

  let next (region:t) = 
    let mat = region.bounds in
    let modif = ref false in
    let aux key value = match I.i key, I.j key with
      | None, Some clock1 -> 
	(match value with
	  | Bound.Equal x -> modif := true; 
	    Bound.Strict x 
	  | _ -> value)
      | Some clock1, None -> 
	(match value with
	  | Bound.Equal x -> modif := true; 
	    Bound.Strict (x + 1) 
	  | _ -> value)
      | _, _ -> value
    in 
    let mat = Mat.mapi aux mat in
    if !modif then {region with bounds = mat }
    else 
      let maxima = 
	ClockSet.fold 
	  (fun clock maxima -> 
	    try let elt = ClockSet.choose maxima in
		let cmp = compare_fract region clock elt in
		if cmp > 0 then ClockSet.singleton clock
		else if cmp = 0
		then ClockSet.add clock maxima
		else maxima
	    with Not_found -> ClockSet.singleton clock
	  ) (clocks region) ClockSet.empty 
      in
      let aux key value = match I.i key, I.j key with
	| None, Some clock1 ->
	  if ClockSet.mem clock1 maxima
	  then
	    (match value with
	      | Bound.Strict x -> Bound.Equal (x-1)
	      | Bound.Equal x -> 
		failwith "this should not happen ?"
	      | _ -> value)
	  else
	    (match value with
	      | Bound.Equal x -> Bound.Strict x
	      | _ -> value)

	| Some clock1, None  -> 
	  if ClockSet.mem clock1 maxima
	  then
	    (match value with
	      | Bound.Strict x -> Bound.Equal (x+1)
	      | Bound.Equal x -> 
		failwith "this should not happen ?"
	      | _ -> value)
	  else
	    (match value with
	      | Bound.Equal x -> Bound.Strict (x+1)
	      | _ -> value)

	| _, _ -> value
      in 
      let mat = Mat.mapi aux mat in
      {region with bounds = mat }


	 
  let resets region clocks = 
    let aux key value = match I.i key, I.j key with
      | None, Some clock1
	when ClockSet.mem clock1 clocks -> Bound.Equal 0
      | Some clock1, None 
	when ClockSet.mem clock1 clocks -> Bound.Equal 0
      | Some clock1, Some clock2
	when ClockSet.mem clock1 clocks 
	  && ClockSet.mem clock2 clocks -> Bound.Equal 0
      | Some clock1, Some clock2
	when ClockSet.mem clock1 clocks -> 
	Mat.find (I.make None (Some clock2)) region.bounds
      | Some clock1, Some clock2
	when ClockSet.mem clock2 clocks -> 
	Mat.find (I.make (Some clock1) None) region.bounds
      | _, _ -> value
    in 
    let mat = Mat.mapi aux (bounds region) in
    {region with bounds = mat}

  open Constraint

  let satisfies_constraints l r = 
    let equal x i = 
      is_integer r x && integer_value r x = i
    in
    let aux = function 
      | Equal (x,i) -> equal x i
      | Smaller (x,i) -> integer_value r x < i
      | GreaterEq (x,i) -> integer_value r x >= i
      | SmallerEq (x,i) ->
	integer_value r x < i || equal x i
      | Greater (x,i) -> 
	integer_value r x >= i && not (equal x i)
    in List.for_all aux l

  let fprint out region =
    ClockSet.iter 
      (fun clock -> Printf.fprintf out "%s %s %d\n" 
	(Clock.to_string clock)
	(if is_integer region clock then "=" else ">")
	(integer_value region clock)) region.clocks


end

let test () = 
  let [x;y;z] = List.map Clock.of_string ["x"; "y"; "z"] in
  let clocks = ClockSet.add x (ClockSet.add y (ClockSet.singleton z)) in

  let r = Region.initial clocks 5 in
  print_endline "----------";
  Region.fprint stdout r;

  let r = Region.next r in
  print_endline "----------";
  Region.fprint stdout r;

  let r = Region.resets r (ClockSet.singleton x) in
  print_endline "---------- reset x";
  Region.fprint stdout r;

  let r = Region.next r in
  print_endline "----------";
  Region.fprint stdout r;

  let r = Region.next r in
  print_endline "----------";
  Region.fprint stdout r;

  let r = Region.resets r (ClockSet.singleton y) in
  print_endline "---------- reset y";
  Region.fprint stdout r;

  let r = Region.next r in
  print_endline "----------";
  Region.fprint stdout r;

  let r = Region.next r in
  let r = Region.next r in
  let r = Region.next r in
  let r = Region.next r in
  let r = Region.next r in
  let r = Region.next r in
  let r = Region.next r in  
  let r = Region.next r in
  print_endline "----------";
  Region.fprint stdout r;

  let c = Constraint.greater (Clock.of_string "x") 3 in
  Printf.printf "%s : %b\n"   (Constraint.to_string c)
    (Region.satisfies_constraints [c] r);

  r


module Game =
  functor (S:Graph.Sig.COMPARABLE) ->
    functor (Player:Graph.Sig.COMPARABLE) ->
struct
  
  module Label = 
  struct 
    type t = {player: Player.t option; 
	      constr: Constraint.t list;
	      reset: ClockSet.t}

    let player l = match l.player with
      | Some p -> p
      | None -> failwith "owner not set for this edge"

    let constr l = l.constr
    let reset l = l.reset
    let set_player l p = {l with player=Some p}
    let set_constr l c = {l with constr=c}
    let set_reset l c = {l with reset=c}

    let make ~player ~constr ~reset = 
      {player=Some player; constr=constr; reset=reset}

    let compare l m =
      let c1 = Player.compare (player l) (player m) in
      if c1 <> 0 then c1
      else
	let c2 = Constraint.compare (constr l) (constr m) in
	if c2 <> 0 then c2
	else ClockSet.compare (reset l) (reset m)

    let default = 
      {player =  None; constr = []; reset = ClockSet.empty }

    let of_string = failwith "unimplemented"
  end  

  module G = Graph.Persistent.Digraph.ConcreteLabeled(S)(Label)

  (* Region abstraction *)
  module RAct = 
  struct 
    type t = Region.t * G.E.t
  end 

  module RV =
  struct 
    type t = S.t * Region.t
    let compare (s,r) (u,t) = match S.compare s u with
      | 0 -> Region.compare r t
      | x -> x
    let equal x y = compare x y = 0

    let hash (s,r) = Hashtbl.hash (S.hash s, Region.hash r)
  end 

  module RMove = Move.Make(Player)(RAct)
  module RArena = Arena.Make(RV)(RMove)

  let region_game g = 
    let region_arena = RArena.G.empty in
    region_arena

end
