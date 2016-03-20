
module Expr = 
struct
  type t = Atom of string * int | Or of t list | And of t list

  let rec to_string = function
    | Atom (s,i) -> s^" == "^string_of_int i
    | Or l -> "("^List.fold_left (fun a b -> a^" || "^to_string b) (to_string (List.hd l)) (List.tl l)^")"
    | And l -> "("^List.fold_left (fun a b -> a^" && "^to_string b) (to_string (List.hd l)) (List.tl l)^")"

  let rec map f = function 
    | Atom (s,i) -> let a,b = f(s,i) in Atom (a,b)
    | Or l -> Or (List.map (map f) l)
    | And l -> And (List.map (map f) l)

  let rec find x = function
    | Atom (y,i) when x = y -> Some i
    | And l -> 
       let rec aux accu = function
	 | [] -> accu 
	 | hd :: tl -> match find x hd with None -> aux accu tl | y -> y
       in aux None l
    | _ -> None 

  let disj a b = if a = b then a else Or [a;b]

  let compare a b = 
    let rec aux = function
      | Or a , Or b 
      | And a, And b -> 
	 let c = compare (List.length a) (List.length b) in 
	 if c <> 0 then c
	 else
	   let rec loop = function
	     | [],[] -> 0
	     | (hda :: tla, hdb :: tlb) -> 
		let c = aux (hda,hdb) in
		if c <> 0 then c else loop (tla,tlb)
	     | _ -> failwith "length of lists are not matching"
	   in loop (a,b)
      | Atom (s,i), Atom(t,j) -> let c = compare s t in if c = 0 then compare i j else c
      | Atom _, _ -> 1
      | Or _, _ -> 1
      | _, _ -> -1
    in aux (a,b)

  let insert l a =
    let rec aux accu a = function
      | [] -> List.rev_append accu [a]
      | hd :: tl when compare hd a < 0 -> aux (hd :: accu) a tl
      | hd :: tl when compare hd a = 0 -> List.rev_append accu (hd :: tl)
      | hd :: tl when compare hd a > 0 -> List.rev_append accu (a :: hd :: tl)
      | _ -> failwith "implementation of the compare function is not correct"
    in aux [] a l

  let sort l = List.fold_left insert [] l

  let unfold_and x = 
    let rec aux accu = function
      | And l -> List.fold_left aux accu l
      | x -> x :: accu
    in match aux [] x with [a] -> a | list -> And list
    
  let conj a b = match unfold_and a, unfold_and b with 
    | And l, And m -> And (List.fold_left insert (sort l) m)
    | And l, m | m, And l -> And (insert l m)
    | a,b -> And [a;b]
		 

  let simplify = function 
    | And l -> And (sort l)
    | x -> x    
end

module Assign = 
struct 
  type t = (string * int) list 
  let to_string a = 
    List.fold_left (fun a (s,i) -> a^s^" <- "^string_of_int i^";") "" a

  let map f = List.map f

  let compare a b = 
    let rec loop = function
      | [],[] -> 0
      | _, [] -> 1
      | [], _ -> -1
      | ((sa,ia) :: tla, (sb,ib) :: tlb) -> 
	 let c = compare (sa,ia) (sb,ib) in
	 if c <> 0 then c else loop (tla,tlb)
    in loop (a,b)
    
end


type t = (Expr.t * Assign.t) list

let to_string c = 
  let buf = Buffer.create 500 in
  let rec aux = function 
    | (e,a) :: [] -> Printf.bprintf buf "else {%s};\n" (Assign.to_string a);
    | (e,a) :: s -> Printf.bprintf buf "if(%s){%s};\n" (Expr.to_string e) (Assign.to_string a); aux s
    | [] -> failwith "empty expression in CodeGenerator.to_string"
  in 
  aux c;
  Buffer.contents buf


let simplify_ite expr assign =
  (* keep only useful updates *)
  Expr.simplify expr, 
  List.fold_left 
    (fun accu (x,i) -> 
     match Expr.find x expr with 
     | Some j when i = j -> accu
     | _ -> (x,i) :: accu
    ) [] assign

let simplify = List.map (fun (x,y) -> simplify_ite x y)

let add_ite expr_assign_list ite = 
  let e,a = ite in
  let rec aux accu = function
    | [] -> List.rev_append accu [ite]
    | (f,b) :: tl when Assign.compare b a < 0 -> aux ((f,b) :: accu) tl
    | (f,b) :: tl when Assign.compare b a = 0 -> List.rev_append accu ((Expr.disj e f,a) :: tl)
    | (f,b) :: tl when Assign.compare b a > 0 -> List.rev_append accu ((e,a) :: (f,b) :: tl)
    | _ -> failwith "implementation of the compare function is not correct"
  in aux [] expr_assign_list

(* merge instructions that do the same assign *)
let merge_assign t =
  List.fold_left add_ite [] t

(* remove empty assignements *)
let remove_empty t =
  List.fold_left (fun accu (a,b) -> if b = [] then accu else (a,b) :: accu) [] t

  
let make t = 
  to_string (remove_empty (merge_assign (simplify t)))

(*let test = 
  let s = to_string (simplify [Atom ("i",1), [("j",2)]; Atom ("j",1), [("j",2)]; ]) in
  print_endline s*)
