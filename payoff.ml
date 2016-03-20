(*
 * Praline
 * Copyright 2011 LSV, CNRS & ENS de Cachan, France
 * Author: Romain Brenguier <brenguier@lsv.ens-cachan.fr>
 * File: payoff.ml
 * Created: Fri Sep 23 2011
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

module type S =
sig 
  type t 
  module Player:COMPARABLE
  val default : t
  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
  val set : Player.t -> int -> t -> t
  val get : Player.t -> t -> int
  val to_list : t -> (Player.t * int) list
  val of_list : (Player.t * int) list -> t
end

module Make =
  functor (P:COMPARABLE) ->
struct
  module Player = P
  module PlayerMap = Map.Make(Player)

  type t = int PlayerMap.t
  let empty = PlayerMap.empty
  let default = empty
  let compare (x:t) (y:t) = PlayerMap.compare compare

  let set p a m = PlayerMap.add p a m
  let get p m = 
    try PlayerMap.find p m
    with Not_found -> 0

  let to_list pay =
    PlayerMap.fold
      (fun i j accu -> if j = 0 then accu else (i,j) :: accu) pay []

  let of_list = 
    let rec aux accu = function
      | [] -> accu
      | (p,a) :: s -> aux (set p a accu) s
    in aux default 

  let to_actions mov = 
    PlayerMap.fold (fun i j accu -> (i,j) :: accu) mov []

  let hash p = Hashtbl.hash (to_list p)
  let compare p q = hash p - hash q
  let equal p q = compare p q = 0

end




 
