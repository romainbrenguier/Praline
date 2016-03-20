(*
 * Praline
 * Copyright 2011 LSV, CNRS & ENS de Cachan, France
 * Author: Romain Brenguier <brenguier@lsv.ens-cachan.fr>
 * File: move.ml
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
  module Action:SIMPLE
  module Player:COMPARABLE
  exception Action_not_found
  val empty : t
  val default : t
  val compare : t -> t -> int
  val set_action : Player.t -> Action.t -> t -> t
  val get_action : Player.t -> t -> Action.t
  val fold : (Player.t -> Action.t -> 'a -> 'a) -> t -> 'a -> 'a
end


module Make =
  functor (P:COMPARABLE) ->
    functor (A:COMPARABLE) ->
struct
  module Action = A
  module Player = P
  module PlayerMap = Map.Make(Player)


  type t = Action.t PlayerMap.t
  let empty = PlayerMap.empty
  let default = empty
  let compare = PlayerMap.compare Action.compare

  exception Action_not_found

  let set_action p a m = PlayerMap.add p a m
  let get_action p m = 
    try PlayerMap.find p m
    with Not_found -> raise Action_not_found

  let fold f mov a = PlayerMap.fold f mov a
end

module Util =
  functor (M:S) ->
struct

  let to_actions mov = 
    M.fold (fun i j accu -> (i,j) :: accu) mov []

  let rec move_of_actions = function
    | [] -> M.empty
    | (p,a) :: s -> M.set_action p a (move_of_actions s)

  let to_string of_action move = 
    M.fold
      (fun player action accu -> 
	(if accu = "" then "" else (accu^",")) ^ of_action action)
      move ""
end
