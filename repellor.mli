(*
 * Praline
 * Copyright 2011 LSV, CNRS & ENS de Cachan, France
 * Author: Romain Brenguier <brenguier@lsv.ens-cachan.fr>
 * File: repellor.mli
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

(** Implementation of the repellor algorithm for Buchi games. *)
module Make :
  functor (G : Game.BUCHI) -> 
sig

  (** constraints on the payoff *)
  type constr
  val empty_constr : constr 
  val make_constr : G.t -> (G.A.M.Player.t -> int) -> constr


  type solution 

  (** The shape gives indication on how the game behaves when 
      no one deviates *)
  type shape
  val shapes : solution -> shape list
  val payoff_of_shape : shape -> Payoff.Make(G.A.M.Player).t 
  val arena_of_shape : shape -> G.A.t
  val scc_of_shape : shape -> G.A.t

  type repellors = ((*Arena.Util(G.A).PlayerSet.t*)G.A.M.Player.t list  * int * Payoff.Make(G.A.M.Player).t, G.A.t) Hashtbl.t
  val repellors : solution -> repellors

  (** Computes some possible equilibria plays for the game with Buchi objectives.
      For each equilibria in the game the algorithms gives at least one solution
      which includes the set of winners. *)
  val buchi_equilibria : ?constr:constr -> G.t -> solution

end 
