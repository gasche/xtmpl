(************************************************************************************)
(*                Xtmpl                                                             *)
(*                                                                                  *)
(*    Copyright (C) 2012-2015 Institut National de Recherche en Informatique        *)
(*    et en Automatique. All rights reserved.                                       *)
(*                                                                                  *)
(*    This program is free software; you can redistribute it and/or modify          *)
(*    it under the terms of the GNU Lesser General Public License version           *)
(*    3 as published by the Free Software Foundation.                               *)
(*                                                                                  *)
(*    This program is distributed in the hope that it will be useful,               *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of                *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                 *)
(*    GNU Library General Public License for more details.                          *)
(*                                                                                  *)
(*    You should have received a copy of the GNU Lesser General Public              *)
(*    License along with this program; if not, write to the Free Software           *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                      *)
(*    02111-1307  USA                                                               *)
(*                                                                                  *)
(*    Contact: Maxence.Guesdon@inria.fr                                             *)
(*                                                                                  *)
(*                                                                                  *)
(************************************************************************************)

(** *)
type name = string * string

type loc = { line: int ; char: int ; len: int ; file: string option }

module Name_ord : Map.OrderedType with type t = name
module Name_map : Map.S with type key = name
module Name_set : Set.S with type elt = name

type cdata = { loc: loc option; text: string; quoted: bool }
type comment = { loc: loc option; comment: string }
type attributes = (string * loc option) Name_map.t
type node = { loc: loc option; name: name ; atts: attributes ; subs: tree list }
and tree =
| E of node
| D of cdata
| C of comment