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

(** Creating XHTML nodes. *)

open Xtmpl

type elt_fun =
  ?xid: tree list -> ?id: string ->
  ?xclass: tree list -> ?class_: string -> ?classes: string list ->
  ?xhref: tree list -> ?href: string ->
  ?xtitle: tree list -> ?title: string ->
  ?xtype: tree list -> ?type_: string ->
  ?xname: tree list -> ?name: string ->
  ?atts: attributes -> tree list -> tree

type elt_fun_ =
  ?prefix: string -> string -> elt_fun

val elt : elt_fun_

val html : elt_fun

  