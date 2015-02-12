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

(** {2 Header} *)

val head : elt_fun
val meta : elt_fun
val title : elt_fun
val base : elt_fun
val link : elt_fun
val style : elt_fun

(** {2 Body and sectionning} *)

val body : elt_fun
val article : elt_fun
val section : elt_fun
val nav : elt_fun
val aside : elt_fun
val h1 : elt_fun
val h2 : elt_fun
val h3 : elt_fun
val h4 : elt_fun
val h5 : elt_fun
val h6 : elt_fun
val header : elt_fun
val footer : elt_fun
val address : elt_fun

(** {2 Blocks} *)

val p : elt_fun
val hr : Xtmpl.tree
val pre : elt_fun
val blockquote : elt_fun
val ol : elt_fun
val ul : elt_fun
val li : elt_fun
val dl : elt_fun
val dt : elt_fun
val dd : elt_fun
val figure : elt_fun
val figcaption : elt_fun
val div : elt_fun
val main : elt_fun

(** {2 Text elements} *)

val a : elt_fun
val em : elt_fun
val strong : elt_fun
val small : elt_fun
val s : elt_fun
val cite : elt_fun
val q : elt_fun
val dfn : elt_fun
val abbr : elt_fun
val data : elt_fun
val time : elt_fun
val code : elt_fun
val var : elt_fun
val samp : elt_fun
val kbd : elt_fun
val sub : elt_fun
val sup : elt_fun
val i : elt_fun
val b : elt_fun
val u : elt_fun
val mark : elt_fun
val ruby : elt_fun
val rb : elt_fun
val rt : elt_fun
val rtc : elt_fun
val rp : elt_fun
val bdi : elt_fun
val bdo : elt_fun
val span : elt_fun
val br : Xtmpl.tree
val wbr : Xtmpl.tree

(** {2 Edits} *)

val ins : elt_fun
val del : elt_fun

(** {2 Embedding resources} *)

val embed : elt_fun
val object_ : elt_fun
val img : elt_fun
val audio : elt_fun
val video : elt_fun
val source : elt_fun

(** {2 Tables} *)

val table : elt_fun
val caption : elt_fun
val colgroup : elt_fun
val col : elt_fun
val tbody : elt_fun
val thead : elt_fun
val tfoot : elt_fun
val tr : elt_fun
val td : elt_fun
val th : elt_fun

(** {2 Forms} *)

val form : elt_fun
val label : elt_fun
val input : elt_fun
val button : elt_fun
val select : elt_fun
val datalist : elt_fun
val optgroup : elt_fun
val option : elt_fun
val textarea : elt_fun
val keygen : elt_fun
val output : elt_fun
val progress : elt_fun
val meter : elt_fun
val fieldset : elt_fun
val legend : elt_fun
