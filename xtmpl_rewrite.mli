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

module Xml = Xtmpl_xml
type name = string * string

type attributes = tree list Xml.Name_map.t
and node = { loc: Xml.loc option; name: name ; atts: attributes ; subs: tree list }
and tree =
| E of node
| D of Xml.cdata
| C of Xml.comment
| PI of Xml.proc_inst
| X of Xml.xml_decl
| DT of Xml.doctype

val atts_empty : attributes

val node : ?loc:Xml.loc -> name -> ?atts:attributes -> tree list -> tree
val cdata : ?loc:Xml.loc -> ?quoted:bool -> string -> tree
val comment : ?loc:Xml.loc -> string -> tree
val proc_inst : ?loc:Xml.loc -> name -> string -> tree
val xml_decl : ?loc:Xml.loc -> Xml.attributes -> tree
val doctype : ?loc:Xml.loc -> name -> string -> tree


type rewrite_stack = (name * attributes * tree list) list

type error =
  Loop of rewrite_stack
| Parse_error of Xml.loc

exception Error of error
val loop_error : rewrite_stack -> 'a
val parse_error : Xml.loc -> 'a

val string_of_error : error -> string


val from_xml : Xml.tree list -> tree list

