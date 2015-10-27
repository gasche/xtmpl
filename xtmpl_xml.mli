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
val string_of_name : string * string -> string
val name_of_string : string -> string * string

type loc = { line: int ; char: int ; len: int ; file: string option }
val string_of_loc : loc -> string

type pos = { pline: int; pbol: int; pchar: int; pfile: string option }

type error = loc * string
exception Error of error
val error : loc -> string -> 'a
val string_of_error : loc * string -> string

module Name_ord : Map.OrderedType with type t = name
module Name_map : Map.S with type key = name
module Name_set : Set.S with type elt = name

type cdata = { loc: loc option; text: string; quoted: bool }
type comment = { loc: loc option; comment: string }
type proc_inst = { loc: loc option; app: name; args: string}
type attributes = (string * loc option) Name_map.t
type xml_decl = { loc: loc option; atts: attributes }
type doctype = { loc: loc option; name: name; args: string}
type node = { loc: loc option; name: name ; atts: attributes ; subs: tree list }
and tree =
| E of node
| D of cdata
| C of comment
| PI of proc_inst
| X of xml_decl
| DT of doctype

val atts_empty : attributes
val node : ?loc:loc -> name -> ?atts:attributes -> tree list -> tree
val cdata : ?loc:loc -> ?quoted:bool -> string -> tree
val comment : ?loc:loc -> string -> tree
val proc_inst : ?loc:loc -> name -> string -> tree
val xml_decl : ?loc:loc -> attributes -> tree
val doctype : ?loc:loc -> name -> string -> tree

val unescape : ?entities:bool -> string -> string
val escape : ?quotes:bool -> string -> string

val from_string : ?pos_start:pos -> string -> tree list
val from_channel : ?pos_start:pos -> in_channel -> tree list
val from_file : string -> tree list

val to_string : tree list -> string
