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

(** Regular XML trees *)

(** {2 Locations} *)

(** A position in a source (file, string, ...). *)
type pos = { line: int; bol: int; char: int; file: string option }

(** A location is a range defined by two positions.*)
type loc = { loc_start: pos; loc_stop: pos}
val string_of_loc : loc -> string
val loc_sprintf : loc option -> ('a, unit, string) format -> 'a

type 'a with_loc = 'a * loc option

(** [loc loc_start loc_stop] creates a {!loc} structure with
  the given positions. *)
val loc : pos -> pos -> loc

(** {2 Errors} *)

type error = loc * string
exception Error of error
val error : loc -> string -> 'a
val string_of_error : loc * string -> string

(** {2 Names} *)

type name = string * string
val string_of_name : string * string -> string

(** [name_of_string str] cuts a name according to first ':' character.
     If no such character is found, return [("",str)]. *)
val name_of_string : string -> string * string

module Name_ord : Map.OrderedType with type t = name
module Name_map : Map.S with type key = name
module Name_set : Set.S with type elt = name

(** {2 XML trees} *)

type cdata = { loc: loc option; text: string; quoted: bool }
type comment = { loc: loc option; comment: string }
type proc_inst = { loc: loc option; app: name; args: string}
type 'a attributes = 'a Name_map.t
type str_attributes = string with_loc attributes
type xml_decl = { loc: loc option; atts: str_attributes }
type doctype = { loc: loc option; name: name; args: string}
type node = { loc: loc option; name: name ; atts: str_attributes ; subs: tree list }
and tree =
| E of node
| D of cdata
| C of comment
| PI of proc_inst

type prolog_misc = PC of comment | PPI of proc_inst
type prolog = {
      decl : xml_decl option ;
      misc : prolog_misc list ;
      doctype : doctype option ;
    }
type 'a doc = { prolog : prolog ; elements : 'a list }

(** {2 Constructors} *)

val node : ?loc:loc -> name -> ?atts:str_attributes -> tree list -> tree
val cdata : ?loc:loc -> ?quoted:bool -> string -> tree
val comment : ?loc:loc -> string -> tree
val prolog_comment : ?loc:loc -> string -> prolog_misc
val pi : ?loc:loc -> name -> string -> tree
val prolog_pi : ?loc:loc -> name -> string -> prolog_misc
val xml_decl : ?loc:loc -> str_attributes -> xml_decl
val doctype : ?loc:loc -> name -> string -> doctype
val prolog : ?decl: xml_decl -> ?doctype: doctype -> prolog_misc list -> prolog
val doc : prolog -> tree list -> tree doc

val merge_cdata : cdata -> cdata -> cdata

(** {2 Input/output} *)

(** Unescape character references and some common entity references:
     [&lt;], [&gt;], [&amp;], [&quot;], [&apos;].
     @param entities can be set to [false] not to unescape
     entity references.
*)
val unescape : ?entities:bool -> string -> string

(** Replace the following characters: [<] by [&lt;],
  [>] by [&gt;], [&] by [&amp;]. Also replace simple
  and double quotes by [&apos;] and [&quot;] if
  [quotes = true] (which is false by default). *)
val escape : ?quotes:bool -> string -> string

val from_string : ?pos_start:pos -> string -> tree list
val from_channel : ?pos_start:pos -> in_channel -> tree list
val from_file : string -> tree list

val doc_from_string : ?pos_start:pos -> string -> tree doc
val doc_from_channel : ?pos_start:pos -> in_channel -> tree doc
val doc_from_file : string -> tree doc

val to_string : tree list -> string
val doc_to_string : tree doc -> string

(** {2 Attributes} *)

val atts_empty : 'a attributes

(** Same as {!Xtmpl_rewrite.atts_of_list} but for generic attributes. *)
val atts_of_list : ?atts: 'a attributes -> (name * 'a) list -> 'a attributes

(** Same as {!Xtmpl_rewrite.atts_one} but for generic attributes. *)
val atts_one : ?atts: 'a attributes -> name -> 'a -> 'a attributes

(** Same as {!Xtmpl_rewrite.atts_remove} but for generic attributes. *)
val atts_remove : name -> 'a attributes -> 'a attributes

(** Same as {!Xtmpl_rewrite.atts_replace} but for generic attributes. *)
val atts_replace : name -> 'a -> 'a attributes -> 'a attributes

(** Same as {!Xtmpl_rewrite.get_att} but for generic attributes. *)
val get_att : 'a attributes -> name -> 'a option

(** Same as {!Xtmpl_rewrite.opt_att} but for {!str_attributes}. *)
val opt_att : str_attributes -> ?def: string -> name -> string * loc option

(** Return a string representation of the given {!str_attributes}. *)
val string_of_atts : str_attributes -> string

