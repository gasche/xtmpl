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

module Name_map = Xml.Name_map

type attributes = tree list Name_map.t
and node = { loc: Xml.loc option; name: name ; atts: attributes ; subs: tree list }
and tree =
| E of node
| D of Xml.cdata
| C of Xml.comment
| PI of Xml.proc_inst
| X of Xml.xml_decl
| DT of Xml.doctype

let atts_empty = Name_map.empty

let node ?loc name ?(atts=atts_empty) subs = E { loc ; name ; atts; subs }
let cdata = Xml.cdata
let comment = Xml.comment
let proc_inst = Xml.proc_inst
let xml_decl = Xml.xml_decl
let doctype = Xml.doctype

type rewrite_stack = (name * attributes * tree list) list

type error =
  Loop of rewrite_stack
| Parse_error of Xml.loc

exception Error of error
let error e = raise (Error e)
let loop_error stack = error (Loop stack)
let parse_error loc = error (Parse_error loc)



let rec string_of_xmls trees =
  Xml.to_string (to_xmls trees)

and to_xml tree = function
| D cdata -> Xml.D cdata
| C comment -> Xml.C comment
| PI pi -> Xml.PI pi
| X decl -> Xml.X decl
| DT dt -> Xml.DT dt
| E { loc ; name ; atts ; subs } ->
    let atts = Name_map.map string_of_xmls atts in
    let subs = to_xmls subs in
    Xml.node ?loc name ~atts subs

and to_xmls = List.map to_xml


let string_of_rewrite_stack l =
  let b = Buffer.create 256 in
  let f ((prefix,t), atts, subs) =
    Buffer.add_string b "==================\n";
    Buffer.add_string b ("Apply <"^prefix^":"^t^">\nAttributes:");
    Name_map.iter
      (fun (p,s) v ->
         Buffer.add_string b "\n  ";
         if p <> "" then Buffer.add_string b (p^":");
         Printf.bprintf b "%s=%S " s (string_of_xmls v))
      atts;
    Buffer.add_string b "\nSubs=\n";
    List.iter (fun xml -> Buffer.add_string b (string_of_xml xml)) subs;
    Buffer.add_string b "\n"
  in
  List.iter f (List.rev l);
  Buffer.contents b

let string_of_error = function
  Loop stack -> 
    "Max rewrite depth reached:\n"^(string_of_rewrite_stack stack)
| Parse_error (loc, msg) ->
    Printf.sprintf "%s: Parse error: %s" (Xml.string_of_loc loc) msg


let from_xml xmls = []

