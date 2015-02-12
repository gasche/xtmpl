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

let add atts name cdata xml =
  match cdata, xml with
  | None, None -> atts
  | Some s, None -> atts_one ~atts ("",name) [D s]
  | None, Some xmls -> atts_one ~atts ("", name) xmls
  | Some s, Some xmls -> atts_one ~atts ("",name) ((D (s^" ")) :: xmls)

let elt : elt_fun_ = fun ?(prefix="") tag ?xid ?id
  ?xclass ?class_ ?classes
    ?xhref ?href ?xtitle ?title
    ?xtype ?type_ ?xname ?name
    ?(atts=atts_empty) subs ->
  let atts = add atts "id" id xid in
  let atts = add atts "href" href xhref in
  let atts = add atts "title" title xtitle in
  let atts = add atts "type" type_ xtype in
  let atts = add atts "name" name xname in
  let atts =
    let class_ =
      match class_, classes with
        None, None -> None
      | Some _, None -> class_
      | None, Some l -> Some (String.concat " " l)
      | Some s, Some l -> Some (String.concat " " (s :: l))
    in
    add atts "class" class_ xclass
  in
  E ((prefix, tag), atts, subs)

let html = elt "html"


