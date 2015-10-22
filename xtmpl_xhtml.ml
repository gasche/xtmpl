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

type tree = rewrite_tree
type attributes = xml_attributes
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

let head = elt "head"
let meta = elt "meta"
let title = elt "title"
let base = elt "base"
let link = elt "link"
let style = elt "style"

let body = elt "body"
let article = elt "article"
let section = elt "section"
let nav = elt "nav"
let aside = elt "aside"
let h1 = elt "h1"
let h2 = elt "h2"
let h3 = elt "h3"
let h4 = elt "h4"
let h5 = elt "h5"
let h6 = elt "h6"
let header = elt "header"
let footer = elt "footer"
let address = elt "address"

let p = elt "p"
let hr = elt "hr" []
let pre = elt "pre"
let blockquote = elt "blockquote"
let ol = elt "ol"
let ul = elt "ul"
let li = elt "li"
let dl = elt "dl"
let dt = elt "dt"
let dd = elt "dd"
let figure = elt "figure"
let figcaption = elt "figcaption"
let div = elt "div"
let main = elt "main"

let a = elt "a"
let em = elt "em"
let strong = elt "strong"
let small = elt "small"
let s = elt "s"
let cite = elt "cite"
let q = elt "q"
let dfn = elt "dfn"
let abbr = elt "abbr"
let data = elt "data"
let time = elt "time"
let code = elt "code"
let var = elt "var"
let samp = elt "samp"
let kbd = elt "kbd"
let sub = elt "sub"
let sup = elt "sup"
let i = elt "i"
let b = elt "b"
let u = elt "u"
let mark = elt "mark"
let ruby = elt "ruby"
let rb = elt "rb"
let rt = elt "rt"
let rtc = elt "rtc"
let rp = elt "rp"
let bdi = elt "bdi"
let bdo = elt "bdo"
let span = elt "span"
let br = elt "br" []
let wbr = elt "wbr" []

let ins = elt "ins"
let del = elt "del"

let embed = elt "embed"
let object_ = elt "object"
let img = elt "img"
let audio = elt "audio"
let video = elt "video"
let source = elt "source"
let script = elt "script"

let table = elt "table"
let caption = elt "caption"
let colgroup = elt "colgroup"
let col = elt "col"
let tbody = elt "tbody"
let thead = elt "thead"
let tfoot = elt "tfoot"
let tr = elt "tr"
let td = elt "td"
let th = elt "th"

let form = elt "form"
let label = elt "label"
let input = elt "input"
let button = elt "button"
let select = elt "select"
let datalist = elt "datalist"
let optgroup = elt "optgroup"
let option = elt "option"
let textarea = elt "textarea"
let keygen = elt "keygen"
let output = elt "output"
let progress = elt "progress"
let meter = elt "meter"
let fieldset = elt "fieldset"
let legend = elt "legend"
