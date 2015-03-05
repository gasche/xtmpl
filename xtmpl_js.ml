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

let log s = Firebug.console##log (Js.string s);;

let dom_of_xtmpl =
  let rec map (doc : Dom_html.document Js.t) = function
    Xtmpl.D s ->
      let n = doc##createTextNode (Js.string s) in
      (n :> Dom.node Js.t)
  | Xtmpl.E (name, atts, subs) ->
      let n =
        match name with
          ("", tag) -> doc##createElement (Js.string tag)
        | (uri, tag) ->
            (*log ("createElementNS("^uri^", "^tag^")");*)
            doc##createElementNS (Js.string uri, Js.string tag)
      in
      let atts =
        try Xtmpl.string_of_xml_atts atts
        with _ ->
            log ("problem with attributes of "^(Xtmpl.string_of_name name));
            []
      in
      List.iter
        (fun (name, v) ->
           let v = Js.string v in
           match name with
             ("", att) -> ignore (n##setAttribute (Js.string att, v))
           | (uri, att) ->
               try n##setAttributeNS(Js.string uri, Js.string att, v)
               with _ ->
                   log ("could not add attribute "^(Xtmpl.string_of_name name))
        )
        atts;
      let subs = List.map (map doc) subs in
      List.iter (Dom.appendChild n) subs;
      (n :> Dom.node Js.t)
  in
  fun t ->
    let doc = Dom_html.document in
    map doc t
;;
