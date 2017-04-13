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

module X = Xtmpl_rewrite
module Xml = Xtmpl_xml
module Name_map = Xml.Name_map

module SMap = Map.Make(String)
type ns_env = { ns : string ; map : string SMap.t }
let ns_env_empty = { ns = "" ; map = SMap.empty }

(* https://www.w3.org/TR/DOM-Level-2-Core/core.html#ID-ElSetAttrNS *)
let apply_ns ?(att=false) env = function
| ("","xmlns") -> ("http://www.w3.org/2000/xmlns/","xmlns")
| ("",tag) -> if att then ("", tag) else (env.ns, tag)
| ("xmlns",tag) -> ("","")
| (pref,tag) ->
    match SMap.find pref env.map with
    | exception Not_found -> (pref, tag)
    | s -> if s = env.ns then ("", tag) else (s, pref ^ ":" ^ tag)

let ns_env_of_att name (str,_) env =
 match name with
    ("xmlns", tag) -> { env with map = SMap.add tag str env.map }
  | ("", "xmlns") -> { env with ns = str }
  | _ -> env

let ns_env_of_atts env atts = Name_map.fold ns_env_of_att atts env

let atts_to_string name atts =
  try Xtmpl_rewrite.atts_to_string ~xml_atts: false atts
  with e ->
      let msg = Printf.sprintf
        "problem with attributes of %s: %s"
          (Xtmpl_xml.string_of_name name) (Printexc.to_string e)
      in
      log msg ;
      Xml.atts_empty

let dom_of_xtmpl =
  let rec map (doc : Dom_html.document Js.t) ns_env = function
    X.D s ->
      let n = doc##createTextNode (Js.string s.Xml.text) in
      (n :> Dom.node Js.t)
  | X.C _ | X.PI _ ->
      let n = doc##createComment (Js.string " ") in
      (n :> Dom.node Js.t)
  | X.E { X.name ; atts ; subs} ->
      let atts = atts_to_string name atts in
      let ns_env = ns_env_of_atts ns_env atts in
      let (elt_ns, tag) = apply_ns ns_env name in
      let n =
        match elt_ns, tag with
        | ("", tag) -> doc##createElement (Js.string tag)
        | (ns, tag) -> doc ## createElementNS (Js.string ns) (Js.string tag)
      in
      Name_map.iter
        (fun name (v, _) ->
           let v = Js.string v in
           let env = if elt_ns <> "" then { ns_env with ns = elt_ns } else ns_env in
           match apply_ns ~att: true env name with
           | ("",  "") ->
               log (Printf.sprintf "not adding %s attribute" (Xml.string_of_name name))
           | ("", att) ->
               log (Printf.sprintf "setAttribute (%s)" att);
               ignore (n ## setAttribute (Js.string att) v)
           | (uri, att) ->
               log (Printf.sprintf "setAttributeNS (%s, %s)" uri att);
               try n ## setAttributeNS (Js.string uri) (Js.string att) v
               with _ ->
                   log ("could not add attribute "^(Xml.string_of_name (uri,att)))
        )
        atts;
      let subs = List.map (map doc ns_env) subs in
      List.iter (Dom.appendChild n) subs;
      (n :> Dom.node Js.t)
  in
  fun ?(doc=Dom_html.document) t ->
    map doc ns_env_empty t
;;
