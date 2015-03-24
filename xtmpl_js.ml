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

(* Provide an implementation of these functions using the Js_of_ocaml regexp
  module, because some Str functions are not available in javascript (since
  they are C functions). *)

module X = Xtmpl

let gen_atts_to_escape =
  let key = ("", X.att_escamp) in
  fun to_s atts ->
    let spec =
      try Some (X.Name_map.find key atts)
      with Not_found -> None
    in
    match spec with
      None -> X.Name_set.empty
    | Some x ->
        let s = to_s x in
        let l = X.split_string s [',' ; ';'] in
        List.fold_left
          (fun set s ->
             let s = X.strip_string s in
             let name =
               match X.split_string s [':'] with
                 [] | [_] -> ("",s)
               | p :: q -> (p, String.concat ":" q)
             in
             X.Name_set.add name set
          )
          X.Name_set.empty
          l

let atts_to_escape = gen_atts_to_escape (fun x -> x)
let xml_atts_to_escape = gen_atts_to_escape
  (function [X.D s] -> s
   | _ -> failwith ("Invalid value for attribute "^X.att_escamp))

let re_amp = Regexp.regexp_string "&amp;"
let unescape_ampersand s = Regexp.global_replace re_amp "&" s

let regexp_substitute re f s =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec iter p =
    match Regexp.search re s p with
      None -> Buffer.add_substring buf s p (len - p)
    | Some (p2, r) ->
        let s2 = f r in
        if p < p2 then Buffer.add_substring buf s p (p2 - p);
        Buffer.add_string buf s2;
        iter (p2 + String.length (Regexp.matched_string r))
  in
  iter 0 ;
  Buffer.contents buf

(* This function only unescape some common named entities
  and characters with code between 0 and 255. *)
let unescape_entities =
  let re = Regexp.regexp "&([a-zA-Z]+|(x?[0-9a-fA-F]+));" in
  let str_of_char_code default code =
    if code >= 256 then
      default
    else
      String.make 1 (Char.chr code)
  in
  let f r =
    let matched = Regexp.matched_string r in
    let group = Regexp.matched_group r 1 in
    match
      try
        let s = "0"^matched in
        Some (int_of_string s)
      with _ -> None
    with
      Some code -> str_of_char_code matched code
    | None ->
        match group with
          None -> matched
        | Some s ->
            match s with
              "lt" -> "<"
            | "gt" -> ">"
            | "amp" -> "&"
            | "quot" -> "\""
            | "apos" -> "'"
            | _ -> matched
  in
  fun s -> regexp_substitute re f s

let rec string_of_xml ?xml_atts tree =
  try
    let b = Buffer.create 256 in
    let ns_prefix s = Some s in
    let output = Xmlm.make_output ~ns_prefix ~decl: false (`Buffer b) in
    let frag = function
    | X.E (tag, atts, childs) ->
        let atts = string_of_xml_atts ?xml_atts atts in
        `El ((tag, atts), childs)
    | X.D d -> `Data d
    in
    Xmlm.output_doc_tree frag output (None, tree);
    Buffer.contents b
  with
    Xmlm.Error ((line, col), error) ->
      let msg = Printf.sprintf "Line %d, column %d: %s"
        line col (Xmlm.error_message error)
      in
      failwith msg

and string_of_xmls ?xml_atts l = String.concat "" (List.map (string_of_xml ?xml_atts) l)
and string_of_xml_atts ?(xml_atts=true) atts =
      let atts_to_escape = xml_atts_to_escape atts in
      let f name xmls acc =
        match name with
          ("", s) when s = X.att_escamp -> acc
        | ("", s) when s = X.att_protect -> acc
        | _ ->
            let s = string_of_xmls xmls in
            let escamp = X.Name_set.mem name atts_to_escape in
            let s = if escamp then unescape_ampersand s else s in
            let s = if xml_atts then s else unescape_entities s in
            (name, s) :: acc
      in
      List.rev (X.Name_map.fold f atts [])
(* end of js_of_ocaml specific implementation *)

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
        try string_of_xml_atts ~xml_atts: false atts
        with e ->
            let msg = Printf.sprintf
              "problem with attributes of %s: %s"
                (Xtmpl.string_of_name name) (Printexc.to_string e)
            in
            log msg ;
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
