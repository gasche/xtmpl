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



let re_escape = Str.regexp "&\\(\\([a-z]+\\)\\|\\(#[0-9]+\\)\\);"
let escape_ampersand s =
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
      '&' when Str.string_match re_escape s i ->
        Buffer.add_char b '&'
    | '&' -> Buffer.add_string b "&amp;"
    | c -> Buffer.add_char b c
  done;
  Buffer.contents b

let re_amp = Str.regexp_string "&amp;"
let unescape_ampersand s = Str.global_replace re_amp "&" s


type 'a attributes = 'a Name_map.t
type 'a tree =
    E of name * 'a attributes * 'a tree list
  | D of string

type rewrite_tree = (('a tree list) as 'a) tree

type xml_attributes = (rewrite_tree list) attributes
type str_attributes = string attributes


let atts_empty = Name_map.empty
let string_of_name = function
  ("",s) -> s
| (p, s) -> p ^ ":" ^ s

let node tag ?(atts=atts_empty) subs = E (tag, atts, subs)
let cdata str = D str

let att_escamp = "escamp_"

let gen_atts_to_escape =
  let key = ("", att_escamp) in
  fun to_s atts ->
    let spec =
      try Some (Name_map.find key atts)
      with Not_found -> None
    in
    match spec with
      None -> Name_set.empty
    | Some x ->
        let s = to_s x in
        let l = split_string s [',' ; ';'] in
        List.fold_left
          (fun set s ->
             let s = strip_string s in
             let name =
               match split_string s [':'] with
                 [] | [_] -> ("",s)
               | p :: q -> (p, String.concat ":" q)
             in
             Name_set.add name set
          )
          Name_set.empty
          l

let atts_to_escape = gen_atts_to_escape (fun x -> x)
let xml_atts_to_escape = gen_atts_to_escape
  (function [D s] -> s
   | _ -> failwith ("Invalid value for attribute "^att_escamp))

