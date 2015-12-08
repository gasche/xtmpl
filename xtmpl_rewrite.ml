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
module Name_set = Xml.Name_set

type attributes = tree list Xml.attributes
and node = { loc: Xml.loc option; name: name ; atts: attributes ; subs: tree list }
and tree =
| E of node
| D of Xml.cdata
| C of Xml.comment
| PI of Xml.proc_inst

let atts_empty = Name_map.empty

let node ?loc name ?(atts=atts_empty) subs = E { loc ; name ; atts; subs }
let cdata ?loc ?(quoted=false) text = D { Xml.loc ; text ; quoted }
let comment ?loc comment = C { Xml.loc ; Xml.comment = comment }
let pi ?loc app args = PI { Xml.loc ; app ; args }
let doc prolog elements = { Xml.prolog ; elements }

type 'a env = ('a callback) Xml.Name_map.t
and 'a callback =
  'a -> 'a env -> ?loc: Xml.loc -> attributes -> tree list -> 'a * tree list

type rewrite_stack = (name * attributes * tree list * Xml.loc option) list

type error =
  Loop of rewrite_stack
| Parse_error of Xml.loc * string
| Invalid_attribute_value of string * tree list
| Fixpoint_limit of int

exception Error of error
let error e = raise (Error e)
let loop_error stack = error (Loop stack)
let parse_error loc msg = error (Parse_error (loc, msg))
let invalid_attribute_value att v = error (Invalid_attribute_value (att,v))
let fixpoint_limit n = error (Fixpoint_limit n)

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

let tag_env = "env_"
let att_escamp = "escamp_"
let att_defer = "defer_"
let att_protect = "protect_"

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
        let l = Xtmpl_misc.split_string s [',' ; ';'] in
        List.fold_left
          (fun set s ->
             let s = Xtmpl_misc.strip_string s in
             let name =
               match Xtmpl_misc.split_string s [':'] with
                 [] | [_] -> ("",s)
               | p :: q -> (p, String.concat ":" q)
             in
             Name_set.add name set
          )
          Name_set.empty
          l

let atts_to_escape = gen_atts_to_escape (fun (x,_loc) -> x)
let xml_atts_to_escape = gen_atts_to_escape
  (function [D s] -> s.Xml.text
   | _ -> failwith ("Invalid value for attribute "^att_escamp))

(* !!!when fixing/changing one of these funs, change also in Xtmpl_js. *)
let rec to_string ?xml_atts trees =
  Xml.to_string (to_xmls ?xml_atts trees)

and atts_to_string ?xml_atts atts =
  let atts_to_escape = xml_atts_to_escape atts in
  let escamp name = Name_set.mem name atts_to_escape in
  Name_map.fold (att_to_string ~escamp ?xml_atts) atts atts_empty

and att_to_string ~escamp ?(xml_atts=true) name xmls map =
  match name with
    ("", s) when s = att_escamp -> map
  | ("", s) when s = att_protect -> map
  | _ ->
      let s = to_string xmls in
      let s = if escamp name then unescape_ampersand s else s in
      let s = if xml_atts then s else Xml.unescape s in
      Xml.atts_one ~atts: map name (s, None)

and to_xml ?xml_atts = function
| D cdata -> Xml.D cdata
| C comment -> Xml.C comment
| PI pi -> Xml.PI pi
| E { loc ; name ; atts ; subs } ->
    let atts = atts_to_string ?xml_atts atts in
    let subs = to_xmls subs in
    Xml.node ?loc name ~atts subs

and to_xmls ?xml_atts l = List.map (to_xml ?xml_atts) l

let to_doc ?xml_atts d =
  Xml.doc d.Xml.prolog (to_xmls ?xml_atts d.Xml.elements)

let doc_to_string ?xml_atts d = Xml.doc_to_string (to_doc ?xml_atts d)

let string_of_rewrite_stack l =
  let b = Buffer.create 256 in
  let f ((prefix,t), atts, subs, loc) =
    Buffer.add_string b "==================\n";
    Buffer.add_string b ("Apply <"^prefix^":"^t^">\nAttributes:");
    Name_map.iter
      (fun (p,s) v ->
         Buffer.add_string b "\n  ";
         if p <> "" then Buffer.add_string b (p^":");
         Printf.bprintf b "%s=%S " s (to_string v))
      atts;
    Buffer.add_string b "\nSubs=\n";
    List.iter (fun xml -> Buffer.add_string b (to_string [xml])) subs;
    Buffer.add_string b "\n"
  in
  List.iter f (List.rev l);
  Buffer.contents b

let string_of_error = function
  Loop stack ->
    "Max rewrite depth reached -- possible loop ?\nRewrite stack:\n"^(string_of_rewrite_stack stack)
| Parse_error (loc, msg) ->
    Printf.sprintf "%s: Parse error: %s" (Xml.string_of_loc loc) msg
| Invalid_attribute_value (att, v) ->
    Printf.sprintf "invalid value of attribute %s: %s" att (to_string v)
| Fixpoint_limit n ->
    Printf.sprintf "Xtmpl fixpoint iteration limit reached (%d)" n


let rec from_xml = function
  | Xml.D cdata -> D cdata
  | Xml.C comment -> C comment
  | Xml.PI pi -> PI pi
  | Xml.E { Xml.loc ; name ; atts ; subs } ->
      let atts = from_xml_atts atts in
      let subs = from_xmls subs in
      node ?loc name ~atts subs

and from_xml_atts atts =
    let to_escape = atts_to_escape atts in
    Name_map.mapi
      (fun name (s,loc) ->
         let pos_start =
           match loc with
             None -> None
           | Some l -> Some l.Xml.loc_start
         in
         let escamp = Name_set.mem name to_escape in
         let s = if escamp then escape_ampersand s else s in
         try from_xmls (Xml.from_string ?pos_start s)
         with Xml.Error (loc, msg) -> parse_error loc msg
      )
      atts
and from_xmls l = List.map from_xml l

let from_doc d = doc d.Xml.prolog (from_xmls d.Xml.elements)

let from_string ?pos_start str =
  try from_xmls (Xml.from_string ?pos_start str)
  with Xml.Error (loc, msg) -> parse_error loc msg

let from_file file =
  try from_xmls (Xml.from_file file)
  with Xml.Error (loc, msg) -> parse_error loc msg

let doc_from_string ?pos_start str =
  try from_doc (Xml.doc_from_string ?pos_start str)
  with Xml.Error (loc, msg) -> parse_error loc msg

let doc_from_file file =
  try from_doc (Xml.doc_from_file file)
  with Xml.Error (loc, msg) -> parse_error loc msg

let atts_replace = Xml.atts_replace
let atts_remove = Xml.atts_remove
let atts_one = Xml.atts_one
let atts_of_list = Xml.atts_of_list
let get_att = Xml.get_att
let opt_att atts ?(def=[]) name =
  match get_att atts name with
    None -> def
  | Some v -> v

let get_att_cdata atts name =
  match get_att atts name with
  | Some [D s] -> Some s.Xml.text
  | Some xmls -> Some (to_string xmls)
  | _ -> None

let opt_att_cdata atts ?(def="") name =
  match get_att_cdata atts name with None -> def | Some s -> s

let upto_first_element =
  let rec iter acc = function
  | [] -> raise Not_found
  | (E _) as xml :: _ -> List.rev (xml :: acc)
  | xml :: q -> iter (xml :: acc) q
  in
  iter []

let env_add_cb ?(prefix="") name = Name_map.add (prefix, name)

let env_get k env =
  try Some (Name_map.find k env)
  with Not_found -> None

let env_empty () = Name_map.empty

let env_add_xml ?prefix a v env =
  env_add_cb ?prefix a (fun data _ ?loc _ _ -> data, v) env

let env_of_list ?(env=env_empty()) l =
  List.fold_right (fun ((prefix,name), f) env -> env_add_cb ~prefix name f env) l env

let protect_in_env env atts =
  match get_att atts ("", att_protect) with
    None -> env
  | Some [D s] ->
      let f env s =
        match Xtmpl_misc.split_string s [':'] with
          [] -> env
        | [s] | ["" ; s] -> Name_map.remove ("",s) env
        | s1 :: q ->
            let s2 = String.concat ":" q in
            Name_map.remove (s1, s2) env
      in
      List.fold_left f env (Xtmpl_misc.split_string s.Xml.text [',' ; ';'])
  | Some l -> invalid_attribute_value att_protect l

let string_of_env env =
  let f (prefix, name) _ acc =
    let s =
      match prefix with
        "" -> name
      | s -> s ^ ":" ^ name
    in
    s :: acc
  in
  String.concat ", " (Name_map.fold f env [])

let limit =
  try Some (int_of_string (Sys.getenv "XTMPL_FIXPOINT_LIMIT"))
  with _ -> None

let max_rewrite_depth =
  try int_of_string (Sys.getenv "XTMPL_REWRITE_DEPTH_LIMIT")
  with _ -> 100

let push stack tag ?loc atts subs =
  let stack = (tag, atts, subs, loc) :: stack in
  if List.length stack > max_rewrite_depth then
    loop_error stack
  else
    stack

exception No_change

let rec eval_env stack data env ?loc atts subs =
(*  prerr_endline
    (Printf.sprintf "env: subs=%s"
      (String.concat "" (List.map string_of_xml subs)));
*)
  let env = Name_map.fold
    (fun (prefix,s) v acc ->
       (*       prerr_endline (Printf.sprintf "env: %s=%s" s v);*)
       env_add_xml ~prefix s v acc)
      atts env
  in
  eval_xmls stack data env subs

and eval_xmls stack data env xmls =
  let (data, l) =
    List.fold_left
      (fun (data, acc) xml ->
         let (data, subs) = eval_xml stack data env xml in
         (data, subs :: acc)
      )
      (data, [])
      xmls
  in
  (data, List.flatten (List.rev l))

and eval_atts =
  let f stack env name xmls (data, map) =
    let (data, xmls) = eval_xmls stack data env xmls in
    (data, Name_map.add name xmls map)
  in
  fun stack data env atts ->
    Name_map.fold (f stack env) atts (data,Name_map.empty)

and eval_xml stack data env xml =
  match xml with
  | D _ | C _ | PI _ -> (data, [ xml ])
  | E { name ; atts ; subs ; loc } ->
      let (data, atts) = eval_atts stack data env atts in
      let env_protect = protect_in_env env atts in
      match name with
        ("", t) when t = tag_env ->
          let stack = push stack name atts subs in
          eval_env stack data env_protect ?loc atts subs
      | (prefix, tag) ->
          match env_get (prefix, tag) env with
          | Some f ->
              let (defer,atts) =
                match get_att_cdata atts ("",att_defer) with
                  None -> (0, atts)
                | Some s ->
                    try
                      let n = int_of_string s in
                      (n, Name_map.remove ("", att_defer) atts)
                    with
                      _ -> (0, atts)
              in
              if defer > 0 then
                (* defer evaluation, evaluate subs first *)
                (
                 let (data, subs) = eval_xmls stack data env_protect subs in
                 let atts = Name_map.add ("",att_defer)
                   [cdata (string_of_int (defer-1))] atts
                 in
                 (data, [ node ?loc (prefix, tag) ~atts subs ])
                )
              else
                (
                 let xml =
                   try
                     let stack = push stack (prefix,tag) ?loc atts subs in
                     Some (stack, f data env_protect ?loc atts subs)
                   with No_change -> None
                 in
                 match xml with
                   None ->
                     (* no change in node, eval children anyway *)
                     let (data, subs) = eval_xmls stack data env_protect subs in
                     (data, [node ?loc (prefix, tag) ~atts subs])
                 | Some (stack, (data, xmls)) ->
                     (*prerr_endline
                        (Printf.sprintf "=== Evaluated tag %s -> %s\n"
                        tag (String.concat "" (List.map string_of_xml xmls)));*)
                     eval_xmls stack data env_protect xmls
                )
                  (* eval f before subs *)
          | None ->
              let (data, subs) = eval_xmls stack data env_protect subs in
              (data, [ node ?loc (prefix, tag) ~atts subs ])

and (eval_string : rewrite_stack -> 'a -> 'a env -> string -> 'a * string) =
  fun stack data env s ->
    let xmls = from_string s in
    let (data, xmls) = eval_xmls stack data env xmls in
    (data, to_string xmls)

let merge_cdata_list =
  let rec f acc = function
    [] -> List.rev acc
  | (D d1) :: (D d2) :: q ->
      let d = D (Xml.merge_cdata d1 d2) in
      f acc (d :: q)
  | ((D _) as x) :: q -> f (x :: acc) q
  | E node :: q ->
      let subs = f [] node.subs in
      f (E {node with subs} :: acc) q
  | xml :: q -> f (xml::acc) q
  in
  fun l -> f [] l

let merge_cdata t =
  match t with
  | E node -> E { node with subs = merge_cdata_list node.subs }
  | xml -> xml

let rec fix_point_snd ?(n=0) f (data, x) =
  match limit with
    Some l when n >= l ->
      fixpoint_limit l
  | _ ->
      let (data, y) = f (data, x) in
      if y = x then (data, x) else fix_point_snd ~n: (n+1) f (data, y)

let apply_to_xmls data env xmls =
  (*prerr_endline (string_of_env env);*)
  let f (data, xmls) = eval_xmls [] data env xmls in
  fix_point_snd f (data, xmls)

let apply_to_xml data env xml = apply_to_xmls data env [xml] ;;

let apply_to_doc data env d =
  let (data, elements) = apply_to_xmls data env d.Xml.elements in
  (data, doc d.Xml.prolog elements)

let (apply_to_string : 'a -> 'a env -> string -> 'a * tree list) = fun data env s ->
  let xmls = from_string s in
  apply_to_xmls data env xmls

let apply_to_file data env file =
  let xmls = from_file file in
  apply_to_xmls data env xmls

let apply_into_file data ?head env ~infile ~outfile =
  let (data, xmls) = apply_to_file data env infile in
  let s = to_string xmls in
  let s = match head with None -> s | Some h -> h^s in
  Xtmpl_misc.file_of_string ~file: outfile s;
  data

let apply_string_into_file data ?head env ~outfile s =
  let (data, xmls) = apply_to_string data env s in
  let s = to_string xmls in
  let s = match head with None -> s | Some h -> h^s in
  Xtmpl_misc.file_of_string ~file: outfile s;
  data