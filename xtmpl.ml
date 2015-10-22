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
module B = Xtmpl_base

let split_string = B.split_string
let strip_string = B.strip_string

let tag_main = "main_"
let tag_env = "env_"
let att_defer = "defer_"
let att_escamp = B.att_escamp
let att_protect = "protect_"


exception No_change

module Name_ord = B.Name_ord
module Name_map = B.Name_map
module Name_set = B.Name_set

type 'a attributes = 'a Name_map.t
type 'a tree = 'a B.tree =
    E of name * 'a attributes * 'a tree list
  | D of string
type rewrite_tree = (('a tree list) as 'a) tree
type xml_attributes = rewrite_tree list attributes
type str_attributes = string attributes

type 'a env = ('a callback) Name_map.t
and 'a callback = 'a -> 'a env -> xml_attributes -> rewrite_tree list ->
  'a * rewrite_tree list

type rewrite_stack = (name * xml_attributes * rewrite_tree list) list
exception Loop of  rewrite_stack

let atts_empty = B.atts_empty
let string_of_name = B.string_of_name
let node = B.node
let cdata = B.cdata


let get_att atts name =
  try Some (Name_map.find name atts)
  with Not_found -> None

(* This function only unescape some common named entities
  and characters with code between 0 and 255. *)
let unescape_entities =
  let re = Str.regexp "&\\([a-zA-Z]+\\|\\(x?[0-9a-fA-F]+\\)\\);" in
(*  let re_num = Str.regexp "&#\\(x?[0-9a-fA-F]+\\);" in*)
  let str_of_char_code default code =
    if code >= 256 then
      default
    else
      String.make 1 (Char.chr code)
  in
  let f s =
    let matched = Str.matched_string s in
    let group = Str.matched_group 1 s in
    match
      try
        let s = "0"^matched in
        Some (int_of_string s)
      with _ -> None
    with
      Some code -> str_of_char_code matched code
    | None ->
        match group with
          "lt" -> "<"
        | "gt" -> ">"
        | "amp" -> "&"
        | "quot" -> "\""
        | "apos" -> "'"
        | _ -> matched
  in
  fun s -> Str.global_substitute re f s


(* !!! When fixing these string_of_ functions, fix also the javascript
   !!! versions in xtmpl_js.ml *)
let rec string_of_xml ?xml_atts tree =
  assert false
  (*
  try
    let b = Buffer.create 256 in
    let ns_prefix s = Some s in
    let output = Xmlm.make_output ~ns_prefix ~decl: false (`Buffer b) in
    let frag = function
    | E (tag, atts, childs) ->
        let atts = string_of_xml_atts ?xml_atts atts in
        `El ((tag, atts), childs)
    | D d -> `Data d
    in
    Xmlm.output_doc_tree frag output (None, tree);
    Buffer.contents b
  with
    Xmlm.Error ((line, col), error) ->
      let msg = Printf.sprintf "Line %d, column %d: %s"
        line col (Xmlm.error_message error)
      in
      failwith msg
*)
and string_of_xmls ?xml_atts l = String.concat "" (List.map (string_of_xml ?xml_atts) l)
and string_of_xml_atts ?(xml_atts=true) atts =
  assert false
  (*    let atts_to_escape = B.xml_atts_to_escape atts in
      let f name xmls acc =
        match name with
          ("", s) when s = B.att_escamp -> acc
        | ("", s) when s = att_protect -> acc
        | _ ->
            let s = string_of_xmls xmls in
            let escamp = Name_set.mem name atts_to_escape in
            let s = if escamp then B.unescape_ampersand s else s in
            let s = if xml_atts then s else unescape_entities s in
            (name, s) :: acc
      in
      List.rev (Name_map.fold f atts [])
*)
let get_att_cdata atts name =
  match get_att atts name with
  | Some [D s] -> Some s
  | Some xmls -> Some (string_of_xmls xmls)
  | _ -> None


let string_of_stack l =
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

let env_add_cb ?(prefix="") name = Name_map.add (prefix, name)

let env_get k env =
  try Some (Name_map.find k env)
  with Not_found -> None

let env_empty () =
  let (f_main : 'a callback) = fun data env atts subs -> (data, subs) in
  env_add_cb tag_main f_main Name_map.empty

let limit =
  try Some (int_of_string (Sys.getenv "XTMPL_FIXPOINT_LIMIT"))
  with _ -> None

let rec fix_point_snd ?(n=0) f (data, x) =
  match limit with
    Some l when n >= l ->
      failwith ("Xtmpl fixpoint iteration limit reached ("^(string_of_int l)^")")
  | _ ->
      let (data, y) = f (data, x) in
      (*let file = Printf.sprintf "/tmp/fixpoint%d.txt" n in
      file_of_string ~file (string_of_xmls y);*)
      if y = x then (data, x) else fix_point_snd ~n: (n+1) f (data, y)

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

let rec xml_of_source s_source source =
 try
    let ns s = Some s in
    let input = Xmlm.make_input ~ns ~enc: (Some `UTF_8) source in
    let el (tag, atts) childs =
      let atts = xmls_of_atts atts in
      E (tag, atts, childs)
    in
    let data d = D d in
    let (_, tree) = Xmlm.input_doc_tree ~el ~data input in
    tree
  with
    Xmlm.Error ((line, col), error) ->
      let msg = Printf.sprintf "%sLine %d, column %d: %s"
        s_source line col (Xmlm.error_message error)
      in
      failwith msg
  | Invalid_argument e ->
      let msg = Printf.sprintf "%sInvalid_argumen(%s)" s_source e in
      failwith msg

and xmls_of_atts atts =
      let atts = List.fold_left
        (fun map (name, s) -> Name_map.add name s map)
        Name_map.empty atts
      in
      let atts_to_escape = B.atts_to_escape atts in
      Name_map.mapi
        (fun name s ->
           let escamp = Name_set.mem name atts_to_escape in
           let s = if escamp then B.escape_ampersand s else s in
           match xml_of_string s with
             E (_,_,xmls) -> (* remove main_ tag*) xmls
           | _ -> assert false
        )
        atts

and xml_of_string ?(add_main=true) s =
  let s =
    if add_main then
      Printf.sprintf "<%s>%s</%s>" tag_main s tag_main
    else
      s
  in
  xml_of_source (s^"\n") (`String (0, s))

let xml_of_file file =
  let ic = open_in file in
  try
    let xml = xml_of_source
      (Printf.sprintf "File %S, " file) (`Channel ic)
    in
    close_in ic;
    xml
  with
    e ->
      close_in ic;
      raise e

let env_add_xml ?prefix a v env =
  env_add_cb ?prefix a (fun data _ _ _ -> data, v) env

let protect_in_env env atts =
  match get_att atts ("", att_protect) with
    None -> env
  | Some [D s] ->
      let f env s =
        match B.split_string s [':'] with
          [] -> env
        | [s] | ["" ; s] -> Name_map.remove ("",s) env
        | s1 :: q ->
            let s2 = String.concat ":" q in
            Name_map.remove (s1, s2) env
      in
      List.fold_left f env (B.split_string s [',' ; ';'])
  | _ -> failwith ("Invalid value for attribute "^att_protect)

let max_rewrite_depth =
  try int_of_string (Sys.getenv "XTMPL_REWRITE_DEPTH_LIMIT")
  with _ -> 100

let push stack tag atts subs =
  let stack = (tag, atts, subs) :: stack in
  if List.length stack > max_rewrite_depth then
    raise (Loop stack)
  else
    stack

let rec eval_env stack data env atts subs =
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

and eval_xml stack data env = function
| (D _) as xml -> (data, [ xml ])
| other ->
    let (tag, atts, subs) =
      match other with
        D _ -> assert false
      | E (tag, atts, subs) -> (tag, atts, subs)
    in
    (*let (data, atts_to_escape) = atts_to_escape data env atts in*)
    (*let f (data, acc) ((prefix,s), v) =
      let escamp = List.mem s atts_to_escape in
      let v = if escamp then escape_ampersand v else v in
      let (data, v2) = eval_string stack data env v in
      (*prerr_endline
         (Printf.sprintf "att: %s -> %s -> %s -> %s"
         v (escape_ampersand v) v2 (unescape_ampersand v2)
         );*)
      let v2 = if escamp then unescape_ampersand v2 else v2 in
      (data, ((prefix, s), v2) :: acc)
    in
    let (data, atts) = List.fold_left f (data, []) atts in
    *)
    let (data, atts) = eval_atts stack data env atts in

    let env_protect = protect_in_env env atts in
    match tag with
      ("", t) when t = tag_env ->
        let stack = push stack tag atts subs in
        eval_env stack data env_protect atts subs
    | (prefix, tag) ->
        match env_get (prefix, tag) env with
        | Some f ->
            (*prerr_endline
              ("applying rule for "^prefix^":"^tag^" "^
               (String.concat ", "
                 (List.map (fun ((p,s), v) -> p^":"^s^"="^v) atts)
               )
              );*)
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
               let atts = Name_map.add ("",att_defer) [D (string_of_int (defer-1))] atts in
               (data, [ E ((prefix, tag), atts, subs) ])
              )
            else
              (
               let xml =
                 try
                   let stack = push stack (prefix,tag) atts subs in
                   Some (stack, f data env_protect atts subs)
                 with No_change -> None
               in
               match xml with
                 None ->
                   (* no change in node, eval children anyway *)
                   let (data, subs) = eval_xmls stack data env_protect subs in
                   (data, [ E ((prefix, tag), atts, subs) ])
               | Some (stack, (data, xmls)) ->
                   (*prerr_endline
                     (Printf.sprintf "=== Evaluated tag %s -> %s\n"
                    tag (String.concat "" (List.map string_of_xml xmls)));*)
                   eval_xmls stack data env_protect xmls
              )
              (* eval f before subs *)
        | None ->
            let (data, subs) = eval_xmls stack data env_protect subs in
            (data, [ E ((prefix, tag), atts, subs) ])

and (eval_string : rewrite_stack -> 'a -> 'a env -> string -> 'a * string) =
  fun stack data env s ->
    let xml = xml_of_string s in
    let (data, xmls) = eval_xml stack data env xml in
    (data, string_of_xmls xmls)

let merge_cdata_list =
  let rec f acc = function
    [] -> List.rev acc
  | (D s1) :: (D s2) :: q -> f acc ((D (s1^s2)) :: q)
  | ((D _) as x) :: q -> f (x :: acc) q
  | (E (t, atts, subs)) :: q ->
      let subs = f [] subs in
      f ((E (t, atts, subs)) :: acc) q
  in
  fun l -> f [] l

let merge_cdata t =
  match t with
  | D _ -> t
  | E (tag, atts, subs) -> E (tag, atts, merge_cdata_list subs)

let apply_to_xmls data env xmls =
  (*prerr_endline (string_of_env env);*)
  let f (data, xmls) = eval_xmls [] data env xmls in
  fix_point_snd f (data, xmls)

let apply_to_xml data env xml = apply_to_xmls data env [xml] ;;

let (apply_to_string : 'a -> 'a env -> string -> 'a * rewrite_tree list) = fun data env s ->
  let xml = xml_of_string s in
  apply_to_xml data env xml

let apply_to_file data env file =
  let s = B.string_of_file file in
  let xml = xml_of_string s in
  apply_to_xml data env xml

let apply_into_file data ?head env ~infile ~outfile =
  let (data, xmls) = apply_to_file data env infile in
  let s = string_of_xmls xmls in
  let s = match head with None -> s | Some h -> h^s in
  B.file_of_string ~file: outfile s;
  data

let apply_string_into_file data ?head env ~outfile s =
  let (data, xmls) = apply_to_string data env s in
  let s = string_of_xmls xmls in
  let s = match head with None -> s | Some h -> h^s in
  B.file_of_string ~file: outfile s;
  data

let string_of_atts atts =
  String.concat " "
    (Name_map.fold
     (fun (pref,s) v acc ->
        let s = Printf.sprintf "%s%s=%S"
          (match pref with "" -> "" | p -> p^":") s (string_of_xmls v)
        in
        s :: acc
     )
       atts []
    )

let env_of_list ?(env=env_empty()) l =
  List.fold_right (fun ((prefix,name), f) env -> env_add_cb ~prefix name f env) l env
;;

let opt_att atts ?(def=[]) name =
  match get_att atts name with None -> def | Some s -> s

let opt_att_cdata atts ?(def="") name =
  match get_att_cdata atts name with None -> def | Some s -> s

let atts_of_list =
  let f acc (name,v) = Name_map.add name v acc in
  fun ?(atts=atts_empty) l -> List.fold_left f atts l

let atts_one ?(atts=atts_empty) name v = Name_map.add name v atts;;
let atts_replace = Name_map.add;;
let atts_remove = Name_map.remove;;

(* deprecated stuff *)

let get_arg = get_att
let get_arg_cdata = get_att_cdata
let opt_arg = opt_att
let opt_arg_cdata = opt_att_cdata
let string_of_args = string_of_atts
let env_add = env_add_cb
let env_add_att = env_add_xml