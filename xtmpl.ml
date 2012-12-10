(*********************************************************************************)
(*                Xtmpl                                                          *)
(*                                                                               *)
(*    Copyright (C) 2012 Institut National de Recherche en Informatique          *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    3 as published by the Free Software Foundation.                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU Lesser General Public           *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*                                                                               *)
(*********************************************************************************)

(** *)

type name = string * string
type attribute = (name * string)

(*c==v=[File.string_of_file]=1.0====*)
let string_of_file name =
  let chanin = open_in_bin name in
  let len = 1024 in
  let s = String.create len in
  let buf = Buffer.create len in
  let rec iter () =
    try
      let n = input chanin s 0 len in
      if n = 0 then
        ()
      else
        (
         Buffer.add_substring buf s 0 n;
         iter ()
        )
    with
      End_of_file -> ()
  in
  iter ();
  close_in chanin;
  Buffer.contents buf
(*/c==v=[File.string_of_file]=1.0====*)

(*c==v=[File.file_of_string]=1.1====*)
let file_of_string ~file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc
(*/c==v=[File.file_of_string]=1.1====*)

module Str_map = Map.Make (struct type t = string * string let compare = compare end);;

let tag_main = "main_";;
let tag_env = "env_";;
let att_defer = "defer_";;

exception No_change
type env = (env -> attribute list -> tree list -> tree list) Str_map.t
and callback = env -> attribute list -> tree list -> tree list
and tree =
    E of name * attribute list * tree list
  | D of string


let env_add ?(prefix="") name = Str_map.add (prefix, name) ;;
let env_get k env =
  try Some (Str_map.find k env)
  with Not_found -> None
;;
let env_empty =
  let f_main env atts subs = subs in
  env_add tag_main f_main Str_map.empty
;;


let rec fix_point ?(n=0) f x =
  (*
  let file = Printf.sprintf "/tmp/fixpoint%d.txt" n in
  file_of_string ~file x;
  *)
  let y = f x in
  if y = x then x else fix_point ~n: (n+1) f y
;;

let string_of_env env =
  let f (prefix, name) _ acc =
    let s =
      match prefix with
        "" -> name
      | s -> s ^ ":" ^ name
    in
    s :: acc
  in
  String.concat ", " (Str_map.fold f env [])
;;

let string_of_xml tree =
  try
    let b = Buffer.create 256 in
    let ns_prefix s = Some s in
    let output = Xmlm.make_output ~ns_prefix ~decl: false (`Buffer b) in
    let frag = function
    | E (tag, atts, childs) -> `El ((tag, atts), childs)
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
;;

let string_of_xmls l = String.concat "" (List.map string_of_xml l);;

let xml_of_source s_source source =
 try
    let ns s = Some s in
    let input = Xmlm.make_input ~ns ~enc: (Some `UTF_8) source in
    let el (tag, atts) childs = E (tag, atts, childs)  in
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

let xml_of_string ?(add_main=true) s =
  let s =
    if add_main then
      Printf.sprintf "<%s>%s</%s>" tag_main s tag_main
    else
      s
  in
  xml_of_source (s^"\n") (`String (0, s))
;;

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
;;

let re_escape = Str.regexp "&\\(\\([a-z]+\\)\\|\\(#[0-9]+\\)\\);";;

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
;;

let re_amp = Str.regexp_string "&amp;";;
let unescape_ampersand s = Str.global_replace re_amp "&" s;;


let env_add_att ?prefix a v env =
  env_add ?prefix a (fun _ _ _ -> [xml_of_string v]) env
;;


let rec eval_env env atts subs =
(*  prerr_endline
    (Printf.sprintf "env: subs=%s"
      (String.concat "" (List.map string_of_xml subs)));
*)
  let env = List.fold_left
    (fun acc ((prefix,s),v) ->
(*       prerr_endline (Printf.sprintf "env: %s=%s" s v);*)
       env_add_att ~prefix s v acc)
    env atts
  in
  List.flatten (List.map (eval_xml env) subs)

and eval_xml env = function
| (D _) as xml -> [ xml ]
| other ->
    let (tag, atts, subs) =
      match other with
        D _ -> assert false
      | E (tag, atts, subs) -> (tag, atts, subs)
    in
    let f ((prefix,s), v) =
      let v2 = eval_string env (escape_ampersand v) in
      (*prerr_endline
         (Printf.sprintf "att: %s -> %s -> %s -> %s"
         v (escape_ampersand v) v2 (unescape_ampersand v2)
         );*)
      let v2 = unescape_ampersand v2 in
      ((prefix, s), v2)
    in
    let atts = List.map f atts in
    let (defer,atts) = List.partition
      (function
       | (("",s), n) when s = att_defer ->
           (try ignore (int_of_string n); true
            with _ -> false)
       | _ -> false
      )
      atts
    in
    let defer =
      match defer with
        [] -> 0
      | ((_,_), n) :: _ -> int_of_string n
    in
    match tag with
      ("", t) when t = tag_env -> ((eval_env env atts subs) : tree list)
    | (prefix, tag) ->
        match env_get (prefix, tag) env with
        | Some f ->
            if defer > 0 then
              (* defer evaluation, evaluate subs first *)
              (
               let subs = List.flatten (List.map (eval_xml env) subs) in
               let att_defer = (("",att_defer), string_of_int (defer-1)) in
               let atts = att_defer :: atts in
               [ E ((prefix, tag), atts, subs) ]
              )
            else
              (
               let xml =
                 try Some (f env atts subs)
                 with No_change -> None
               in
               match xml with
                 None ->
                   (* no change in node, eval children anyway *)
                   let subs = List.flatten (List.map (eval_xml env) subs) in
                   [ E ((prefix, tag), atts, subs) ]
               | Some xml ->
                   (*prerr_endline
                     (Printf.sprintf "=== Evaluated tag %s -> %s\n"
                    tag (String.concat "" (List.map string_of_xml xml)));*)
                   List.flatten (List.map (eval_xml env) xml)
              )
              (* eval f before subs *)
        | None ->
            let subs = List.flatten (List.map (eval_xml env) subs) in
            [ E ((prefix, tag), atts, subs) ]

and eval_string env s =
  let xml = xml_of_string s in
  string_of_xmls (eval_xml env xml)
;;

let apply_to_xmls env xmls =
  let f xmls = List.flatten (List.map (eval_xml env) xmls) in
  fix_point f xmls
;;

let apply_to_xml env xml = apply_to_xmls env [xml] ;;

let apply_to_string env s =
  let xml = xml_of_string s in
  apply_to_xml env xml
;;

let apply_to_file env file =
  let s = string_of_file file in
  let xml = xml_of_string s in
  apply_to_xml env xml
;;

let apply_into_file ?head env ~infile ~outfile =
  let xmls = apply_to_file env infile in
  let s = string_of_xmls xmls in
  let s = match head with None -> s | Some h -> h^s in
  file_of_string ~file: outfile s
;;

let apply_string_into_file ?head env ~outfile s =
  let xmls = apply_to_string env s in
  let s = string_of_xmls xmls in
  let s = match head with None -> s | Some h -> h^s in
  file_of_string ~file: outfile s
;;

let get_arg args name =
  try Some (List.assoc name args)
  with Not_found -> None
;;

let string_of_args args =
  String.concat " "
    (List.map (fun ((pref,s),v) -> Printf.sprintf "%s%s=%S"
      (match pref with "" -> "" | p -> p^":") s v)
    args)
;;

let opt_arg args ?(def="") name =
  match get_arg args name with None -> def | Some s -> s
;;


let env_of_list ?(env=env_empty) l =
  List.fold_right (fun ((prefix,name), f) env -> env_add ~prefix name f env) l env
;;

  