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

(*c==v=[String.split_string]=1.1====*)
let split_string ?(keep_empty=false) s chars =
  let len = String.length s in
  let rec iter acc pos =
    if pos >= len then
      match acc with
        "" -> []
      | _ -> [acc]
    else
      if List.mem s.[pos] chars then
        match acc with
          "" ->
            if keep_empty then
              "" :: iter "" (pos + 1)
            else
              iter "" (pos + 1)
        | _ -> acc :: (iter "" (pos + 1))
      else
        iter (Printf.sprintf "%s%c" acc s.[pos]) (pos + 1)
  in
  iter "" 0
(*/c==v=[String.split_string]=1.1====*)

(*c==v=[String.strip_string]=1.0====*)
let strip_string s =
  let len = String.length s in
  let rec iter_first n =
    if n >= len then
      None
    else
      match s.[n] with
        ' ' | '\t' | '\n' | '\r' -> iter_first (n+1)
      | _ -> Some n
  in
  match iter_first 0 with
    None -> ""
  | Some first ->
      let rec iter_last n =
        if n <= first then
          None
        else
          match s.[n] with
            ' ' | '\t' | '\n' | '\r' -> iter_last (n-1)
          |	_ -> Some n
      in
      match iter_last (len-1) with
        None -> String.sub s first 1
      |	Some last -> String.sub s first ((last-first)+1)
(*/c==v=[String.strip_string]=1.0====*)

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
let att_escamp = "escamp_";;

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

let env_add_att ?prefix a v env =
  env_add ?prefix a (fun _ _ _ -> [xml_of_string v]) env
;;

let atts_to_escape env atts =
  let key = ("", att_escamp) in
  let spec =
    try Some (List.assoc key atts)
    with Not_found ->
        match env_get ("", att_escamp) env with
          None -> None
        | Some f ->
            Some (string_of_xmls (f env [] []))
  in
  match spec with
    None -> []
  | Some s ->
      let l = split_string s [',' ; ';'] in
      List.map strip_string l
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
    let atts_to_escape = atts_to_escape env atts in
    let f ((prefix,s), v) =
      let escamp = List.mem s atts_to_escape in
      let v = if escamp then escape_ampersand v else v in
      let v2 = eval_string env v in
      (*prerr_endline
         (Printf.sprintf "att: %s -> %s -> %s -> %s"
         v (escape_ampersand v) v2 (unescape_ampersand v2)
         );*)
      let v2 = if escamp then unescape_ampersand v2 else v2 in
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

  