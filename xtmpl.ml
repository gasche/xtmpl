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
let att_protect = "protect_";;

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

module Name_ord = struct
  type t = name
  let compare (p1,s1) (p2,s2) =
    match String.compare s1 s2 with
      0 -> String.compare p1 p2
    | n -> n
  end
;;
module Name_map = Map.Make (Name_ord)
module Name_set = Set.Make (Name_ord)

type 'a env = ('a callback) Str_map.t
and 'a callback = 'a -> 'a env -> attributes -> tree list -> 'a * tree list
and tree =
    E of name * attributes * tree list
  | D of string
and attributes = tree list Name_map.t

type rewrite_stack = (name * attributes * tree list) list
exception Loop of  rewrite_stack

let atts_empty = Name_map.empty

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

;;

let atts_to_escape = gen_atts_to_escape (fun x -> x);;
let xml_atts_to_escape = gen_atts_to_escape
  (function [D s] -> s
   | _ -> failwith ("Invalid value for attribute "^att_escamp));;

let get_arg args name =
  try Some (Name_map.find name args)
  with Not_found -> None
;;

let rec string_of_xml tree =
  try
    let b = Buffer.create 256 in
    let ns_prefix s = Some s in
    let output = Xmlm.make_output ~ns_prefix ~decl: false (`Buffer b) in
    let frag = function
    | E (tag, atts, childs) ->
        let atts = string_of_xml_atts atts in
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

and string_of_xmls l = String.concat "" (List.map string_of_xml l)
and string_of_xml_atts atts =
      let atts_to_escape = xml_atts_to_escape atts in
      let f name xmls acc =
        match name with
          ("", s) when s = att_escamp -> acc
        | _ ->
            let s = string_of_xmls xmls in
            let escamp = Name_set.mem name atts_to_escape in
            let s = if escamp then unescape_ampersand s else s in
            (name, s) :: acc
      in
      List.rev (Name_map.fold f atts [])
;;

let get_arg_cdata args name =
  match get_arg args name with
  | Some [D s] -> Some s
  | Some xmls -> Some (string_of_xmls xmls)
  | _ -> None
;;

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
;;

let env_add ?(prefix="") name = Str_map.add (prefix, name) ;;
let env_get k env =
  try Some (Str_map.find k env)
  with Not_found -> None
;;
let env_empty () =
  let (f_main : 'a callback) = fun data env atts subs -> (data, subs) in
  env_add tag_main f_main Str_map.empty
;;

let limit =
  try Some (int_of_string (Sys.getenv "XTMPL_FIXPOINT_LIMIT"))
  with _ -> None
;;

let rec fix_point_snd ?(n=0) f (data, x) =
  match limit with
    Some l when n >= l ->
      failwith ("Xtmpl fixpoint iteration limit reached ("^(string_of_int l)^")")
  | _ ->
      let (data, y) = f (data, x) in
      (*let file = Printf.sprintf "/tmp/fixpoint%d.txt" n in
      file_of_string ~file (string_of_xmls y);*)
      if y = x then (data, x) else fix_point_snd ~n: (n+1) f (data, y)
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
      let atts_to_escape = atts_to_escape atts in
      Name_map.mapi
        (fun name s ->
           let escamp = Name_set.mem name atts_to_escape in
           let s = if escamp then escape_ampersand s else s in
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
  env_add ?prefix a (fun data _ _ _ -> data, v) env
;;



let protect_in_env env atts =
  match get_arg atts ("", att_protect) with
    None -> env
  | Some [D s] ->
      let f env s =
        match split_string s [':'] with
          [] -> env
        | [s] | ["" ; s] -> Str_map.remove ("",s) env
        | s1 :: q ->
            let s2 = String.concat ":" q in
            Str_map.remove (s1, s2) env
      in
      List.fold_left f env (split_string s [',' ; ';'])
  | _ -> failwith ("Invalid value for attribute "^att_protect)
;;

let max_rewrite_depth =
  try int_of_string (Sys.getenv "XTMPL_REWRITE_DEPTH_LIMIT")
  with _ -> 100
;;

let push stack tag atts subs =
  let stack = (tag, atts, subs) :: stack in
  if List.length stack > max_rewrite_depth then
    raise (Loop stack)
  else
    stack
;;

let rec eval_env stack data env atts subs =
(*  prerr_endline
    (Printf.sprintf "env: subs=%s"
      (String.concat "" (List.map string_of_xml subs)));
*)
  let env = Name_map.fold
    (fun (prefix,s) v acc ->
       (*       prerr_endline (Printf.sprintf "env: %s=%s" s v);*)
       env_add_att ~prefix s v acc)
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
              match get_arg_cdata atts ("",att_defer) with
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
;;

let apply_to_xmls data env xmls =
  (*prerr_endline (string_of_env env);*)
  let f (data, xmls) = eval_xmls [] data env xmls in
  fix_point_snd f (data, xmls)
;;

let apply_to_xml data env xml = apply_to_xmls data env [xml] ;;

let (apply_to_string : 'a -> 'a env -> string -> 'a * tree list) = fun data env s ->
  let xml = xml_of_string s in
  apply_to_xml data env xml
;;

let apply_to_file data env file =
  let s = string_of_file file in
  let xml = xml_of_string s in
  apply_to_xml data env xml
;;

let apply_into_file data ?head env ~infile ~outfile =
  let (data, xmls) = apply_to_file data env infile in
  let s = string_of_xmls xmls in
  let s = match head with None -> s | Some h -> h^s in
  file_of_string ~file: outfile s;
  data
;;

let apply_string_into_file data ?head env ~outfile s =
  let (data, xmls) = apply_to_string data env s in
  let s = string_of_xmls xmls in
  let s = match head with None -> s | Some h -> h^s in
  file_of_string ~file: outfile s;
  data
;;


let string_of_args args =
  String.concat " "
    (Name_map.fold
     (fun (pref,s) v acc ->
        let s = Printf.sprintf "%s%s=%S"
          (match pref with "" -> "" | p -> p^":") s (string_of_xmls v)
        in
        s :: acc
     )
       args []
    )
;;

let env_of_list ?(env=env_empty()) l =
  List.fold_right (fun ((prefix,name), f) env -> env_add ~prefix name f env) l env
;;

let opt_arg args ?(def=[]) name =
  match get_arg args name with None -> def | Some s -> s
;;


let opt_arg_cdata args ?(def="") name =
  match get_arg_cdata args name with None -> def | Some s -> s
;;

let atts_of_list =
  let f acc (name,v) = Name_map.add name v acc in
  fun ?(atts=atts_empty) l -> List.fold_left f atts l
;;

let atts_one ?(atts=atts_empty) name v = Name_map.add name v atts;;
let atts_replace = Name_map.add;;
let atts_remove = Name_map.remove;;
  