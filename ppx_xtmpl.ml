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

open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let lid loc s = Location.mkloc (Longident.parse s) loc

(*c==v=[File.string_of_file]=1.1====*)
let string_of_file name =
  let chanin = open_in_bin name in
  let len = 1024 in
  let s = Bytes.create len in
  let buf = Buffer.create len in
  let rec iter () =
    try
      let n = input chanin s 0 len in
      if n = 0 then
        ()
      else
        (
         Buffer.add_subbytes buf s 0 n;
         iter ()
        )
    with
      End_of_file -> ()
  in
  iter ();
  close_in chanin;
  Buffer.contents buf
(*/c==v=[File.string_of_file]=1.1====*)

let error loc msg =
  raise (Location.Error (Location.error ~loc msg))

let file_path exp =
  let loc = exp.pexp_loc in
  let base_path =
    match loc.Location.loc_start.Lexing.pos_fname with
    | "" -> Filename.current_dir_name
    | f -> Filename.dirname f
  in
  match exp.pexp_desc with
  | Pexp_constant (Const_string (file, _)) ->
      begin
        match Filename.is_relative file with
        | true -> Filename.concat base_path file
        | false -> file
      end
  | _ -> error loc "String constant expected in %xtmpl extension node"

let read_template loc file =
  try
    let str = string_of_file file in
    match Xtmpl.xml_of_string str with
      Xtmpl.E(_,_,xmls) -> xmls
    | _ -> assert false
  with
    Sys_error msg -> error loc (Printf.sprintf "File %S: %s" file msg)

type parameter =
  { default : Xtmpl.tree list option ;
    typ : [ `CData | `Xmls | `Other of string * string ] ;
  }

let string_of_name = function
  "", s -> s
| p, s -> Printf.sprintf "%s:%s" p s

let prune_param_atts =
  List.fold_right Xtmpl.atts_remove
    [ "", "param" ; "", "optional" ; "", "type" ; "", "to_xml"]

let gather_params loc xmls =
  let rec add_param acc tag atts subs =
    let (acc, default) =
      match Xtmpl.get_arg_cdata atts ("","optional") with
      | Some "true" ->
          let (acc, subs) = iter_list acc subs in
          (acc, Some subs)
      | _ ->
          (acc, None)
    in
    let typ =
      match Xtmpl.get_arg_cdata atts ("","type") with
        None | Some "cdata" -> `CData
      | Some "xml" -> `Xmls
      | Some typ ->
          match Xtmpl.get_arg_cdata atts ("","to_xml") with
            None -> error loc
              (Printf.sprintf "Missing to_xml attribute for param %S of type %S"
                 (string_of_name tag) typ)
          | Some code ->
            `Other (typ, code)
    in
    let acc = Xtmpl.Name_map.add tag { default ; typ } acc in
    let atts = prune_param_atts atts in
    (acc, Xtmpl.E (tag, atts, []))
  and iter acc xml =
    match xml with
      Xtmpl.D _ -> (acc, xml)
    | Xtmpl.E (tag, atts, subs) ->
        match Xtmpl.get_arg_cdata atts ("","param") with
        | Some "true" -> add_param acc tag atts subs
        | _ ->
            let (acc, subs) = iter_list acc subs in
            (acc, Xtmpl.E(tag,atts,subs))
  and iter_list acc xmls =
    let (acc, xmls) = List.fold_left
              (fun (acc, acc_xmls) xml ->
                 let (acc, xml) = iter acc xml in
                 (acc, xml :: acc_xmls)
              )
              (acc, []) xmls
    in
    (acc, List.rev xmls)
  in
  iter_list Xtmpl.Name_map.empty xmls

let parse_ocaml_expression loc str =
  let lexbuf = Lexing.from_string str in
  try Parser.parse_expression Lexer.token_with_comments lexbuf
  with e ->
    error loc
        (Printf.sprintf "Error while parsing the following OCaml code:\n%s\n%s"
         str (Printexc.to_string e))

let to_id = String.map
  (function
   | 'a'..'z' as c -> c
   | '0'..'9' as c -> c
   | 'A'..'Z' as c -> Char.lowercase c
   | _ -> '_')

let id_of_param_name = function
| "", s -> to_id s
| p,s -> to_id p ^ "_" ^ to_id s

let fun_of_param loc name p body =
  let id = id_of_param_name name in
  let (label, default) =
    match p.default with
      None -> id, None
    | Some v ->
        let label = "?"^id in
        let def =
          match p.typ, v with
          | `CData, [Xtmpl.D v] -> Exp.constant (Const_string (v, None))
          | `CData, [] -> Exp.constant (Const_string ("", None))
          | `CData, _ ->
              error loc
                (Printf.sprintf "Parameter %s should have CData default value"
                 (string_of_name name))
          | `Xmls, xmls -> Exp.ident (lid loc ("__default_"^id))
          | `Other _, [Xtmpl.D code] ->
              parse_ocaml_expression loc code
          | `Other _, _ ->
              error loc
                (Printf.sprintf "Parameter %s should have OCaml code as default value (given as CDATA)"
                 (string_of_name name))
        in
        (label, Some def)
  in
  let pat = Pat.var ~loc (Location.mkloc id loc) in
  Exp.fun_ ~loc label default pat body

let funs_of_params loc params body =
  let exp = [%expr fun () -> [%e body]] in
  let exp = Xtmpl.Name_map.fold (fun_of_param loc) params exp in
  [%expr fun ?(env=Xtmpl.env_empty()) -> [%e exp]]

let env_of_param loc ((prefix,str) as name) p exp =
  let e_prefix = Exp.constant (Const_string (prefix,None)) in
  let e_str = Exp.constant (Const_string (str,None)) in
  let id = id_of_param_name name in
  let e_id = Exp.ident (lid loc id) in
  let def =
    match p.typ with
    | `CData -> [%expr Xtmpl.env_add_att ~prefix: [%e e_prefix] [%e e_str] [Xtmpl.D [%e e_id]] env]
    | `Xmls -> [%expr Xtmpl.env_add_att ~prefix: [%e e_prefix] [%e e_str] [%e e_id] env]
    | `Other (typ, f)->
        let to_xml = parse_ocaml_expression loc f in
        [%expr let v_ = ([%e to_xml]) [%e e_id] in
               Xtmpl.env_add_att ~prefix: [%e e_prefix] [%e e_str] v_ env]
  in
  [%expr let env = [%e def] in [%e exp]]

let defaults_of_params loc params exp =
  let f name p exp =
    match p.typ, p.default with
    | `Xmls, Some xmls ->
        let const_tmpl = Exp.constant ~loc (Const_string (Xtmpl.string_of_xmls xmls, None)) in
        let id = "__default_"^(id_of_param_name name) in
        Exp.let_ Nonrecursive
          [Vb.mk (Pat.var (Location.mkloc id loc))
            [%expr [Xtmpl.xml_of_string [%e const_tmpl]]]
          ]
          exp
    | _ -> exp
  in
  Xtmpl.Name_map.fold f params exp

let map_xtmpl exp =
  let loc = exp.pexp_loc in
  let file = file_path exp in
  let tmpl = read_template loc file in
  let (params, tmpl) = gather_params loc tmpl in
  let const_tmpl = Exp.constant ~loc (Const_string (Xtmpl.string_of_xmls tmpl, None)) in
  let call = [%expr let (_, res) = Xtmpl.apply_to_xmls () env [tmpl_] in res] in
  let envs = Xtmpl.Name_map.fold (env_of_param loc) params call in
  let funs = funs_of_params loc params envs in
  let defaults = defaults_of_params loc params funs in
  let exp_tmpl = [%expr let tmpl_ = Xtmpl.xml_of_string [%e const_tmpl] in [%e defaults]] in
  exp_tmpl


let getenv_mapper argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt = "xtmpl"; loc }, pstr)} ->
          begin
            match pstr with
            | PStr [{ pstr_desc = Pstr_eval (exp, _) }] ->
                (* we expect an expression *)
                map_xtmpl exp
            | _ ->
                error loc "[%xtmpl] accepts a string"
          end
      | x -> default_mapper.expr mapper x;
  }

let () = register "xtmpl" getenv_mapper
