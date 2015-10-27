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

open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

module X = Xtmpl_rewrite
module Xml = Xtmpl_xml

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

let error loc msg = raise (Location.Error (Location.error ~loc msg))
let kerror loc = Printf.ksprintf (error loc)

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
    X.from_string str
  with
    Sys_error msg -> error loc (Printf.sprintf "File %S: %s" file msg)

type parameter =
  { name : Xtmpl_xml.name ;
    default : X.tree list option ;
    typ : [ `CData | `Xmls | `Other of string * string ] ;
    mlname : string option ;
  }

let string_of_name = function
  "", s -> s
| p, s -> Printf.sprintf "%s:%s" p s

let prune_param_atts =
  List.fold_right X.atts_remove
    [ "", "param_" ; "", "optional_" ; "", "type_" ; "", "to_xml_" ; "", "name_"]

let gather_params loc xmls =
  let rec add_param acc tag atts subs =
    let (acc, default) =
      match X.get_att_cdata atts ("","optional_") with
      | Some "true" ->
          let (acc, subs) = iter_list acc subs in
          (acc, Some subs)
      | _ ->
          (acc, None)
    in
    let typ =
      match X.get_att_cdata atts ("","type_") with
        None | Some "cdata" -> `CData
      | Some "xml"
      | Some "xmls" -> `Xmls
      | Some typ ->
          match X.get_att_cdata atts ("","to_xml_") with
            None -> error loc
              (Printf.sprintf "Missing to_xml attribute for param %S of type %S"
                 (string_of_name tag) typ)
          | Some code ->
            `Other (typ, code)
    in
    let mlname = X.get_att_cdata atts ("", "name_") in
    let acc = Xtmpl_xml.Name_map.add tag { name = tag ; default ; typ ; mlname } acc in
    let atts = prune_param_atts atts in
    (acc, X.node tag ~atts [])
  and iter acc xml =
    match xml with
      X.D _ | X.C _ | X.PI _ | X.X _ | X.DT _ -> (acc, xml)
    | X.E {X.name ; atts ; subs} ->
        match X.get_att_cdata atts ("","param_") with
        | Some "true" -> add_param acc name atts subs
        | _ ->
            let (acc, atts) = iter_atts acc atts in
            let (acc, subs) = iter_list acc subs in
            (acc, X.node name ~atts subs)
  and iter_list acc xmls =
    let (acc, xmls) = List.fold_left
              (fun (acc, acc_xmls) xml ->
                 let (acc, xml) = iter acc xml in
                 (acc, xml :: acc_xmls)
              )
              (acc, []) xmls
    in
    (acc, List.rev xmls)
  and iter_atts acc atts =
     Xml.Name_map.fold iter_att atts (acc, Xml.Name_map.empty)
  and iter_att name v (acc, atts) =
    let (acc, xmls) = iter_list acc v in
    (acc, Xml.Name_map.add name xmls atts)
  in
  iter_list Xml.Name_map.empty xmls

let parse_ocaml_expression loc str =
  let lexbuf = Lexing.from_string str in
  try Parse.expression lexbuf
  with e ->
    error loc
        (Printf.sprintf "Error while parsing the following OCaml expression:\n%s\n%s"
         str (Printexc.to_string e))

let parse_ocaml_type loc str =
  let lexbuf = Lexing.from_string str in
  try Parse.core_type lexbuf
  with e ->
    error loc
        (Printf.sprintf "Error while parsing the following OCaml type:\n%s\n%s"
         str (Printexc.to_string e))

let to_id = String.map
  (function
   | 'a'..'z' as c -> c
   | '0'..'9' as c -> c
   | 'A'..'Z' as c -> Char.lowercase c
   | _ -> '_')

let ml_id_of_param p =
  match p.mlname with
    Some s -> s
  | None ->
      match p.name with
      | "", s -> to_id s
      | p,s -> to_id p ^ "_" ^ to_id s

let fun_of_param loc body (name, p) =
  let id = ml_id_of_param p in
  let label =
    match p.default with
      None -> id
    | Some v ->  "?"^id
  in
  let pat = Pat.var ~loc (Location.mkloc id loc) in
  Exp.fun_ ~loc label None pat body

let funs_of_params loc params body =
  let exp = [%expr fun () -> [%e body]] in
  (* list parameters in reverse order to generate them in name order *)
  let params = Xml.Name_map.fold (fun name p acc -> (name, p) :: acc) params [] in
  let exp = List.fold_left (fun_of_param loc) exp params in
  [%expr fun ?(env=Xtmpl_rewrite.env_empty()) -> [%e exp]]

let env_or_defaults loc params exp =
 let f name p exp =
    let (prefix, str) = name in
    let e_prefix = Exp.constant (Const_string (prefix,None)) in
    let e_str = Exp.constant (Const_string (str,None)) in
    let id = ml_id_of_param p in
    let e_id = Exp.ident (lid loc id) in
    let e_name =
      let (p,s) = name in
      let const s = Exp.constant (Const_string (s, None)) in
      [%expr ([%e const p], [%e const s])]
    in
    let add_to_env exp =
      match p.typ with
      | `CData ->
          [%expr Xtmpl_rewrite.env_add_xml
            ~prefix: [%e e_prefix] [%e e_str] [Xtmpl_rewrite.cdata [%e exp] ] env
          ]
      | `Xmls -> [%expr Xtmpl_rewrite.env_add_xml ~prefix: [%e e_prefix] [%e e_str] [%e exp] env]
      | `Other (typ, f)->
          let to_xml = parse_ocaml_expression loc f in
          [%expr let v_ = ([%e to_xml]) [%e exp] in
            Xtmpl_rewrite.env_add_xml ~prefix: [%e e_prefix] [%e e_str] v_ env]
    in
    let default_def v =
      match p.typ, v with
      | `CData, [X.D v] -> Exp.constant (Const_string (v.Xml.text, None))
      | `CData, [] -> Exp.constant (Const_string ("", None))
      | `CData, _ ->
              error loc
            (Printf.sprintf "Parameter %S should have CData default value"
             (string_of_name name))
      | `Xmls, xmls -> Exp.ident (lid loc ("__default_"^id))
      | `Other _, [X.D code] ->
          parse_ocaml_expression loc code.Xml.text
      | `Other _, _ ->
          error loc
            (Printf.sprintf "Parameter %S should have OCaml code as default value (given as CDATA)"
             (string_of_name name))
    in
    match p.default with
    | None ->
        [%expr let [%p (Pat.var (Location.mkloc "env" loc))] = [%e (add_to_env e_id)] in [%e exp]]
    | Some default_xmls ->
        [%expr
          let env =
            match [%e e_id] with
              Some v -> [%e add_to_env (Exp.ident (lid loc "v"))]
            | None ->
                match Xtmpl_rewrite.env_get [%e e_name] env with
                  Some _ -> env
                | None -> [%e add_to_env (default_def default_xmls)]
          in
          [%e exp]
        ]
  in
  Xml.Name_map.fold f params exp

let defaults_of_params loc params exp =
  let f name p exp =
    match p.typ, p.default with
    | `Xmls, Some xmls ->
        let const_tmpl = Exp.constant ~loc
          (Const_string (X.to_string xmls, None))
        in
        let id = "__default_"^(ml_id_of_param p) in
        Exp.let_ Nonrecursive
          [Vb.mk (Pat.var (Location.mkloc id loc))
            [%expr Xtmpl_rewrite.from_string [%e const_tmpl]]
          ]
          exp
    | _ -> exp
  in
  Xml.Name_map.fold f params exp

let map_tmpl loc tmpl =
  let (params, tmpl) = gather_params loc tmpl in
  let const_tmpl = Exp.constant ~loc
    (Const_string (X.to_string tmpl, None)) in
  let call = [%expr let (_, res) = Xtmpl_rewrite.apply_to_xmls () env tmpl_ in res] in
  (*let envs = Xtmpl_rewrite.Name_map.fold (env_of_param loc) params call in*)
  let envs = env_or_defaults loc params call in
  let funs = funs_of_params loc params envs in
  let defaults = defaults_of_params loc params funs in
  let exp_tmpl = [%expr let tmpl_ = Xtmpl_rewrite.from_string [%e const_tmpl] in [%e defaults]] in
  exp_tmpl

let template_of_inline_string loc node exp =
  match exp.pexp_desc with
  | Pexp_constant (Const_string (str, _)) -> X.from_string str
  | _ -> kerror loc "String constant expected in %s extension node" node

let map_xtmpl_string exp =
  let loc = exp.pexp_loc in
  let tmpl = template_of_inline_string loc "xtmpl.string" exp in
  map_tmpl loc tmpl

let map_xtmpl exp =
  let loc = exp.pexp_loc in
  let file = file_path exp in
  let tmpl = read_template loc file in
  map_tmpl loc tmpl

let typ_of_params loc params =
  let f acc (name, p) =
    let opt = p.default <> None in
    let label = Printf.sprintf "%s%s"
      (if opt then "?" else "")
      (ml_id_of_param p)
    in
    let typ =
      let str = match p.typ with
        | `CData -> "string"
        | `Xmls -> "Xtmpl_rewrite.tree list"
        | `Other (typ, _) -> typ
      in
      let typ = parse_ocaml_type loc str in
      if opt then
        let lid_option = Location.mkloc (Ldot (Lident "*predef*","option")) loc in
        Typ.constr lid_option [typ]
      else
        typ
    in
    Typ.arrow label typ acc
  in
 (* list parameters in reverse order to generate them in name order *)
  let params = Xml.Name_map.fold (fun name p acc -> (name, p) :: acc) params [] in
  let typ = List.fold_left f [%type: unit -> Xtmpl_rewrite.tree list] params in
  [%type: ?env: unit Xtmpl_rewrite.env -> [%t typ] ]

let map_xtmpl_string_type exp =
  let loc = exp.pexp_loc in
  let tmpl = template_of_inline_string loc "xtmpl.string.type" exp in
  let (params, tmpl) = gather_params loc tmpl in
  typ_of_params loc params

let typ_mapper mapper typ =
  match typ.ptyp_desc with
  | Ptyp_extension ({ txt = "xtmpl.string.type" ; loc }, pstr) ->
      begin
        match pstr with
        | PStr [{ pstr_desc = Pstr_eval (exp, _) }] ->
            (* we expect an expression *)
            map_xtmpl_string_type exp
        | _ ->
            error loc "[%xtmpl.string.type] accepts a string"
      end
  | _ -> default_mapper.typ mapper typ

let expr_mapper mapper expr =
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
  | { pexp_desc = Pexp_extension ({ txt = "xtmpl.string"; loc }, pstr)} ->
      begin
        match pstr with
        | PStr [{ pstr_desc = Pstr_eval (exp, _) }] ->
            (* we expect an expression *)
            map_xtmpl_string exp
        | _ ->
            error loc "[%xtmpl.string] accepts a string"
      end
  | x -> default_mapper.expr mapper x

let xtmpl_mapper argv =
  { default_mapper with
    typ = typ_mapper ;
    expr = expr_mapper ;
  }

let () = register "xtmpl" xtmpl_mapper
