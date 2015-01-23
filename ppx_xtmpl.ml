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
      Xtmpl.E(_,_,xmls) -> (str, xmls)
    | _ -> assert false
  with
    Sys_error msg -> error loc (Printf.sprintf "File %S: %s" file msg)

let gather_params xmls =
  ([], xmls)

let make_funs params body =
  body

let map_xtmpl exp =
  let loc = exp.pexp_loc in
  let file = file_path exp in
  let (str, tmpl) = read_template loc file in
  let (params, tmpl) = gather_params tmpl in
  let const_tmpl = Exp.constant ~loc (Const_string (str, None)) in
  let lid_xml_of_string = lid loc "Xtmpl.xml_of_string" in
  let body = assert false in
  let funs = make_funs params body in
  let exp_tmpl = [%expr let tmpl_ = [%e (Exp.ident lid_xml_of_string)] [%e const_tmpl] in [%e funs]] in
  assert false


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
