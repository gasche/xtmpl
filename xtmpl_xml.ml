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

module S = Sedlexing
module U = Sedlexing.Utf8

type name = string * string

type loc = { line: int ; char: int ; len: int ; file: string option }
type pos = { pline: int ; pbol : int; pchar: int ; pfile : string option }

type error = loc * string
exception Error of error
let error loc msg = raise (Error (loc, msg))

let string_of_loc loc =
  Printf.sprintf "%sline %d, character%s %d%s"
    (match loc.file with
     | None -> ""
     | Some s -> Printf.sprintf "File %s, " s)
    loc.line
    (if loc.len > 1 then "s" else "")
    loc.char
    (if loc.len > 1 then Printf.sprintf "-%d" (loc.char + loc.len) else "")

let string_of_error (loc, str) =
  Printf.sprintf "%s: %s" (string_of_loc loc) str

let loc_of_pos pos len =
  { line = pos.pline;
    char = pos.pchar - pos.pbol ;
    len ;
    file = pos.pfile ;
  }

let loc_of_pos2 pos pos2 =
  { line = pos.pline;
    char = pos.pchar - pos.pbol ;
    len = pos2.pchar - pos.pchar ;
    file = pos.pfile ;
  }

let nl_code = Char.code '\n'

let update_pos pos str =
  let f pos i = function
  | `Malformed msg -> error (loc_of_pos pos 1) msg
  | `Uchar c when c = nl_code ->
      let pbol = pos.pchar in
      { pos with
        pline = pos.pline + 1;
        pbol ;
        pchar = pos.pchar + 1 ;
      }
  | _ -> { pos with pchar = pos.pchar + 1}
  in
  Uutf.String.fold_utf_8 f pos str

let update_pos_from_lb pos lb = update_pos pos (U.lexeme lb)

module Name_ord = struct
  type t = name
  let compare (p1,s1) (p2,s2) =
    match String.compare s1 s2 with
      0 -> String.compare p1 p2
    | n -> n
  end

module Name_map = Map.Make (Name_ord)
module Name_set = Set.Make (Name_ord)

type cdata = { loc: loc option; text: string }
type attributes = string Name_map.t
type node = { loc: loc option; name: name ; atts: attributes ; subs: tree list }
and tree =
| E of node
| D of cdata
| C of cdata

let atts_empty = Name_map.empty
let node ?loc name ?(atts=atts_empty) subs = E { loc; name; atts; subs}
let cdata ?loc text = D { loc ; text }
let comment ?loc text = C { loc ; text }

type stack = (pos * name * attributes) Stack.t


let rec parse_comment pos buf lb =
  match%sedlex lb with
    "-->" -> Buffer.contents buf
  | any ->
      Buffer.add_string buf (U.lexeme lb);
      parse_comment pos buf lb
  | _ ->
      let pos = update_pos pos (Buffer.contents buf) in
      error (loc_of_pos pos 1) "Unexpected end of stream while parsing comment"

let rec parse_cdata pos buf lb =
  match%sedlex lb with
    "]]>" -> Buffer.contents buf
  | any ->
      Buffer.add_string buf (U.lexeme lb);
      parse_cdata pos buf lb
  | _ ->
      let pos = update_pos pos (Buffer.contents buf) in
      error (loc_of_pos pos 1) "Unexpected end of stream while parsing cdata"

let rec parse_tree_list acc pos lb =
  match%sedlex lb with
    "<!--" ->
      let pos = update_pos_from_lb pos lb in
      let text = parse_comment pos (Buffer.create 256) lb in
      let pos2 = update_pos pos text in
      (* update pos2 with the "-->" lexeme just read *)
      let pos2 = update_pos_from_lb pos2 lb in
      let loc = loc_of_pos2 pos pos2 in
      let acc = (comment ~loc text) :: acc in
      parse_tree_list acc pos2 lb

  | "<[CDATA[" ->
      let pos = update_pos_from_lb pos lb in
      let text = parse_cdata pos (Buffer.create 256) lb in
      let pos2 = update_pos pos text in
      (* update pos2 with the "]]>" lexeme just read *)
      let pos2 = update_pos_from_lb pos2 lb in
      let loc = loc_of_pos2 pos pos2 in
      let acc = (cdata ~loc text) :: acc in
      parse_tree_list acc pos2 lb

  | '<' ->
      let pos2 = update_pos_from_lb pos lb in
      let (node, pos2) = parse_node pos2 lb in
      let acc = node :: acc in
      parse_tree_list acc pos2 lb
  | any ->
      error (loc_of_pos pos 1) ("Unhandled character stream: "^(U.lexeme lb))
  | _ ->
      error (loc_of_pos pos 1) "Unexpected end of stream"

and parse_node pos lb =
  match%sedlex lb with
  | _ -> error (loc_of_pos pos 1) "parsing nodes is not implemented yet!"
  
let xml = {|<!--hello comment !-->bla bl bla |}
let tree =
  try
    parse_tree_list []
      { pline = 1; pchar = 1 ; pbol = 0 ; pfile = None }
      (U.from_string xml)
  with
  Error e ->
      prerr_endline (string_of_error e)




