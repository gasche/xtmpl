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

let string_of_name = function
  ("",s) -> s
| (p, s) -> p ^ ":" ^ s

let name_of_string str =
  try
    let p = String.index str ':' in
    let len = String.length str in
    let prefix = String.sub str 0 p in
    let suffix =
      if p + 1 < len then String.sub str p (len - (p + 1)) else ""
    in
    (prefix, suffix)
  with
    Not_found -> ("", str)

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

type cdata = { loc: loc option; text: string ; quoted: bool}
type comment = { loc: loc option; comment: string }
type attributes = (string * loc option) Name_map.t
type node = { loc: loc option; name: name ; atts: attributes ; subs: tree list }
and tree =
| E of node
| D of cdata
| C of comment

let atts_empty = Name_map.empty
let node ?loc name ?(atts=atts_empty) subs = E { loc; name; atts; subs}
let cdata ?loc ?(quoted=false) text = D { loc ; text ; quoted }
let comment ?loc comment = C { loc ; comment }

type stack = (pos * name * attributes) Stack.t

let e_nameStartChar = [%sedlex.regexp? ":" | 'A'..'Z' | "_" | 'a'..'z' | 0xC0..0xD6 | 0xD8..0xF6 | 0xF8..0x02FF | 0x0370..0x037D | 0x037F..0x1FFF | 0x200C..0x200D | 0x2070..0x218F | 0x2C00..0x2FEF | 0x3001..0xD7FF | 0xF900..0xFDCF | 0xFDF0..0xFFFD | 0x010000..0x0EFFFF]
let e_nameChar = [%sedlex.regexp? e_nameStartChar | "-" | "." | '0'..'9' | 0xB7 | 0x0300..0x036F | 0x203F..0x2040]

let e_name = [%sedlex.regexp? e_nameStartChar , Star(e_nameChar)]
let e_space = [%sedlex.regexp? 	Plus(0x20 | 0x9 | 0xD | 0xA)]

let e_char_no_minus = [%sedlex.regexp?  0x9 | 0xA | 0xD | 0x20..0x2C | 0x2E..0xD7FF | 0xE000..0xFFFD | 0x10000..0x10FFFF]
let e_char = [%sedlex.regexp? e_char_no_minus | '-']

let e_charRef = [%sedlex.regexp?
    ("&#", Plus('0'..'0'), ';') | ("&#x", Plus('0'..'9'|'a'..'f'|'A'..'F'), ';')]
let e_entityRef = [%sedlex.regexp? '&',e_name,';']
let e_reference = [%sedlex.regexp? e_entityRef | e_charRef]
let e_attValueChar = [%sedlex.regexp? 0x00..0x25| 0x27..0x3B | 0x3D..0x0EFFFF]

let e_attValue = [%sedlex.regexp?
    '"', Star(e_attValueChar | e_reference), '"'
  | "'", Star(e_attValueChar | e_reference), "'"
  ]

let rec parse_comment pos buf lb =
  match%sedlex lb with
    "-->" -> Buffer.contents buf
  | e_char_no_minus | '-', e_char_no_minus ->
      Buffer.add_string buf (U.lexeme lb);
      parse_comment pos buf lb
  | any ->
      error (loc_of_pos pos 1) ("Invalid comment character: "^(U.lexeme lb))
  | _ ->
      let pos = update_pos pos (Buffer.contents buf) in
      error (loc_of_pos pos 1) "Unexpected end of stream while parsing comment"

let rec parse_cdata pos buf lb =
  match%sedlex lb with
    "]]>" -> Buffer.contents buf
  | e_char ->
      Buffer.add_string buf (U.lexeme lb);
      parse_cdata pos buf lb
  | any ->
      error (loc_of_pos pos 1) ("Invalid cdata character: "^(U.lexeme lb))
  | _ ->
      let pos = update_pos pos (Buffer.contents buf) in
      error (loc_of_pos pos 1) "Unexpected end of stream while parsing cdata"

let add_elt stack elt =
  match stack with
  | [] -> assert false
  | (x,l) :: q -> (x, elt :: l) :: q

let push stack pos_start name attributes =
  ((name, pos_start, attributes), []) :: stack

let pop stack pos_end name =
  match stack with
  | [] -> assert false
  | ((n,pos_start,atts), subs) :: q ->
      if n = name then
        (
         let loc = loc_of_pos2 pos_start pos_end in
         let elt = node ~loc ~atts name (List.rev subs) in
         add_elt q elt
        )
      else
        error (loc_of_pos pos_end 1)
          (Printf.sprintf "Found </%s> instead of </%s>"
           (string_of_name name) (string_of_name n))

let rec parse_text stack pos lb =
  match%sedlex lb with
    "<!--" ->
      let pos = update_pos_from_lb pos lb in
      let text = parse_comment pos (Buffer.create 256) lb in
      let pos2 = update_pos pos text in
      (* update pos2 with the "-->" lexeme just read *)
      let pos2 = update_pos_from_lb pos2 lb in
      let loc = loc_of_pos2 pos pos2 in
      let stack = add_elt stack (comment ~loc text) in
      parse_text stack pos2 lb

  | "<![CDATA[" ->
      let pos = update_pos_from_lb pos lb in
      let text = parse_cdata pos (Buffer.create 256) lb in
      let pos2 = update_pos pos text in
      (* update pos2 with the "]]>" lexeme just read *)
      let pos2 = update_pos_from_lb pos2 lb in
      let loc = loc_of_pos2 pos pos2 in
      let stack = add_elt stack (cdata ~loc text) in
      parse_text stack pos2 lb

  | '<',e_name ->
      let name =
        let s = U.lexeme lb in
        let len = String.length s in
        name_of_string (String.sub s 1 (len - 1))
      in
      let pos2 = update_pos_from_lb pos lb in
      let (atts, pos2, closed) = parse_attributes atts_empty pos2 lb in
      let stack =
        if closed then
          (
           let loc = loc_of_pos2 pos pos2 in
           let elt = node ~loc ~atts name [] in
           add_elt stack elt
          )
        else
          push stack pos name atts
      in
      parse_text stack pos2 lb
  | "</",e_name,'>' ->
      let lexeme = U.lexeme lb in
      let len = String.length lexeme in
      let name = name_of_string (String.sub lexeme 2 (len - 3)) in
      let pos2 = update_pos_from_lb pos lb in
      let stack = pop stack pos2 name in
      parse_text stack pos2 lb
  | "]]>" ->
      error (loc_of_pos pos 3)
        ("Invalid sequence in character data: "^(U.lexeme lb))
  | Plus(e_attValueChar) ->
      let lexeme = U.lexeme lb in
      let pos2 = update_pos_from_lb pos lb in
      let loc = loc_of_pos2 pos pos2 in
      let stack = add_elt stack (cdata ~loc lexeme) in
      parse_text stack pos2 lb
  | '<', any ->
      let pos2 = update_pos_from_lb pos lb in
      error (loc_of_pos2 pos pos2)
        (Printf.sprintf "Unexpected characters: %s" (U.lexeme lb))
  | any ->
      error (loc_of_pos pos 1) "Unexpected characters from this point"
  | _ ->
      match stack with
        [] -> assert false
      | ((name,_,_),_) :: _ :: _ ->
          error (loc_of_pos pos 1)
            (Printf.sprintf "Element not terminated: %s"
             (string_of_name name))
      | [_,subs] ->
          List.rev subs

and parse_attributes map pos lb =
  match%sedlex lb with
  | e_space -> parse_attributes map (update_pos_from_lb pos lb) lb
  | e_name ->
       let name = name_of_string (U.lexeme lb) in
       let pos = update_pos_from_lb pos lb in
       let (att_value, pos2) = parse_attribute_eq pos lb in
       let map = Name_map.add name att_value map in
       parse_attributes map pos2 lb
  | '>' -> (map, update_pos_from_lb pos lb, false)
  | "/>" -> (map, update_pos_from_lb pos lb, true)
  | any ->
      error (loc_of_pos pos 1)
        ("Unexpected character in attribute list: "^(U.lexeme lb))
  | _ -> error (loc_of_pos pos 1) "Unexpected end of stream while parsing attributes"

and parse_attribute_eq pos lb =
  match%sedlex lb with
  | e_space -> parse_attribute_eq (update_pos_from_lb pos lb) lb
  | '=', Star(e_space) -> parse_attribute_value pos lb
  | any ->
      error (loc_of_pos pos 1)
        ("Unexpected character: "^(U.lexeme lb)^"; '=' was expected")
  | _ ->
      error (loc_of_pos pos 1)
        "Unexpected end of stream while parsing attribute"

and parse_attribute_value pos lb =
  match%sedlex lb with
  | e_attValue ->
    let lexeme = U.lexeme lb in
    let pos2 = update_pos_from_lb pos lb in
    let len = String.length lexeme in
    let v = String.sub lexeme 1 (len - 2) in
    let loc = loc_of_pos2 pos pos2 in
    ((v, Some loc), pos2)
  | any ->
      error (loc_of_pos pos 1)
        ("Unexpected character: "^(U.lexeme lb))
  | _ ->
      error (loc_of_pos pos 1)
        "Unexpected end of stream while parsing attribute value"

let xml = {|<!--hello comment !-->bla bl <strong>bla</strong> |}
let tree =
  try
    let pos_start =
      { pline = 1; pchar = 1 ; pbol = 0 ; pfile = None }
    in
    let xmls =
      parse_text [(("",""), pos_start, atts_empty), []]
        pos_start
        (U.from_string xml)
    in
    ignore(xmls)
  with
  Error e ->
      prerr_endline (string_of_error e)




