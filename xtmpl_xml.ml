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

type pos = { line: int ; bol : int; char: int ; file : string option }
type loc = { loc_start: pos; loc_stop: pos }
type 'a with_loc = 'a * loc option

type error = loc * string
exception Error of error
let error loc msg = raise (Error (loc, msg))

let string_of_loc loc =
  let start = loc.loc_start in
  let stop = loc.loc_stop in
  let line = start.line in
  let char = start.char - start.bol in
  let len =
    if start.file = stop.file then
      stop.char - start.char
    else
      1
  in
  let file = start.file in
  Printf.sprintf "%sline %d, character%s %d%s"
    (match file with
     | None -> ""
     | Some s -> Printf.sprintf "File %s, " s)
    line
    (if len > 1 then "s" else "")
    char
    (if len > 1 then Printf.sprintf "-%d" (char + len) else "")

let loc_sprintf loc fmt =
  match loc with
  | None -> Printf.sprintf fmt
  | Some loc -> Printf.ksprintf
      (fun s -> Printf.sprintf "%s:\n%s" (string_of_loc loc) s)
      fmt

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
      if p + 1 < len then String.sub str (p+1) (len - (p + 1)) else ""
    in
    (prefix, suffix)
  with
    Not_found -> ("", str)

let loc loc_start loc_stop = { loc_start ; loc_stop }
let loc_of_pos pos len =
  { loc_start = pos ;
    loc_stop = { pos with char = pos.char + len } ;
  }

let nl_code = Char.code '\n'

let update_pos pos str =
  let f pos i = function
  | `Malformed msg -> error (loc_of_pos pos 1) msg
  | `Uchar c when c = nl_code ->
      let bol = pos.char in
      { pos with
        line = pos.line + 1;
        bol ;
        char = pos.char + 1 ;
      }
  | _ -> { pos with char = pos.char + 1}
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
type proc_inst = { loc: loc option; app: name; args: string}
type 'a attributes = 'a Name_map.t
type str_attributes = string with_loc attributes
type xml_decl = { loc: loc option; atts: str_attributes }
type doctype = { loc: loc option; name: name; args: string}
type node = { loc: loc option; name: name ; atts: str_attributes ; subs: tree list }
and tree =
| E of node
| D of cdata
| C of comment
| PI of proc_inst

type prolog_misc = PC of comment | PPI of proc_inst
type prolog = {
      decl : xml_decl option ;
      misc : prolog_misc list ;
      doctype : doctype option ;
    }
type 'a doc = { prolog : prolog ; elements : 'a list }

let atts_empty = Name_map.empty
let node ?loc name ?(atts=atts_empty) subs = E { loc; name; atts; subs}
let cdata ?loc ?(quoted=false) text = D { loc ; text ; quoted }
let merge_cdata (c1 : cdata) (c2 : cdata) =
  let loc =
    match c1.loc, c2.loc with
      None, _ | _, None -> None
    | Some l1, Some l2 -> Some (loc l1.loc_start l2.loc_stop)
  in
  { loc ; text = c1.text ^ c2.text ;
    quoted = c1.quoted || c2.quoted ;
  }

let comment ?loc comment = C { loc ; comment }
let prolog_comment ?loc comment = PC { loc ; comment }
let pi ?loc app args = PI { loc ; app ; args }
let prolog_pi ?loc app args = PPI { loc ; app ; args }
let xml_decl ?loc atts : xml_decl = { loc ; atts }
let doctype ?loc name args : doctype = { loc ; name ; args }
let prolog ?decl ?doctype misc = { decl ; doctype ; misc }
let doc prolog elements = { prolog ; elements }

type stack = (pos * name * str_attributes) Stack.t

let e_nameStartChar = [%sedlex.regexp? ":" | 'A'..'Z' | "_" | 'a'..'z' | 0xC0..0xD6 | 0xD8..0xF6 | 0xF8..0x02FF | 0x0370..0x037D | 0x037F..0x1FFF | 0x200C..0x200D | 0x2070..0x218F | 0x2C00..0x2FEF | 0x3001..0xD7FF | 0xF900..0xFDCF | 0xFDF0..0xFFFD | 0x010000..0x0EFFFF]
let e_nameChar = [%sedlex.regexp? e_nameStartChar | "-" | "." | '0'..'9' | 0xB7 | 0x0300..0x036F | 0x203F..0x2040]

let e_name = [%sedlex.regexp? e_nameStartChar , Star(e_nameChar)]
let e_space = [%sedlex.regexp? 	Plus(0x20 | 0x9 | 0xD | 0xA)]

let e_char_no_minus = [%sedlex.regexp?  0x9 | 0xA | 0xD | 0x20..0x2C | 0x2E..0xD7FF | 0xE000..0xFFFD | 0x10000..0x10FFFF]
let e_char = [%sedlex.regexp? e_char_no_minus | '-']

let e_charRef = [%sedlex.regexp?
    ("&#", Plus('0'..'9'), ';') | ("&#x", Plus('0'..'9'|'a'..'f'|'A'..'F'), ';')]
let e_entityRef = [%sedlex.regexp? '&',e_name,';']
let e_reference = [%sedlex.regexp? e_entityRef | e_charRef]
let e_attValueChar =
  [%sedlex.regexp? 0x00..0x25| 0x27..0x3B | 0x3D..0x0EFFFF]
let e_attValueChar_noquot =
  [%sedlex.regexp? 0x00..0x21 | 0x23..0x25| 0x27..0x3B | 0x3D..0x0EFFFF]
let e_attValueChar_noapos =
  [%sedlex.regexp? 0x00..0x25| 0x28..0x3B | 0x3D..0x0EFFFF]

let e_attValue = [%sedlex.regexp?
    '"', Star(e_attValueChar_noquot | e_reference), '"'
  | "'", Star(e_attValueChar_noapos | e_reference), "'"
  ]

let e_xml = [%sedlex.regexp? ('x'|'X'),('m'|'M'),('l'|'L')]

let map_string lexer str =
  let buf = Buffer.create (String.length str) in
  lexer buf (U.from_string str);
  Buffer.contents buf

let cp_to_string cp =
  let b = Buffer.create 10 in
  Uutf.Buffer.add_utf_8 b cp ;
  Buffer.contents b
let unescape =
  let add = Buffer.add_string in
  let rec iter entities buf lb =
    match%sedlex lb with
    | "&lt;" ->
        if entities then add buf "<" else add buf (U.lexeme lb);
        iter entities buf lb
    | "&gt;" ->
        if entities then add buf ">" else add buf (U.lexeme lb);
        iter entities buf lb
    | "&amp;" ->
        if entities then add buf "&" else add buf (U.lexeme lb);
        iter entities buf lb
    | "&quot;" ->
        if entities then add buf "\"" else add buf (U.lexeme lb);
        iter entities buf lb
    | "&apos;" ->
        if entities then add buf "'" else add buf (U.lexeme lb);
        iter entities buf lb

    | "&#", Plus('0'..'9'), ';' ->
        let lexeme = U.lexeme lb in
        let s =
          try
            let n =
              let len = String.length lexeme in
              let s = String.sub lexeme 2 (len - 3) in
              int_of_string s
            in
            cp_to_string n
          with _ -> lexeme
        in
        add buf s ;
        iter entities buf lb
    | "&#x", Plus('0'..'9'|'a'..'f'|'A'..'F'), ';' ->
        let lexeme = U.lexeme lb in
        let s =
          try
            let n =
              let len = String.length lexeme in
              let s = "0"^(String.sub lexeme 2 (len - 3)) in
              int_of_string s
            in
            cp_to_string n
          with
            _ -> lexeme
        in
        add buf s;
        iter entities buf lb
    | any -> add buf (U.lexeme lb); iter entities buf lb
    | _ -> ()
  in
  fun ?(entities=true) -> map_string (iter entities)

let escape =
  let add = Buffer.add_string in
  let rec iter quotes buf lb =
    match%sedlex lb with
    | "<" -> add buf "&lt;"; iter quotes buf lb
    | ">" -> add buf "&gt;"; iter quotes buf lb
    | "&" -> add buf "&amp;"; iter quotes buf lb
    | "\"" ->
        if quotes
        then add buf "&quot;"
        else add buf (U.lexeme lb);
        iter quotes buf lb
    | "'" ->
        if quotes
        then add buf "&apos;"
        else add buf (U.lexeme lb);
        iter quotes buf lb
    | any -> add buf (U.lexeme lb); iter quotes buf lb
    | _ -> ()
  in
  fun ?(quotes=false) -> map_string (iter quotes)

let rec parse_comment pos buf lb =
  match%sedlex lb with
    "-->" -> unescape (Buffer.contents buf)
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
    "]]>" -> unescape ~entities: false (Buffer.contents buf)
  | e_char ->
      Buffer.add_string buf (U.lexeme lb);
      parse_cdata pos buf lb
  | any ->
      error (loc_of_pos pos 1) ("Invalid cdata character: "^(U.lexeme lb))
  | _ ->
      let pos = update_pos pos (Buffer.contents buf) in
      error (loc_of_pos pos 1) "Unexpected end of stream while parsing cdata"

let rec parse_proc_inst pos buf lb =
  match%sedlex lb with
    "?>" ->
      let args = unescape (Buffer.contents buf) in
      let pos = update_pos pos (U.lexeme lb) in
      let args = Xtmpl_misc.strip_string args in
      (args, pos)
  | e_char ->
      Buffer.add_string buf (U.lexeme lb);
      parse_proc_inst pos buf lb
  | any ->
      error (loc_of_pos pos 1) ("Invalid character in processing instruction: "^(U.lexeme lb))
  | _ ->
      let pos = update_pos pos (Buffer.contents buf) in
      error (loc_of_pos pos 1)
        "Unexpected end of stream while parsing processing instruction"

let rec parse_doctype pos buf lb =
  match%sedlex lb with
    ">" ->
      let args = unescape (Buffer.contents buf) in
      let pos = update_pos pos (U.lexeme lb) in
      let args = Xtmpl_misc.strip_string args in
      (args, pos)
  | e_char ->
      Buffer.add_string buf (U.lexeme lb);
      parse_doctype pos buf lb
  | any ->
      error (loc_of_pos pos 1) ("Invalid character in doctype decl: "^(U.lexeme lb))
  | _ ->
      let pos = update_pos pos (Buffer.contents buf) in
      error (loc_of_pos pos 1)
        "Unexpected end of stream while parsing doctype decl"

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
         let loc = loc pos_start pos_end in
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
      let loc = loc pos pos2 in
      let stack = add_elt stack (comment ~loc text) in
      parse_text stack pos2 lb

  | "<![CDATA[" ->
      let pos = update_pos_from_lb pos lb in
      let text = parse_cdata pos (Buffer.create 256) lb in
      let pos2 = update_pos pos text in
      (* update pos2 with the "]]>" lexeme just read *)
      let pos2 = update_pos_from_lb pos2 lb in
      let loc = loc pos pos2 in
      let stack = add_elt stack (cdata ~loc ~quoted: true text) in
      parse_text stack pos2 lb

  | "<?",e_name ->
      let pos2 = update_pos_from_lb pos lb in
      let app =
        let s = U.lexeme lb in
        let len = String.length s in
        name_of_string (String.sub s 2 (len - 2))
      in
      begin
        match app with
          ("", s) when String.lowercase s = "xml" ->
            let loc = loc pos pos2 in
            error loc "Illegal XML declaration here"
        | _ ->
            let (args, pos2) = parse_proc_inst pos2 (Buffer.create 256) lb in
            let loc = loc pos pos2 in
            let stack = add_elt stack (pi ~loc app args) in
            parse_text stack pos2 lb
      end
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
           let loc = loc pos pos2 in
           let elt = node ~loc ~atts name [] in
           add_elt stack elt
          )
        else
          push stack pos name atts
      in
      parse_text stack pos2 lb
  | "</",e_name,Star(e_space),'>' ->
      let lexeme = U.lexeme lb in
      let len = String.length lexeme in
      let name = String.sub lexeme 2 (len - 3) in
      let name = name_of_string (Xtmpl_misc.strip_string name) in
      let pos2 = update_pos_from_lb pos lb in
      let stack = pop stack pos2 name in
      parse_text stack pos2 lb
  | "]]>" ->
      error (loc_of_pos pos 3)
        ("Invalid sequence in character data: "^(U.lexeme lb))
  | Plus(e_attValueChar | e_reference) ->
      let str = unescape (U.lexeme lb) in
      let pos2 = update_pos_from_lb pos lb in
      let loc = loc pos pos2 in
      let stack = add_elt stack (cdata ~loc str) in
      parse_text stack pos2 lb
  | '<', any ->
      let pos2 = update_pos_from_lb pos lb in
      error (loc pos pos2)
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

and parse_attributes ?(xml_decl=false) map pos lb =
  match%sedlex lb with
  | e_space -> parse_attributes ~xml_decl map (update_pos_from_lb pos lb) lb
  | e_name ->
       let name = name_of_string (U.lexeme lb) in
       let pos = update_pos_from_lb pos lb in
       let (att_value, pos2) = parse_attribute_eq pos lb in
       let map = Name_map.add name att_value map in
       parse_attributes ~xml_decl map pos2 lb
  | "?>" ->
      if xml_decl then
        (map, update_pos_from_lb pos lb, true)
      else
        error (loc_of_pos pos 2)
          ("Unexpected characters: "^(U.lexeme lb))
  | '>' ->
      if xml_decl then
        error (loc_of_pos pos 1)
          ("Unexpected character: "^(U.lexeme lb))
      else
        (map, update_pos_from_lb pos lb, false)
  | "/>" ->
      if xml_decl then
        error (loc_of_pos pos 2)
          ("Unexpected characters: "^(U.lexeme lb))
      else
        (map, update_pos_from_lb pos lb, true)
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
    let v = unescape (String.sub lexeme 1 (len - 2)) in
    let loc = loc pos pos2 in
    ((v, Some loc), pos2)
  | any ->
      error (loc_of_pos pos 1)
        ("Unexpected character: "^(U.lexeme lb))
  | _ ->
      error (loc_of_pos pos 1)
        "Unexpected end of stream while parsing attribute value"

let new_stack pos_start = [(("",""), pos_start, atts_empty), []]

let rec parse_prolog ?xml_decl misc pos lb =
  match%sedlex lb with
  | "<!DOCTYPE",Plus(e_space) ->
      let pos2 = update_pos_from_lb pos lb in
      let name = match%sedlex lb with
        | e_name -> name_of_string (U.lexeme lb)
        | _ ->
            error (loc_of_pos pos 1)
              ("Invalid character in doctype decl: "^(U.lexeme lb))
      in
      let (args, pos2) = parse_doctype pos2 (Buffer.create 256) lb in
      let loc = loc pos pos2 in
      let doctype = doctype ~loc name args in
      let prolog = prolog ?decl: xml_decl ~doctype (List.rev misc) in
      let elements = parse_text (new_stack pos2) pos2 lb in
      doc prolog elements

  | "<!--" ->
      let pos = update_pos_from_lb pos lb in
      let text = parse_comment pos (Buffer.create 256) lb in
      let pos2 = update_pos pos text in
      (* update pos2 with the "-->" lexeme just read *)
      let pos2 = update_pos_from_lb pos2 lb in
      let loc = loc pos pos2 in
      let comment = prolog_comment ~loc text in
      parse_prolog ?xml_decl (comment::misc) pos2 lb

  | "<?",e_name ->
      let pos2 = update_pos_from_lb pos lb in
      let app =
        let s = U.lexeme lb in
        let len = String.length s in
        name_of_string (String.sub s 2 (len - 2))
      in
      begin
        match app with
          ("", s) when String.lowercase s = "xml" ->
            let loc = loc pos pos2 in
            error loc "Illegal XML declaration here"
        | _ ->
            let (args, pos2) = parse_proc_inst pos2 (Buffer.create 256) lb in
            let loc = loc pos pos2 in
            let pi = prolog_pi ~loc app args in
            parse_prolog ?xml_decl (pi :: misc) pos2 lb
      end
  | e_space ->
      let pos2 = update_pos_from_lb pos lb in
      parse_prolog ?xml_decl misc pos2 lb
  | '<',e_name ->
      Sedlexing.rollback lb ;
      let prolog = prolog ?decl: xml_decl (List.rev misc) in
      let elements = parse_text (new_stack pos) pos lb in
      doc prolog elements
  | _ ->
      let pos2 = update_pos_from_lb pos lb in
      let loc = loc pos pos2 in
      error loc (Printf.sprintf "Illegal character %S" (U.lexeme lb))

let parse_doc pos lb =
  match%sedlex lb with
  | "<?",e_xml,Plus(e_space) ->
      let pos2 = update_pos_from_lb pos lb in
      let (atts, pos2, _) = parse_attributes
        ~xml_decl: true atts_empty pos2 lb
      in
      let loc = loc pos pos2 in
      let xml_decl = xml_decl ~loc atts in
      parse_prolog ~xml_decl [] pos2 lb
  | _ ->
      Sedlexing.rollback lb ;
      parse_prolog [] pos lb

let get_att atts name =
  try Some (Name_map.find name atts)
  with Not_found -> None

let opt_att atts ?(def="") name =
  match get_att atts name with None -> (def, None) | Some s -> s

let print_att buf name value =
  Printf.bprintf buf "%s=\"%s\"" (string_of_name name)
    (escape ~quotes: true value)

let xmlns_name = ("","xmlns") ;;
let version_name = ("", "version") ;;

let rec print_tree buf = function
| C { comment } ->
    Printf.bprintf buf "<!--%s-->" (escape comment)
| D { text ; quoted = true } ->
    Printf.bprintf buf "<![CDATA[%s]]>" text
| D { text ; quoted = false } ->
    Printf.bprintf buf "%s" (escape text)
| PI { app ; args } ->
    Printf.bprintf buf "<?%s %s?>"
      (string_of_name app) (escape args)


| E { name ; atts ; subs } ->
    Printf.bprintf buf "<%s" (string_of_name name);
    Name_map.iter
      (fun name (value,_) ->
         Buffer.add_string buf " ";
         print_att buf name value
      )
      atts;
    match subs with
      [] -> Buffer.add_string buf "/>"
    | _ ->
        Buffer.add_string buf ">";
        List.iter (print_tree buf) subs;
        Printf.bprintf buf "</%s>" (string_of_name name)

let print_prolog_misc buf = function
| PC { comment } ->
    Printf.bprintf buf "<!--%s-->\n" (escape comment)
| PPI { app ; args } ->
    Printf.bprintf buf "<?%s %s?>\n"
      (string_of_name app) (escape args)

let print_prolog buf p =
  (match p.decl with
   | None -> ()
   | Some { atts } ->
       Printf.bprintf buf "<?xml " ;
       let (v, _) = opt_att ~def: "1.0" atts version_name in
       print_att buf version_name v;
       Name_map.iter
         (fun name (value,_) ->
            if name <> version_name then
              (
               Buffer.add_string buf " ";
               print_att buf name value
              )
         )
         atts;
       Buffer.add_string buf "?>\n"
  );
  List.iter (print_prolog_misc buf) p.misc;
  (
   match p.doctype with
   | None -> ()
   | Some { name ; args } ->
       Printf.bprintf buf "<!DOCTYPE %s %s>\n"
         (string_of_name name) (escape args)
  )

let print_doc buf elt_to_string doc =
  print_prolog buf doc.prolog ;
  List.iter (elt_to_string buf) doc.elements

let string_of_xml t =
  let buf = Buffer.create 512 in
  print_tree buf t ;
  Buffer.contents buf

let to_string l =
  let buf = Buffer.create 512 in
  List.iter (print_tree buf) l;
  Buffer.contents buf

let doc_to_string_ f doc =
  let buf = Buffer.create 512 in
  print_doc buf f doc;
  Buffer.contents buf

let doc_to_string = doc_to_string_ print_tree

let from_lexbuf
  ?(pos_start={ line = 1; char = 1 ; bol = 0 ; file = None }) lb =
    parse_text (new_stack pos_start)
    pos_start lb

let doc_from_lexbuf
  ?(pos_start={ line = 1; char = 1 ; bol = 0 ; file = None }) lb =
    parse_doc pos_start lb

let from_string ?pos_start str =
  let lb=  U.from_string str in
  from_lexbuf ?pos_start lb

let doc_from_string ?pos_start str =
  let lb=  U.from_string str in
  doc_from_lexbuf ?pos_start lb

let from_channel ?pos_start ic =
  let lb=  U.from_channel ic in
  from_lexbuf ?pos_start lb

let doc_from_channel ?pos_start ic =
  let lb=  U.from_channel ic in
  doc_from_lexbuf ?pos_start lb

let from_file file =
  let ic = open_in_bin file in
  let pos_start= { line = 1; char = 1 ; bol = 0 ; file = Some file } in
  try let xmls = from_channel ~pos_start ic in close_in ic; xmls
  with e ->
    close_in ic;
    raise e

let doc_from_file file =
  let ic = open_in_bin file in
  let pos_start= { line = 1; char = 1 ; bol = 0 ; file = Some file } in
  try let xmls = doc_from_channel ~pos_start ic in close_in ic; xmls
  with e ->
    close_in ic;
    raise e

let atts_of_list =
  let f acc (name,v) = Name_map.add name v acc in
  fun ?(atts=atts_empty) l -> List.fold_left f atts l

let atts_one ?(atts=atts_empty) name v = Name_map.add name v atts;;
let atts_replace = Name_map.add;;
let atts_remove = Name_map.remove;;

let string_of_atts atts =
  let buf = Buffer.create 256 in
  Name_map.iter
    (fun name (value,_) ->
         Buffer.add_string buf " ";
         print_att buf name value
    )
    atts;
    Buffer.contents buf
(*
let xml = {|<?xml version='1' ?>
  <!DOCTYPE toto sdkfsdl>
  <!--hello comment !-->
   <?myapp tralalalal?>
   bla bl <strong title="coucou&lt;">bla</strong> foo bar|}
let xml = Xtmpl_misc.string_of_file Sys.argv.(1)
let tree =
  try
    let xmls = from_string xml in
    print_endline (to_string xmls)
  with
  Error e ->
      prerr_endline (string_of_error e)
*)



