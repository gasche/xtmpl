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

module E = Sedlexing.Latin1

let add_lexeme buf lexbuf =
  Buffer.add_string buf (E.lexeme lexbuf)

let rec main buf lexbuf =
  match%sedlex lexbuf with
  | "<:" -> start buf lexbuf; main buf lexbuf
  | eof -> ()
  | any -> add_lexeme buf lexbuf; main buf lexbuf
  | _ -> assert false

and start buf lexbuf =
  match%sedlex lexbuf with
  | Plus('a'..'z'|'A'..'Z') ->
      let id = E.lexeme lexbuf in
      Printf.bprintf buf "(Xtmpl.E((\"\",%S)," id ;
      start_arg buf lexbuf
  | _ ->
      Buffer.add_string buf "(Xtmpl.E((\"\",\"\")," ;
      start_arg buf lexbuf

and start_arg buf lexbuf =
  match%sedlex lexbuf with
  | '<' ->
      Buffer.add_string buf "Xtmpl.atts_empty, [ ";
      xml_items buf lexbuf ;
      Buffer.add_string buf "]))"
  | _ -> main buf lexbuf

and xml_items buf lexbuf =
  match%sedlex lexbuf with
  | ">>" -> ()
  | "<:" ->
      start buf lexbuf ;
      Buffer.add_string buf " ; ";
      xml_items buf lexbuf
  | "[[" ->
      Buffer.add_string buf "(Xtmpl.D (";
      pcdata buf lexbuf ;
      Buffer.add_string buf " ; ";
      xml_items buf lexbuf
  | any -> add_lexeme buf lexbuf; xml_items buf lexbuf
  | _ -> assert false

and pcdata buf lexbuf =
  match%sedlex lexbuf with
  | "]]" -> Buffer.add_string buf "))"
  | any -> add_lexeme buf lexbuf; pcdata buf lexbuf
  | _ -> assert false


let lex lb =
  let buf = Buffer.create 256 in
  main buf lb;
  Buffer.contents buf

let () = print_string (lex (E.from_channel stdin))
