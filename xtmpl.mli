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

(** Xml templating library.

    Rewrite XML {!type:tree}s using {!type:callback} rules
    provided by the {!type:env} environment.

    A complete description of the templating rules is available in the
    {!section:engine} section below.
*)

(** This exception can be raised by callbacks to indicate that the
  node to be rewritten remains unchanged. *)
exception No_change

type name = string * string

module Name_ord : Map.OrderedType with type t = name
module Name_map : Map.S with type key = name
module Name_set : Set.S with type elt = name

type 'a env
and 'a callback = 'a -> 'a env -> attributes -> tree list -> 'a * tree list
and tree =
    E of name * attributes * tree list
  | D of string
and attributes = tree list Name_map.t

type rewrite_stack = (name * attributes * tree list) list
exception Loop of  rewrite_stack

val string_of_stack : rewrite_stack -> string

val empty_atts : attributes

(** {2 Environment}

    An {!type:env} is a {!type:name}-to-{!type:callback} associative map. In addition
    to basic manipulation functions, the functions {!val:env_add_att} and
    {!val:env_of_list} provide convenient shortcuts for common operations.

    The environments are immutable, all mutating operations return new
    environments.
*)

(** An environment that contains only one binding, associating
  {!val:tag_main} to a function returning its subnodes.
*)
val env_empty : unit -> 'a env

(** Add a binding to an environment.

    [env_add "double" (fun _ _ xml -> xml @ xml)] binds the key
    [("", "double")] to a callback that doubles an XML subtree.

    [env_add ~prefix: "foo" "double" (fun _ _ xml -> xml @ xml)] does the same but
    for the key  [("foo", "double")].

    If the same key was already bound, the previous binding is
    replaced.

    @param prefix is [""] by default.
*)
val env_add : ?prefix: string -> string -> 'a callback -> 'a env -> 'a env

(** Get a binding from an environment.

    If the binding is not found, returns [None].
*)
val env_get : name -> 'a env -> 'a callback option

(** String representation of all the keys in the environment. *)
val string_of_env : 'a env -> string

(** Bind a callback that returns some XML.

    The most frequent operation performed by a callback is to return a
    constant XML subtree. This convenience function lets you provide
    the XML subtree as a string.

    [env_add_att "logo" "<img src=\"logo.png\"/>" env] binds the key
    [("","logo")] to a callback that returns an XHTML image tag.

    Note that the provided XML is automatically wrapped in the
    {!val:tag_main} tag, which will cause the corresponding
    templating rules to be applied to it
*)
val env_add_att : ?prefix:string -> string -> tree list -> 'a env -> 'a env

(** Add several bindings at once.

    This convenience function saves you the effort of calling
    {!val:env_add} several times yourself.

    [env_of_list ~env:env [ (ns1, k1), f1 ; (ns2, k2), f2 ]] is equivalent to
    [env_add ~prefix: ns1 k1 f1 (env_add ~prefix: ns2 k2 f2 env)]. This means that one key
    is present twice in the list, the first association in the list
    will hide the second one in the resulting environment.

    @param env The environment to which bindings are added. If
    not provided, {!val:env_empty} is used.
*)
val env_of_list : ?env: 'a env -> (name * 'a callback) list -> 'a env

(** {2 XML Manipulation} *)

(** A dummy tag, currently ["main_"]. It is used when parsing a
   string as an XML tree, to ensure that we will parse a tree and
   not a list of trees. For this purpose, the tag is used to enclosed
   a string before parsing it.

    Used by {!val:env_add_att}, {!val:xml_of_string} and, most importantly,
    by the [apply_*] functions.
*)
val tag_main : string

(** The environment tag, currently ["env_"].

    See the template rules in the templating engine section below for more
    information about this tag.
*)
val tag_env : string

(** Output an XML string.

    The returned string does not include the initial [<?xml ... ?>].
*)
val string_of_xml : tree -> string

(** Use {!string_of_xml} to concatenate a list of xml trees into one string,
  with no separator.*)
val string_of_xmls : tree list -> string

val string_of_xml_atts : attributes -> (name * string) list

(** Parses a string as XML.

    @param add_main if true, adds [<main_>..</main_>] around the string
    (see {!val:tag_main}).
    @raise Failure when a parsing error occurs, includes the source string.
*)
val xml_of_string : ?add_main:bool -> string -> tree

val xmls_of_atts : (name * string) list -> attributes

(** Same as {!xml_of_string} but read from a file. *)
val xml_of_file : string -> tree

(** {2:engine Templating engine}

    The [apply_*] functions apply a given environment to XML tree(s). These
    trees are given as parameter ({!apply_to_xmls}) or can be read from a
    file ({!apply_to_file}), or a string ({!apply_to_string}).

    The functions return the result of the rewrite as XML trees, or
    can write it to a file ({!apply_into_file}).

    The rewrite rules are applied until a fix-point is reached.

    {b I.} A single iteration descends recursively into the XML tree. If an
    element has a callback associated in the environment, then the
    callback is applied to the node's attributes and children.

    {i Example}: consider the following XML:

    {v <album author="Rammstein" name="Reise, Reise">
  <track>Los</track>
  <track>Mein Teil</track>
</album> v}

    This would look for a callback bound to [("","album")] in the environment
    and call it using [callback env [("","author"),"Rammstein"; ("","name"),"Reise, Reise"] xml]
    where [env] is the current environment and [xml] represents the
    two children [ <track>..</track> ] elements.

    {b II.} The callback returns a new list of elements that is used
    instead of the old element.

    {i Example}: assuming that [env_add "x2" (fun _ _ xml -> xml @ xml)],
    then [<x2>A</x2>] is rewritten as [AA].

    {b III.} The engine then recursively descends into those replaced
    elements (this means that a poorly conceived rule set may well never
    terminate).

    {i Example}: [<x2><x2>A</x2></x2>] is first rewritten as
    [<x2>A</x2><x2>A</x2>], and then as [AAAA].

    {b IV.} The [env_] and [main_] elements (see {!val:tag_env}
    and {!val:tag_main}) are a special case: both are automatically
    replaced with their children (as if their callback was
    [(fun _ _ xml -> xml)]).

    [env_] effectively changes the environment
    used when processing its children by adding the bindings defined by
    its arguments (using {!val:env_add_att}, hence the name).

    {i Example}: [<env_ a="&lt;b&gt;A&lt;/b&gt;"><a/></env_>] is
    replaced by [<a/>], which in turn is replaced by
    [<b>A</b>].

    {b V.} If an element has a [defer_] attribute (that is greater
    than zero), then it is not processed and the attribute is decremented
    by one, and the process recursively applies to its children.

    {i Example}: [<x2 defer_="1"><x2>A</x2></x2>] is rewritten as
    [<x2 defer_="0">AA</x2>]. The {b next} iteration will effectively apply the
    rule to the node and return [AAAA].
*)

(** Applies as many iterations as necessary to a piece of XML (represented
    as an unparsed string) to reach a fix-point.

    See above for how an iteration is applied.
*)
val apply_to_string : 'a -> 'a env -> string -> 'a * tree list

(** As {!val:apply_to_string}, but reads the XML from a file. *)
val apply_to_file : 'a -> 'a env -> string -> 'a * tree list

(** As {!val:apply_to_string}, but applies to a list of XML trees.*)
val apply_to_xmls : 'a -> 'a env -> tree list -> 'a * tree list

(** As {!val:apply_to_file}, but writes the result back to a file.

    For instance, [apply_to_file env ~infile:"source.xml" ~outfile: "dest.xml"].

    @param head Prepend this string to the XML that is output
    to the file. By default, nothing is prepended.
*)
val apply_into_file : 'a -> ?head:string -> 'a env -> infile: string -> outfile: string -> 'a

(** As {!val:apply_into_file}, but read the XML from a string instead of a file.
*)
val apply_string_into_file : 'a -> ?head:string -> 'a env -> outfile: string -> string -> 'a

(** {2 Utilities}

    Several useful functions when workin with XML.
*)

(** Finds a binding in an attribute list.

    This performs the same as [List.assoc], but returns an optional string
    instead of raising [Not_found].
*)
val get_arg : attributes -> name -> tree list option

val get_arg_cdata : attributes -> name -> string option

(** A string representation of an argument list.

    [string_of_args ["a","x";"b","y"]] returns [a="x" b="y"]. Note that
    the argument names are output verbatim, but the argument values are
    escaped with the [%S] format.
*)
val string_of_args : attributes -> string

(** Finds a binding in an associative list, or returns a default.

    This performs the same as [List.assoc], but returns the provided
    default value instead of raising [Not_found].

    @param def Default value, returned for missing bindings. If not
    provided, an empty string is used.
*)
val opt_arg : attributes -> ?def:tree list -> name -> tree list

val opt_arg_cdata : attributes -> ?def:string -> name -> string

val atts_of_list : ?atts: attributes -> (name * tree list) list -> attributes
val one_att : ?atts: attributes -> name -> tree list -> attributes
val remove_att : name -> attributes -> attributes
val replace_att : name -> tree list -> attributes -> attributes
