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

(** A name is a pair [(prefix, s)], where prefix can be the empty string.
  When the prefix is not empty, it corresponds to the notation [prefix:foo]
  for XML tags and attribute names.*)
type name = string * string

module Name_ord : Map.OrderedType with type t = name

(** Mapping over names. *)
module Name_map : Map.S with type key = name

(** Sets of names. *)
module Name_set : Set.S with type elt = name

type 'a env
and 'a callback = 'a -> 'a env -> attributes -> tree list -> 'a * tree list
  (** A ['a callback] takes data of type ['a], a ['a env], attributes and XML nodes.
    It returns a potentially new data of type ['a] and XML nodes. The attributes are
    the attributes of the node being rewritten, and the XML trees in parameter are the
    XML children of this node. The environment must be a ['a env] so that
    if can be used in the callback to perform rewriting, returning a
    new data of type ['a]. *)

and tree =
    E of name * attributes * tree list
  | D of string
and attributes = tree list Name_map.t
  (** Attributes of a XML node. Note that the content value of an attribute is
      a list of XML trees, not just a string. *)

(** Recursively merge sibling [D] nodes into one [D] node. *)
val merge_cdata : tree -> tree

(** Same as {!merge_cdata} but taking a {!tree} list. *)
val merge_cdata_list : tree list -> tree list

(** To catch eventual infinite loops in rewriting, we keep a
  stack of the rules called. *)
type rewrite_stack = (name * attributes * tree list) list

(** The [Loop] exception is raised when the rewrite stack
  is higher than a default value of 100. This value can be changed
  by setting the [XTMPL_REWRITE_DEPTH_LIMIT] environment variable.
  Default value is [100]. *)
exception Loop of  rewrite_stack

(** String representation of the given rewrite stack. *)
val string_of_stack : rewrite_stack -> string

(** {2 Attributes}

Here are some convenient functions to work with attributes: building, querying. *)

(** Empty {!attributes} structure. *)
val atts_empty : attributes

(** [atts_of_list list] returns an {!attributes} structure from the given
  list of pairs [(name, xml tree list) ; (name, xml tree list) ; ...].
  The last  pair of the list is added first (using [List.fold_right]).
  @param atts can be used to specify an existing {!attributes} structure to
  add bindings to, instead of starting from an empty one. *)
val atts_of_list : ?atts: attributes -> (name * tree list) list -> attributes

(** [atts_one ?atts name xmls] is like {!atts_of_list} but for one attribute. *)
val atts_one : ?atts: attributes -> name -> tree list -> attributes

(** [atts_remove name attributes] removes the binding to [name] from the
  [attributes].*)
val atts_remove : name -> attributes -> attributes

(** [atts_replace name xmls attributes] adds a new bindings from [name] to
  [xmls] in [attributes]. If [name] was previously bound, the previous
  binding is removed.
  [atts_replace ~atts name xmls] is equivalent to [atts_replace name xmls atts].
*)
val atts_replace : name -> tree list -> attributes -> attributes

(** [get_arg attributes name] returns the xml tree list bound to [name] in
  [attributes]. Return [None] if no binding was found for [name]. *)
val get_arg : attributes -> name -> tree list option

(** Same as {!get_arg} but return a string [s] only if [name] is bound to
  a single CDATA XML node ([[D s]]).
  In particular, if [name] is bound to a list of XML tree, or to
  a single tree which is not a CDATA, the function returns [None]. *)
val get_arg_cdata : attributes -> name -> string option

(** [opt_arg attributes ?def name] returns the tree list associated to [name]
  in the [attributes] or a default value. This default value can be
  specified with the [def] parameter. If no default value is specified, then
  the empty list is used.
*)
val opt_arg : attributes -> ?def:tree list -> name -> tree list

(** Same as {!opt_arg} but looking for CDATA bounded values, as in
  {!get_arg_cdata}.*)
val opt_arg_cdata : attributes -> ?def:string -> name -> string


(** Return a string representation of attributes, in the form
    [a="foo" b="bar" ...].
    Note that the argument names are output verbatim, but the
    argument values are escaped with the [%S] format.
*)
val string_of_args : attributes -> string

(** {2 Environment}

    An {!type:env} is a {!type:name}-to-{!type:callback} associative map. In addition
    to basic manipulation functions, the functions {!val:env_add_att} and
    {!val:env_of_list} provide convenient shortcuts for common operations.

    The environments are immutable, all mutating operations return new
    environments.
*)

(** An environment that contains only one binding, associating
  {!val:tag_main} to a function such as [(fun data _env _atts subs -> (data, subs))],
  i.e. only returning the given data and subnodes.
*)
val env_empty : unit -> 'a env

(** Add a binding to an environment.

    [env_add "double" (fun acc _ _ xml -> (acc, xml @ xml))] binds the key
    [("", "double")] to a callback that doubles an XML subtree.

    [env_add ~prefix: "foo" "double" (fun acc _ _ xml -> (acc, xml @ xml))] does the same but
    for the key  [("foo", "double")].

    If the same key was already bound, the previous binding is replaced.

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

    The most frequent operation performed by a callback is to return
    constant XML subtrees. This convenience function lets you provide
    the XML subtrees.

    [env_add_att "logo" [ E (("","img"), atts_one ("","src") [D "logo.png"], []) ] env]
    binds the key [("","logo")] to a callback that returns an XHTML image tag.

    @param prefix can be used to specify a prefix for the rule name. Default is [""].
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
    not provided, {!val:env_empty}[ ()] is used.
*)
val env_of_list : ?env: 'a env -> (name * 'a callback) list -> 'a env

(** {2 XML Manipulation} *)

(** A dummy tag, currently ["main_"]. It is used when parsing a
   string as an XML tree, to ensure that we will parse a tree and
   not a list of trees. For this purpose, the tag is used to enclosed
   a string before parsing it.

   Used by {!val:xml_of_string} and, most importantly, by the
     {!apply_to_string}, {!apply_to_file}, {!apply_into_file} and
     {!apply_string_into_file} functions.
*)
val tag_main : string

(** The environment tag, currently ["env_"].

    See the template rules in the {!section:engine} section below for more
    information about this tag.
*)
val tag_env : string

(** The defer attribute, currently ["defer_"]. See the engine section
  for details. *)
val att_defer : string

(** The escamp attribute, currently ["escamp_"]. This attribute
  is used when converting XML to string or reading XML from a string.
  The CDATA associated to this attribute indicates the other attributes
  in which the ampersands must be escaped (when parsing XML from an
  attribute string) or unescaped (when printing XML to an attribute string).
  This is useful for urls with &, for example in [<a href="...">] nodes.

  Example: In [<a escamp_="href", href="http://foo.fr?v1=3amp;v2=4">...</a>].
  As attributes are parsed as XML, setting the [escamp_] attribute to ["href"]
  will make the ampersand escaped. The attribute is kept during rewriting.
  When the XML tree will be converted to a string, the [escamp_] attribute
  will be removed. Several attribute names can be indicated, using [',']
  or [';'] as separator, as in
  [ <a escamp_="href, foo, gee:buz" href="..." ...>...</a> ].

  This treatment is done in {!xmls_of_atts} (escaping) and {!string_of_xml_atts} (unescaping).
*)
val att_escamp : string

(** The protect attribute, currently "protect_". See the engine section
  for details. This attribute is removed when a XML tree is converted
  to a string, as for {!att_escamp}.
*)
val att_protect : string

(** Output an XML string.

    The returned string does not include the initial [<?xml ... ?>].

    @param xml_atts indicates whether the code in attributes remains valid XML
    of not. Default is [true] but it should be set to [false] when outputting
    final documents like HTML pages.
*)
val string_of_xml : ?xml_atts: bool -> tree -> string

(** Use {!string_of_xml} to concatenate a list of xml trees into one string,
  with no separator.*)
val string_of_xmls : ?xml_atts: bool -> tree list -> string

(** Return a list of strings to represent the given attributes. *)
val string_of_xml_atts : ?xml_atts: bool -> attributes -> (name * string) list

(** Parses a string as XML.

    @param add_main if true (default), adds [<main_>..</main_>] around the string
    (see {!val:tag_main}).
    @raise Failure when a parsing error occurs, including the source string.
*)
val xml_of_string : ?add_main:bool -> string -> tree

(** Convert "string attributes" to an {!attributes} structure. The value
  associated to each name must be valid XML. See also {!att_escamp}. *)
val xmls_of_atts : (name * string) list -> attributes

(** Same as {!xml_of_string} but read from a file. *)
val xml_of_file : string -> tree

(** {2:engine Templating engine}

    The [apply_*] functions apply a given environment and data to XML tree(s). These
    trees are given as parameter ({!apply_to_xmls}) or can be read from a
    file ({!apply_to_file}), or a string ({!apply_to_string}).

    The functions return the result of the rewrite as XML trees, or
    can write it to a file ({!apply_into_file}). They also return data
    as the result of the callbacks called, as in a classic fold function (callbacks
    take the data in parameter, as the environment, the attributes and subnodes
    of the rewritten node).

    The rewrite rules are applied until a fix-point is reached.
    If the [XTMPL_FIXPOINT_LIMIT] environment variable contains a valid integer [n],
    it is used as a fix-point limit: if no fix-point is reached in [n] iterations,
    then a [Failure] exception is raised.

{ol
{- A single iteration descends recursively into the XML tree. If an
    element has a callback associated in the environment, then the
    callback is applied to the current data and the node's attributes and children.

    {i Example}: consider the following XML:

    {v <album author="Rammstein" name="Reise, Reise">
  <track>Los</track>
  <track>Mein Teil</track>
</album> v}

    This would look for a callback bound to [("","album")] in the environment
    and call it using
    [callback data env {("","author")->[ D "Rammstein"]|("","name")->[D "Reise, Reise"]} xml]
    where [env] is the current environment and [xml] represents the
    two children [ <track>..</track> ] elements.
}
{- The callback returns a pair composed of (maybe new) data
     and a new list of elements that is used instead of the old element.

    {i Example}: assuming that the environnement was build using
    [env_add "x2" (fun data _ _ xml -> (data, xml @ xml)) env],
    then [<x2>A</x2>] is rewritten as [AA].
}
{- The engine then recursively descends into those replaced
    elements (this means that a poorly conceived rule set may well never
    terminate).

    {i Example}: [<x2><x2>A</x2></x2>] is first rewritten as
    [<x2>A</x2><x2>A</x2>], and then as [AAAA].
}
{-  The [env_] and [main_] elements (see {!val:tag_env}
    and {!val:tag_main}) are a special case: both are automatically
    replaced with their children (as if their callback was
    [(fun data _ _ xml -> (data, xml))]).

    [env_] effectively changes the environment
    used when processing its children by adding the bindings defined by
    its arguments (using {!val:env_add_att}, hence the name).

    {i Example}: [<env_ a="&lt;b&gt;A&lt;/b&gt;"><a/></env_>] is
    replaced by [<a/>], which in turn is replaced by
    [<b>A</b>].
}
{- If an element has a [defer_] attribute (that is greater
    than zero), then it is not processed and the attribute is decremented
    by one, and the process recursively applies to its children.

    {i Example}: [<x2 defer_="1"><x2>A</x2></x2>] is rewritten as
    [<x2 defer_="0">AA</x2>]. The {b next} iteration will effectively apply the
    rule to the node and return [AAAA].
}
{-  If an element has a [protect_] attribute, then the value
    must be CDATA and contains a list of names to remove from the environment
    when descending in the children. The names are separated by [','] or [';'],
    for example: [<foo protect_="title,id,foo:bar" ..>...</foo>].
}
}
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

    For instance, [apply_to_file data env ~infile:"source.xml" ~outfile: "dest.xml"].

    @param head Prepend this string to the XML that is output
    to the file. By default, nothing is prepended.
*)
val apply_into_file : 'a -> ?head:string -> 'a env -> infile: string -> outfile: string -> 'a

(** As {!val:apply_into_file}, but read the XML from a string instead of a file.
*)
val apply_string_into_file : 'a -> ?head:string -> 'a env -> outfile: string -> string -> 'a


