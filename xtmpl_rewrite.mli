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

(** Rewriting XML trees.

    Rewrite XML {!type:tree}s using {!type:callback} rules
    provided by the {!type:env} environment.

    A complete description of the templating rules is available in the
    {!section:engine} section below.
*)

module Xml = Xtmpl_xml

(** {2 XML trees} *)

type name = string * string

(** Attributes of a XML node in a rewrite tree.
       Note that the content value of an attribute is
       a list of XML trees, not just a string. *)
type attributes = tree list Xml.attributes

and node =
  { loc: Xml.loc option;
    name: name ;
    atts: attributes ;
    subs: tree list ;
  }
and tree =
| E of node (** XML element *)
| D of Xml.cdata  (** CDATA *)
| C of Xml.comment  (** Comment *)
| PI of Xml.proc_inst  (** Processing instruction *)
| X of Xml.xml_decl  (** XML declaration *)
| DT of Xml.doctype  (** Doctype *)

(** {2 Constructors} *)


val node : ?loc:Xml.loc -> name -> ?atts:attributes -> tree list -> tree
val cdata : ?loc:Xml.loc -> ?quoted:bool -> string -> tree
val comment : ?loc:Xml.loc -> string -> tree
val proc_inst : ?loc:Xml.loc -> name -> string -> tree
val xml_decl : ?loc:Xml.loc -> Xml.str_attributes -> tree
val doctype : ?loc:Xml.loc -> name -> string -> tree

(** {2:attributes Attributes} *)

(** Empty {!attributes} structure. *)
val atts_empty : attributes

(** [atts_of_list list] returns an {!attributes} structure from the given
  list of pairs [(name, xml tree list) ; (name, xml tree list) ; ...].
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
  binding is removed.*)
val atts_replace : name -> tree list -> attributes -> attributes

(** [get_att attributes name] returns the xml tree list bound to [name] in
  [attributes]. Return [None] if no binding was found for [name].
*)
val get_att : attributes -> name -> tree list option

(** Same as {!get_att} but return a string [s] only if [name] is bound to
  a single CDATA XML node ([[D s]]).
  In particular, if [name] is bound to a list of XML tree, or to
  a single tree which is not a CDATA, the function returns [None].
*)
val get_att_cdata : attributes -> name -> string option

(** [opt_att attributes ?def name] returns the tree list associated to [name]
  in the [attributes] or a default value. This default value can be
  specified with the [def] parameter. If no default value is specified, then
  the empty list is used.
*)
val opt_att : attributes -> ?def:tree list -> name -> tree list

(** Same as {!opt_att} but looking for CDATA bounded values, as in
  {!get_att_cdata}.
*)
val opt_att_cdata : attributes -> ?def:string -> name -> string

(** {2 Environment}

    An {!type:env} is a {!type:name}-to-{!type:callback} associative map. In addition
    to basic manipulation functions, the functions {!val:env_add_xml} and
    {!val:env_of_list} provide convenient shortcuts for common operations.

    The environments are immutable, all mutating operations return new
    environments.
*)
type 'a env = ('a callback) Xml.Name_map.t
and 'a callback = 'a -> 'a env -> attributes -> tree list -> 'a * tree list

(** This exception can be raised by callbacks to indicate that the
  node to be rewritten remains unchanged. *)
exception No_change

(** An environment that contains only one binding, associating
  {!val:tag_main} to a function such as [(fun data _env _atts subs -> (data, subs))],
  i.e. only returning the given data and subnodes.
*)
val env_empty : unit -> 'a env

(** Add a binding to an environment.

    [env_add_cb "double" (fun acc _ _ xml -> (acc, xml @ xml))] binds the key
    [("", "double")] to a callback that doubles an XML subtree.

    [env_add_cb ~prefix: "foo" "double" (fun acc _ _ xml -> (acc, xml @ xml))] does the same but
    for the key  [("foo", "double")].

    If the same key was already bound, the previous binding is replaced.

    @param prefix is [""] by default.
*)
val env_add_cb : ?prefix: string -> string -> 'a callback -> 'a env -> 'a env

(** Bind a callback that returns some XML.

    The most frequent operation performed by a callback is to return
    constant XML subtrees. This convenience function lets you provide
    the XML subtrees.

    [env_add_xml "logo" [ E (("","img"), atts_one ("","src") [D "logo.png"], []) ] env]
    binds the key [("","logo")] to a callback that returns an XHTML image tag.

    @param prefix can be used to specify a prefix for the rule name. Default is [""].
*)
val env_add_xml : ?prefix:string -> string -> tree list -> 'a env -> 'a env

(** Get a binding from an environment.

    If the binding is not found, returns [None].
*)
val env_get : name -> 'a env -> 'a callback option

(** String representation of all the keys in the environment. *)
val string_of_env : 'a env -> string

(** Add several bindings at once.

    This convenience function saves you the effort of calling
    {!val:env_add_cb} several times yourself.

    [env_of_list ~env:env [ (ns1, k1), f1 ; (ns2, k2), f2 ]] is equivalent to
    [env_add_cb ~prefix: ns1 k1 f1 (env_add_cb ~prefix: ns2 k2 f2 env)]. This means that one key
    is present twice in the list, the first association in the list
    will hide the second one in the resulting environment.

    @param env The environment to which bindings are added. If
    not provided, {!val:env_empty}[ ()] is used.
*)
val env_of_list : ?env: 'a env -> (name * 'a callback) list -> 'a env

(** {2 Errors} *)

(** To catch eventual infinite loops in rewriting, we keep a
  stack of the rules called. *)
type rewrite_stack = (name * attributes * tree list) list

type error =
  Loop of rewrite_stack
    (** The [Loop] error is raised when the rewrite stack
       is higher than a default value of [100]. This value can be changed
       by setting the [XTMPL_REWRITE_DEPTH_LIMIT] environment variable.
       *)

| Parse_error of Xml.loc * string
| Invalid_attribute_value of string * tree list
| Fixpoint_limit of int

exception Error of error
val loop_error : rewrite_stack -> 'a
val parse_error : Xml.loc -> string -> 'a
val invalid_attribute_value : string -> tree list -> 'a
val fixpoint_limit : int -> 'a

val string_of_error : error -> string

(** String representation of the given rewrite stack. *)
val string_of_rewrite_stack : rewrite_stack -> string

(** {2 Special tags and attributes} *)

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

(** {2 Input/output} *)

val from_xml : Xml.tree list -> tree list

(** Output an XML string.

    @param xml_atts indicates whether the code in attributes remains valid XML
    of not. Default is [true] but it should be set to [false] when outputting
    final documents like XHTML pages.
*)
val to_string : ?xml_atts: bool -> tree list -> string

(** Parses a string as {!tree} list. *)
val from_string : string -> tree list

(** Same as {!from_string} but read from a file. *)
val from_file : string -> tree list

(** {2 XML Manipulation} *)

(** Recursively merge sibling [D] nodes into one [D] node. *)
val merge_cdata : tree -> tree

(** Same as {!merge_cdata} but taking a {!tree} list. *)
val merge_cdata_list : tree list -> tree list

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
    [env_add_cb "x2" (fun data _ _ xml -> (data, xml @ xml)) env],
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
    its attributes (using {!val:env_add_xml}).

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
val apply_into_file :
  'a -> ?head:string -> 'a env ->
    infile: string -> outfile: string -> 'a

(** As {!val:apply_into_file}, but read the XML from a string instead of a file.
*)
val apply_string_into_file :
  'a -> ?head:string -> 'a env -> outfile: string -> string -> 'a

