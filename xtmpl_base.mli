type name = string * string

val re_escape : Str.regexp
val escape_ampersand : string -> string
val re_amp : Str.regexp
val unescape_ampersand : string -> string
module Name_ord : Map.OrderedType with type t = name
module Name_map : Map.S with type key = name
module Name_set : Set.S with type elt = name
type 'a attributes = 'a Name_map.t
type 'a tree = E of name * 'a attributes * 'a tree list | D of string

type rewrite_tree = (('a tree list) as 'a) tree

type xml_attributes = (rewrite_tree list) attributes
type str_attributes = string attributes

val atts_empty : 'a Name_map.t
val string_of_name : string * string -> string
val node : name -> ?atts:'a Name_map.t -> 'a tree list -> 'a tree
val cdata : string -> 'a tree
val att_escamp : string
val gen_atts_to_escape : ('a -> string) -> 'a Name_map.t -> Name_set.t
val atts_to_escape : str_attributes -> Name_set.t
val xml_atts_to_escape : xml_attributes -> Name_set.t
