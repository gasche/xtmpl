####################################################################################
#                Xtmpl                                                             #
#                                                                                  #
#    Copyright (C) 2012-2015 Institut National de Recherche en Informatique        #
#    et en Automatique. All rights reserved.                                       #
#                                                                                  #
#    This program is free software; you can redistribute it and/or modify          #
#    it under the terms of the GNU Lesser General Public License version           #
#    3 as published by the Free Software Foundation.                               #
#                                                                                  #
#    This program is distributed in the hope that it will be useful,               #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of                #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                 #
#    GNU Library General Public License for more details.                          #
#                                                                                  #
#    You should have received a copy of the GNU Lesser General Public              #
#    License along with this program; if not, write to the Free Software           #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                      #
#    02111-1307  USA                                                               #
#                                                                                  #
#    Contact: Maxence.Guesdon@inria.fr                                             #
#                                                                                  #
#                                                                                  #
####################################################################################

# DO NOT FORGET TO UPDATE META AND opam FILES
VERSION=0.16.0

OCAMLFIND=ocamlfind
OCAMLC=ocamlc -g
OCAMLOPT=ocamlopt -g
PACKAGES=sedlex,uutf,re.str,iri
JS_PACKAGES=$(PACKAGES),js_of_ocaml
COMPFLAGS=-annot -rectypes -safe-string
OCAMLPP=
OCAMLLIB:=`$(OCAMLC) -where`

INSTALLDIR=$(OCAMLLIB)

RM=rm -f
CP=cp -f
MKDIR=mkdir -p

LIB=xtmpl.cmxa
LIB_CMXS=$(LIB:.cmxa=.cmxs)
LIB_A=$(LIB:.cmxa=.a)
LIB_BYTE=$(LIB:.cmxa=.cma)
LIB_CMI=$(LIB:.cmxa=.cmi)

LIB_CMXFILES= \
	xtmpl_misc.cmx \
	xtmpl_xml.cmx \
	xtmpl_rewrite.cmx \
	xtmpl_xhtml.cmx
LIB_CMOFILES=$(LIB_CMXFILES:.cmx=.cmo)
LIB_CMIFILES=$(LIB_CMXFILES:.cmx=.cmi)
LIB_OFILES=$(LIB_CMXFILES:.cmx=.o)

LIB_JS=xtmpl_js.cma
LIB_JS_CMI=$(LIB_JS:.cma=.cmi)
LIB_JS_CMOFILES=xtmpl_js.cmo
LIB_JS_CMIFILES=$(LIB_JS_CMOFILES:.cmo=.cmi)

all: byte opt
byte: $(LIB_BYTE) $(LIB_JS) ppx_xtmpl.byte
opt: $(LIB) $(LIB_CMXS) ppx_xtmpl

$(LIB): $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLFIND) $(OCAMLOPT) -o $@ -a -package $(PACKAGES) $(LIB_CMXFILES)

$(LIB_CMXS): $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLFIND) $(OCAMLOPT) -shared -o $@ -package $(PACKAGES) $(LIB_CMXFILES)

$(LIB_BYTE): $(LIB_CMIFILES) $(LIB_CMOFILES)
	$(OCAMLFIND) $(OCAMLC) -o $@ -a -package $(PACKAGES) $(LIB_CMOFILES)

$(LIB_JS): $(LIB_BYTE) $(LIB_JS_CMIFILES) $(LIB_JS_CMOFILES)
	$(OCAMLFIND) $(OCAMLC) -o $@ -a -package $(JS_PACKAGES) $(LIB_JS_CMOFILES)

%.cmx: %.ml %.cmi
	$(OCAMLFIND) $(OCAMLOPT) -c -package $(PACKAGES) $(COMPFLAGS) $<

%.cmo: %.ml %.cmi
	$(OCAMLFIND) $(OCAMLC) -c -package $(PACKAGES) $(COMPFLAGS) $<

%.cmi: %.mli
	$(OCAMLFIND) $(OCAMLC) -c -package $(PACKAGES) $(COMPFLAGS) $<

ppx_xtmpl: $(LIB) ppx_xtmpl.ml
	$(OCAMLFIND) $(OCAMLOPT) -o $@ -package ppx_tools.metaquot,$(PACKAGES) \
	$(COMPFLAGS) -linkpkg $(LIB) ppx_xtmpl.ml

ppx_xtmpl.byte: $(LIB_BYTE) ppx_xtmpl.ml
	$(OCAMLFIND) $(OCAMLC) -o $@ -package ppx_tools.metaquot,$(PACKAGES) \
	$(COMPFLAGS) -linkpkg $(LIB_BYTE) ppx_xtmpl.ml

xtmpl_js.cmo: xtmpl_js.ml
	$(OCAMLFIND) $(OCAMLC) -c -package $(JS_PACKAGES) \
	-package js_of_ocaml.ppx \
	$(COMPFLAGS) $<

xtmpl_js.cmi: xtmpl_js.mli
	$(OCAMLFIND) $(OCAMLC) -c -package $(JS_PACKAGES) $(COMPFLAGS) $<

.PHONY: test_ppx_xtmpl

test: test_ppx_xtmpl test_cat

test_ppx_xtmpl: ppx_xtmpl test_ppx_xtmpl.ml
	$(OCAMLFIND) $(OCAMLOPT) -o $@ -dsource -rectypes -package uutf,sedlex,re.str -linkpkg \
	-ppx ./ppx_xtmpl xtmpl.cmxa test_ppx_xtmpl.ml
	@echo "======"
	./$@

test_cat: xtmpl.cmxa test_cat.ml
	$(OCAMLFIND) $(OCAMLOPT) -o $@ -rectypes -package uutf,sedlex,re.str -linkpkg \
	xtmpl.cmxa test_cat.ml

test_js.js: xtmpl.cma xtmpl_js.cmo test_js.ml
	$(OCAMLFIND) $(OCAMLC) -o $@.byte -rectypes \
	-package js_of_ocaml,js_of_ocaml.ppx,uutf,sedlex,re.str -linkpkg \
	$^
	js_of_ocaml --pretty +js_of_ocaml/nat.js $@.byte -o $@
	#--disable staticeval --disable share --noinline
	rm -f $@.byte

##########
.PHONY: doc
dump.odoc:
	$(OCAMLFIND) ocamldoc -package $(JS_PACKAGES) -dump $@ -rectypes \
	xtmpl_*.mli

doc: dump.odoc
	$(MKDIR) doc
	$(OCAMLFIND) ocamldoc -load $^ -rectypes -t Xtmpl -d doc -html

docstog: dump.odoc
	$(MKDIR) web/refdoc
	ocamldoc.opt \
	-t "Xtmpl reference documentation" \
	-load $^ -d web/refdoc -i `ocamlfind query stog` -g odoc_stog.cmxs

##########
install: xtmpl.cmo xtmpl.cmx
	$(OCAMLFIND) install xtmpl META LICENSE \
		$(LIB) $(LIB_CMXS) $(LIB_OFILES) $(LIB_CMXFILES) $(LIB_A) \
		$(LIB_BYTE) $(LIB_CMIFILES) \
		$(LIB_JS) $(LIB_JS_CMIFILES) \
		ppx_xtmpl ppx_xtmpl.byte

uninstall:
	ocamlfind remove xtmpl

# archive :
###########
archive:
	git archive --prefix=xtmpl-$(VERSION)/ HEAD | gzip > ../xtmpl-gh-pages/xtmpl-$(VERSION).tar.gz

#####
clean:
	$(RM) *.cm* *.o *.annot *.a ppx_xtmpl ppx_xtmpl.byte dump.odoc

# headers :
###########
HEADFILES=Makefile *.ml *.mli
.PHONY: headers noheaders
headers:
	headache -h header -c .headache_config $(HEADFILES)

noheaders:
	headache -r -c .headache_config $(HEADFILES)

# depend :
##########

.PHONY: depend

.depend depend:
	$(OCAMLFIND) ocamldep `ls xtmpl*.ml xtmpl*.mli | grep -v _js.ml` > .depend

xtmpl_js.cmi xtmpl_js.cmo: xtmpl.cmi

include .depend
