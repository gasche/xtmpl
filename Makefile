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

# DO NOT FORGET TO UPDATE META FILE
VERSION=0.11

OCAMLFIND=ocamlfind
PACKAGES=xmlm
COMPFLAGS=-annot -rectypes -safe-string -g
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

LIB_CMXFILES=xtmpl.cmx xtmpl_xhtml.cmx
LIB_CMOFILES=$(LIB_CMXFILES:.cmx=.cmo)
LIB_CMIFILES=$(LIB_CMXFILES:.cmx=.cmi)
LIB_OFILES=$(LIB_CMXFILES:.cmx=.o)

all: byte opt
byte: $(LIB_BYTE) ppx_xtmpl.byte
opt: $(LIB) $(LIB_CMXS) ppx_xtmpl

$(LIB): $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLFIND) ocamlopt -o $@ -a -package $(PACKAGES) $(LIB_CMXFILES)

$(LIB_CMXS): $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLFIND) ocamlopt -shared -o $@ -package $(PACKAGES) $(LIB_CMXFILES)

$(LIB_BYTE): $(LIB_CMIFILES) $(LIB_CMOFILES)
	$(OCAMLFIND) ocamlc -o $@ -a -package $(PACKAGES) $(LIB_CMOFILES)

%.cmx: %.ml %.cmi
	$(OCAMLFIND) ocamlopt -c -package $(PACKAGES) $(COMPFLAGS) $<

%.cmo: %.ml %.cmi
	$(OCAMLFIND) ocamlc -c -package $(PACKAGES) $(COMPFLAGS) $<

%.cmi: %.mli
	$(OCAMLFIND) ocamlc -c -package $(PACKAGES) $(COMPFLAGS) $<

ppx_xtmpl: $(LIB) ppx_xtmpl.ml
	$(OCAMLFIND) ocamlopt -o $@ -package ppx_tools.metaquot,str,$(PACKAGES) \
	$(COMPFLAGS) -linkpkg $(LIB) ppx_xtmpl.ml

ppx_xtmpl.byte: $(LIB_BYTE) ppx_xtmpl.ml
	$(OCAMLFIND) ocamlc -o $@ -package ppx_tools.metaquot,str,$(PACKAGES) \
	$(COMPFLAGS) -linkpkg $(LIB_BYTE) ppx_xtmpl.ml

.PHONY: test_ppx_xtmpl

test: test_ppx_xtmpl

test_ppx_xtmpl: ppx_xtmpl test_ppx_xtmpl.ml
	$(OCAMLFIND) ocamlopt -o $@ -dsource -rectypes -package xmlm,str -linkpkg \
	-ppx ./ppx_xtmpl xtmpl.cmx test_ppx_xtmpl.ml
	@echo "======"
	./$@

##########
.PHONY: doc
doc:
	$(MKDIR) doc
	$(OCAMLFIND) ocamldoc -package $(PACKAGES) xtmpl.mli xtmpl_xhtml.mli \
	-rectypes -t Xtmpl -d doc -html

webdoc: doc
	$(MKDIR) ../xtmpl-gh-pages/refdoc
	$(CP) doc/* ../xtmpl-gh-pages/refdoc/
	$(CP) web/index.html web/style.css ../xtmpl-gh-pages/


##########
install: xtmpl.cmo xtmpl.cmx
	$(OCAMLFIND) install xtmpl META LICENSE \
		$(LIB) $(LIB_CMXS) $(LIB_OFILES) $(LIB_CMXFILES) $(LIB_A) \
		$(LIB_BYTE) $(LIB_CMIFILES) \
		ppx_xtmpl ppx_xtmpl.byte

uninstall:
	ocamlfind remove xtmpl

# archive :
###########
archive:
	git archive --prefix=xtmpl-$(VERSION)/ HEAD | gzip > ../xtmpl-gh-pages/xtmpl-$(VERSION).tar.gz

#####
clean:
	$(RM) *.cm* *.o *.annot *.a ppx_xtmpl ppx_xtmpl.byte

# headers :
###########
HEADFILES=Makefile *.ml *.mli
.PHONY: headers noheaders
headers:
	headache -h header -c .headache_config $(HEADFILES)

noheaders:
	headache -r -c .headache_config $(HEADFILES)


