#################################################################################
#                Xtmpl                                                          #
#                                                                               #
#    Copyright (C) 2012 Institut National de Recherche en Informatique          #
#    et en Automatique. All rights reserved.                                    #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU Lesser General Public License version        #
#    3 as published by the Free Software Foundation.                            #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
#    GNU Library General Public License for more details.                       #
#                                                                               #
#    You should have received a copy of the GNU Lesser General Public           #
#    License along with this program; if not, write to the Free Software        #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   #
#    02111-1307  USA                                                            #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#                                                                               #
#                                                                               #
#################################################################################

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

all: byte opt
byte: xtmpl.cmo ppx_xtmpl.byte
opt: xtmpl.cmx xtmpl.cmxs ppx_xtmpl

xtmpl.cmx: xtmpl.cmi xtmpl.ml
	$(OCAMLFIND) ocamlopt -c -package $(PACKAGES) $(COMPFLAGS) xtmpl.ml

xtmpl.cmxs: xtmpl.cmx
	$(OCAMLFIND) ocamlopt -shared -o $@ -package $(PACKAGES) $(COMPFLAGS) xtmpl.cmx

xtmpl.cmo: xtmpl.cmi xtmpl.ml
	$(OCAMLFIND) ocamlc -c -package $(PACKAGES) $(COMPFLAGS) xtmpl.ml

xtmpl.cmi: xtmpl.mli
	$(OCAMLFIND) ocamlc -c -package $(PACKAGES) $(COMPFLAGS) $<

ppx_xtmpl: ppx_xtmpl.ml
	$(OCAMLFIND) ocamlopt -o $@ -package ppx_tools.metaquot,str,$(PACKAGES) \
	$(COMPFLAGS) -linkpkg xtmpl.cmx $<

ppx_xtmpl.byte: ppx_xtmpl.ml
	$(OCAMLFIND) ocamlc -o $@ -package ppx_tools.metaquot,str,$(PACKAGES) \
	$(COMPFLAGS) -linkpkg xtmpl.cmo $<

##########
.PHONY: doc
doc:
	$(MKDIR) doc
	$(OCAMLFIND) ocamldoc -package $(PACKAGES) xtmpl.mli -t Xtmpl -d doc -html

webdoc: doc
	$(MKDIR) ../xtmpl-gh-pages/refdoc
	$(CP) doc/* ../xtmpl-gh-pages/refdoc/
	$(CP) web/index.html web/style.css ../xtmpl-gh-pages/


##########
install: xtmpl.cmo xtmpl.cmx
	$(OCAMLFIND) install xtmpl META LICENSE \
		xtmpl.cmi xtmpl.mli xtmpl.cmo xtmpl.cmx xtmpl.cmxs xtmpl.o \
		ppx_xtmpl ppx_xtmpl.byte

uninstall:
	ocamlfind remove xtmpl

# archive :
###########
archive:
	git archive --prefix=xtmpl-$(VERSION)/ HEAD | gzip > ../xtmpl-gh-pages/xtmpl-$(VERSION).tar.gz

#####
clean:
	$(RM) *.cm* *.o *.annot *.a

# headers :
###########
HEADFILES=Makefile *.ml *.mli
.PHONY: headers noheaders
headers:
	headache -h header -c .headache_config $(HEADFILES)

noheaders:
	headache -r -c .headache_config $(HEADFILES)


