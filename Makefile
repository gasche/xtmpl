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
VERSION=0.10

INCLUDES=`ocamlfind query -i-format xmlm`
COMPFLAGS=$(INCLUDES) -annot -rectypes -safe-string
OCAMLPP=

OCAMLC=ocamlc.opt -g #ocamlcp -p a #
OCAMLOPT=ocamlopt.opt -g #ocamloptp -p #
OCAMLLIB:=`$(OCAMLC) -where`
OCAMLDOC=ocamldoc.opt
OCAMLFIND=ocamlfind

INSTALLDIR=$(OCAMLLIB)

RM=rm -f
CP=cp -f
MKDIR=mkdir -p

all: byte opt
byte: xtmpl.cmo pp_xml.byte
opt: xtmpl.cmx xtmpl.cmxs pp_xml

xtmpl.cmx: xtmpl.cmi xtmpl.ml
	$(OCAMLOPT) -c $(COMPFLAGS) xtmpl.ml

xtmpl.cmxs: xtmpl.cmx
	$(OCAMLOPT) -shared -o $@ $(COMPFLAGS) xtmpl.cmx

xtmpl.cmo: xtmpl.cmi xtmpl.ml
	$(OCAMLC) -c $(COMPFLAGS) xtmpl.ml

xtmpl.cmi: xtmpl.mli
	$(OCAMLC) -c $(COMPFLAGS) $<

pp_xml: pp_xml.ml
	$(OCAMLFIND) ocamlopt -o $@ -package sedlex -linkpkg $^

pp_xml.byte: pp_xml.ml
	$(OCAMLFIND) ocamlc -o $@ -package sedlex -linkpkg $^

##########
.PHONY: doc
doc:
	$(MKDIR) doc
	$(OCAMLDOC) $(INCLUDES) xtmpl.mli -t Xtmpl -d doc -html

webdoc: doc
	$(MKDIR) ../xtmpl-gh-pages/refdoc
	$(CP) doc/* ../xtmpl-gh-pages/refdoc/
	$(CP) web/index.html web/style.css ../xtmpl-gh-pages/


##########
install: xtmpl.cmo xtmpl.cmx
	ocamlfind install xtmpl META LICENSE \
		xtmpl.cmi xtmpl.mli xtmpl.cmo xtmpl.cmx xtmpl.cmxs xtmpl.o

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


