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

VERSION=0.7

INCLUDES=`ocamlfind query -i-format xmlm`
COMPFLAGS=$(INCLUDES) -annot -rectypes
OCAMLPP=

OCAMLC=ocamlc.opt -g
OCAMLOPT=ocamlopt.opt -g
OCAMLLIB:=`$(OCAMLC) -where`
OCAMLDOC=ocamldoc.opt

INSTALLDIR=$(OCAMLLIB)

RM=rm -f
CP=cp -f
MKDIR=mkdir -p

all: byte opt
byte: xtmpl.cmo
opt: xtmpl.cmx xtmpl.cmxs

xtmpl.cmx: xtmpl.cmi xtmpl.ml
	$(OCAMLOPT) -c $(COMPFLAGS) xtmpl.ml

xtmpl.cmxs: xtmpl.cmx
	$(OCAMLOPT) -shared -o $@ $(COMPFLAGS) xtmpl.cmx

xtmpl.cmo: xtmpl.cmi xtmpl.ml
	$(OCAMLC) -c $(COMPFLAGS) xtmpl.ml

xtmpl.cmi: xtmpl.mli
	$(OCAMLC) -c $(COMPFLAGS) $<

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
	ocamlfind install xtmpl META LICENSE xtmpl.cmi xtmpl.mli xtmpl.cmo xtmpl.cmx xtmpl.cmxs xtmpl.o

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


