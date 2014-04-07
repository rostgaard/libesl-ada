###############################################################################
#                                                                             #
#  This is free software;  you can redistribute it  and/or modify it          #
#  under terms of the  GNU General Public License as published  by the        #
#  Free Software  Foundation;  either version 3,  or (at your option) any     #
#  later version.  This software is distributed in the hope  that it will     #
#  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty    #
#  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        #
#  General Public License for  more details.                                  #
#  You should have  received  a copy of the GNU General  Public  License      #
#  distributed  with  this  software;   see  file COPYING3.  If not, go       #
#  to http://www.gnu.org/licenses for a complete copy of the license.         #
#                                                                             #
###############################################################################

LIBNAME=esl
GPR_TARGET=lib/gnat/ 
GNATMAKE=gnatmake
GNATMAKE_TEST=gnatmake -P common
AHVEN_XML_DIR=xml_results

include makefile.setup

all: ${LIBNAME}

esl: esl_build

${LIBNAME}_build: fix-whitespace
	-gnatmake -p -P esl_build && touch esl

clean: tests_clean
	gnatclean -P esl_build
	BUILDTYPE=Debug gnatclean -P esl_build

distclean: clean
	-rm -rf downloads
	-rm -rf external_libs
	-rm -rf xml_results

uninstall:
	rm -rf $(PREFIX)/esl
	rm -rf $(PREFIX)/include/esl
	rm -f $(PREFIX)/$(GPR_TARGET)/esl.gpr

install: all
	install --directory        $(DESTDIR)$(PREFIX)/esl
	install --target-directory=$(DESTDIR)$(PREFIX)/esl              lib/*
	install --directory        $(DESTDIR)$(PREFIX)/include/esl
	install --target-directory=$(DESTDIR)$(PREFIX)/include/esl      src/*.ad[sb]
	install --target-directory=$(DESTDIR)$(PREFIX)/include/esl      external/*.ad[sb]
	install --directory        $(DESTDIR)$(PREFIX)/lib/gnat
	install esl.gpr.dist       $(DESTDIR)$(PREFIX)/lib/gnat/esl.gpr

downloads/ahven-2.3:
	-mkdir downloads
	(cd downloads && wget "http://downloads.sourceforge.net/project/ahven/ahven/Ahven%202.3/ahven-2.3.tar.gz")
	(cd downloads && tar xzf ahven-2.3.tar.gz)

external_libs/lib/ahven: downloads/ahven-2.3
	PREFIX=$(PWD)/external_libs make -C downloads/ahven-2.3 install_lib

tester: external_libs/lib/ahven
	${GNATMAKE} -P test tester

xml_tests: tester
	@-mkdir ${AHVEN_XML_DIR}
	./tester -q -x -d ${AHVEN_XML_DIR} > /dev/null

tests: xml_tests

tests_clean:
	@-rm esl-client-tasking-test esl-packet_content_type-test esl-packet-test parser
	
esl-client-tasking-test:
	mkdir -p build debug
	${GNATMAKE_TEST} $@

esl-packet_content_type-test:
	mkdir -p build debug
	${GNATMAKE_TEST} $@

esl-packet-test:
	mkdir -p build debug
	${GNATMAKE_TEST} $@

parser:
	mkdir -p build debug
	${GNATMAKE_TEST} $@
examples:
	mkdir -p build debug
	${GNATMAKE} -P examples/examples.gpr

fix-whitespace:
	@find src tests -name '*.ad?' | xargs --no-run-if-empty egrep -l '	| $$' | grep -v '^b[~]' | xargs --no-run-if-empty perl -i -lpe 's|	|        |g; s| +$$||g'

.PHONY: debug tests esl-client-tasking-test examples tester
