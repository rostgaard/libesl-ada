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
include makefile.setup

all: ${LIBNAME}

esl: esl_build

${LIBNAME}_build:
	-mkdir -p lib
	-mkdir -p build
	-gnatmake -P esl_build && touch esl

debug:
	mkdir -p debug
	BUILDTYPE=Debug gnatmake -P esl_build

clean: tests_clean
	gnatclean -P esl_build
	BUILDTYPE=Debug gnatclean -P esl_build

distclean: clean
	rm esl

uninstall:
	rm -rf $(PREFIX)/esl
	rm -rf $(PREFIX)/include/esl
	rm -f $(PREFIX)/$(GPR_TARGET)/esl.gpr

install: all
	install --directory        $(DESTDIR)$(PREFIX)/esl
	install --target-directory=$(DESTDIR)$(PREFIX)/esl              lib/*
	install --directory        $(DESTDIR)$(PREFIX)/include/esl
	install --target-directory=$(DESTDIR)$(PREFIX)/include/esl      src/*.ad[sb]
	install --target-directory=$(DESTDIR)$(PREFIX)/include/esl      src/*/*.ad[sb]
	install --directory        $(DESTDIR)$(PREFIX)/lib/gnat
	install esl.gpr.dist       $(DESTDIR)$(PREFIX)/lib/gnat/esl.gpr

tests: all
	@./tests/build
	@./tests/run

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

.PHONY: debug tests esl-client-tasking-test examples
