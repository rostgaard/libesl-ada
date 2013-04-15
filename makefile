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

GPR_TARGET=lib/gnat/ 

include makefile.setup

all: esl

esl:
	-mkdir lib
	-mkdir build
	gnatmake -P esl_build && touch esl

debug:
	BUILDTYPE=Debug gnatmake -P esl_build

clean:
	gnatclean -P esl_build
	BUILDTYPE=Debug gnatclean -P esl_build

uninstall:
	rm -rf $(PREFIX)/esl
	rm -rf $(PREFIX)/include/esl
	rm -f $(PREFIX)/$(GPR_TARGET)/esl.gpr

install: all
	mkdir -p $(PREFIX)/lib/gnat
	mkdir -p $(PREFIX)/esl
	mkdir -p $(PREFIX)/include/esl
	cp -pr lib/* $(PREFIX)/esl
	cp -pr src/*.ad[sb] $(PREFIX)/include/esl
	cp -pr esl.gpr.dist $(PREFIX)/lib/gnat/esl.gpr
