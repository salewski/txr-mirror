# Copyright 2009
# Kaz Kylheku <kkylheku@gmail.com>
# Vancouver, Canada
# All rights reserved.
#
# BSD License:
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
#   1. Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#   2. Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in
#      the documentation and/or other materials provided with the
#      distribution.
#   3. The name of the author may not be used to endorse or promote
#      products derived from this software without specific prior
#      written permission.
#
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.

# Test data in the tests/ directory is in the public domain,
# unless it contains notices to the contrary.

OPT_FLAGS := -O2
LANG_FLAGS := -ansi -D_GNU_SOURCE
DIAG_FLAGS := -Wall
DBG_FLAGS := -g
LEX_DBG_FLAGS :=
TXR_DBG_OPTS := --gc-debug
LEXLIB := fl

CFLAGS := $(LANG_FLAGS) $(DIAG_FLAGS) $(OPT_FLAGS) $(DBG_FLAGS)

OBJS := txr.o lex.yy.o y.tab.o match.o lib.o regex.o gc.o unwind.o stream.o
txr: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ -l$(LEXLIB)

-include dep.mk

lex.yy.c: parser.l
	$(LEX) $(LEX_DBG_FLAGS) $<

y.tab.c y.tab.h: parser.y
	if $(YACC) -v -d $< ; then true ; else rm $@ ; false ; fi

clean:
	rm -f txr $(OBJS) \
	  y.tab.c lex.yy.c y.tab.h y.output $(TESTS:.ok=.out)

depend: txr
	./txr depend.txr > dep.mk

TESTS := $(patsubst %.txr,%.ok,$(shell find tests -name '*.txr' | sort))

tests: txr $(TESTS)
	@echo "** tests passed!"

tests/001/%: TXR_ARGS := tests/001/data
tests/002/%: TXR_OPTS := -DTESTDIR=tests/002
tests/004/%: TXR_ARGS := -a 123 -b -c

%.ok: %.txr
	./txr $(TXR_DBG_OPTS) $(TXR_OPTS) $^ $(TXR_ARGS) > $(@:.ok=.out)
	diff $(@:.ok=.expected) $(@:.ok=.out)

%.expected: %.txr
	./txr $(TXR_OPTS) $^ $(TXR_ARGS) > $@

