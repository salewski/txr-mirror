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

-include config.make

CFLAGS := -I$(top_srcdir) $(LANG_FLAGS) $(DIAG_FLAGS) $(OPT_FLAGS) $(DBG_FLAGS)

OBJS := txr.o lex.yy.o y.tab.o match.o lib.o regex.o gc.o unwind.o stream.o
OBJS += hash.o utf8.o

PROG := ./txr

$(PROG): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LEXLIB)

VPATH := $(top_srcdir)

-include $(top_srcdir)/dep.mk

lex.yy.c: parser.l
	$(LEX) $(LEX_DBG_FLAGS) $<

y.tab.c y.tab.h: parser.y
	if $(YACC) -v -d $< ; then true ; else rm $@ ; false ; fi

clean:
	rm -f $(PROG) $(OBJS) \
	  y.tab.c lex.yy.c y.tab.h y.output $(TESTS:.ok=.out)

distclean: clean
	rm -f config.make config.log

depend: $(PROG)
	$(PROG) $(top_srcdir)/depend.txr > $(top_srcdir)/dep.mk

TESTS := $(patsubst $(top_srcdir)/%.txr,./%.ok,\
                    $(shell find $(top_srcdir)/tests -name '*.txr' | sort))

tests: $(PROG) $(TESTS)
	@echo "** tests passed!"

tests/001/%: TXR_ARGS := $(top_srcdir)/tests/001/data
tests/002/%: TXR_OPTS := -DTESTDIR=$(top_srcdir)/tests/002
tests/004/%: TXR_ARGS := -a 123 -b -c

%.ok: %.txr
	mkdir -p $(dir $@)
	$(PROG) $(TXR_DBG_OPTS) $(TXR_OPTS) $^ $(TXR_ARGS) > $(@:.ok=.out)
	diff $(^:.txr=.expected) $(@:.ok=.out)

%.expected: %.txr
	$(PROG) $(TXR_OPTS) $^ $(TXR_ARGS) > $@

install: $(PROG)
	mkdir -p $(install_prefix)$(bindir)
	mkdir -p $(install_prefix)$(datadir)
	mkdir -p $(install_prefix)$(mandir)/man1
	cp txr $(install_prefix)$(bindir)
	cp $(top_srcdir)/txr.1 $(install_prefix)$(mandir)/man1

config.make:
	@echo "config.make missing: you didn't run ./configure"
	@exit 1
