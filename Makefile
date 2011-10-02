# Copyright 2011
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

include config.make

CFLAGS := -I. -I$(top_srcdir) $(LANG_FLAGS) $(DIAG_FLAGS) \
          $(OPT_FLAGS) $(DBG_FLAGS) $(PLATFORM_FLAGS)
CFLAGS := $(filter-out $(REMOVE_FLAGS),$(CFLAGS))

ifneq ($(subst g++,@,$(notdir $(CC))),$(notdir $(CC)))
CFLAGS := $(filter-out -Wmissing-prototypes -Wstrict-prototypes,$(CFLAGS))
endif

OBJS := txr.o lex.yy.o y.tab.o match.o lib.o regex.o gc.o unwind.o stream.o
OBJS += hash.o utf8.o filter.o

PROG := ./txr

$(PROG): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LEXLIB)

VPATH := $(top_srcdir)

-include $(top_srcdir)/dep.mk

lex.yy.c: parser.l
	$(LEX) $(LEX_DBG_FLAGS) $<

y.tab.c y.tab.h: parser.y
	if $(YACC) -v -d $< ; then true ; else rm $@ ; false ; fi

# Suppress useless sccs id array and unused label warning in byacc otuput.
# Bison-generated parser also tests for this lint define.
y.tab.o: CFLAGS += -Dlint

.PHONY: rebuild
rebuild: clean $(PROG)

.PHONY: clean
clean:
	rm -f $(PROG) $(OBJS) \
	  y.tab.c lex.yy.c y.tab.h y.output $(TESTS:.ok=.out)

.PHONY: distclean
distclean: clean
	rm -f config.h config.make config.log

.PHONY: depend
depend:
	$(PROG) $(top_srcdir)/depend.txr $(OBJS) > $(top_srcdir)/dep.mk

TESTS := $(patsubst $(top_srcdir)/%.txr,./%.ok,\
                    $(shell find $(top_srcdir)/tests -name '*.txr' | sort))

.PHONY: tests $(TEST)
tests: $(TESTS)
	@echo "** tests passed!"

tests/001/%: TXR_ARGS := $(top_srcdir)/tests/001/data
tests/002/%: TXR_OPTS := -DTESTDIR=$(top_srcdir)/tests/002
tests/004/%: TXR_ARGS := -a 123 -b -c
tests/005/%: TXR_ARGS := $(top_srcdir)/tests/005/data
tests/006/%: TXR_ARGS := $(top_srcdir)/tests/006/data

tests/002/%: TXR_SCRIPT_ON_CMDLINE := y

%.ok: %.txr
	mkdir -p $(dir $@)
	$(if $(TXR_SCRIPT_ON_CMDLINE),\
	  $(PROG) $(TXR_DBG_OPTS) $(TXR_OPTS) -c "$$(cat $^)" \
	    $(TXR_ARGS) > $(@:.ok=.out),\
	  $(PROG) $(TXR_DBG_OPTS) $(TXR_OPTS) $^ $(TXR_ARGS) > $(@:.ok=.out))
	diff $(^:.txr=.expected) $(@:.ok=.out)

%.expected: %.txr
	$(PROG) $(TXR_OPTS) $^ $(TXR_ARGS) > $@

#
# Installation macro.
#
# $1 - chmod perms
# $2 - source file
# $3 - dest directory
#
define INSTALL
	mkdir -p $(3)
	cp -f $(2) $(3)
	chmod $(1) $(3)/$(notdir $(2))
	touch -r $(2) $(3)/$(notdir $(2))
endef

.PHONY: install
install: $(PROG)
	$(call INSTALL,0755,txr,$(DESTDIR)$(bindir))
	$(call INSTALL,0444,$(top_srcdir)/txr.1,$(DESTDIR)$(mandir)/man1)

#
# Install the tests as well as the script to run them
# 
install-tests:
	mkdir -p $(DESTDIR)$(datadir)
	(cd $(top_srcdir) ; \
	 find tests -name '*.out' -prune -o -print | cpio -co) \
	| (cd $(DESTDIR)$(datadir) ; cpio -idu)
	(echo "#!/bin/sh" ; \
	 echo "set -ex" ; \
	 echo "cd $(datadir)" ; \
	 make -s -n tests top_srcdir=. PROG=$(bindir)/txr) \
	 > run.sh
	$(call INSTALL,0755,run.sh,$(DESTDIR)$(datadir)/tests)

#
# Generate web page from man page
# 
txr-manpage.html: txr.1 genman.txr
	man2html $< | $(PROG) genman.txr - > $@

config.make config.h:
	@echo "$@ missing: you didn't run ./configure"
	@exit 1

#
# Special targets used by ./configure
#

conftest: conftest.c
	$(CC) $(CFLAGS) -o $@ $^

conftest2: conftest1.c conftest2.c
	$(CC) $(CFLAGS) -o $@ $^

conftest.syms: conftest.o
	$(NM) -t o -P $^ > $@

.PHONY: conftest.yacc
conftest.yacc:
	@echo $(YACC)
