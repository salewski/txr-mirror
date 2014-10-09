# Copyright 2009-2014
# Kaz Kylheku <kaz@kylheku.com>
# Vancouver, Canada
# All rights reserved.
#
# Redistribution of this software in source and binary forms, with or without
# modification, is permitted provided that the following two conditions are met.
#
# Use of this software in any manner constitutes agreement with the disclaimer
# which follows the two conditions.
#
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in
#    the documentation and/or other materials provided with the
#    distribution.
#
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DAMAGES, HOWEVER CAUSED,
# AND UNDER ANY THEORY OF LIABILITY, ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

include config.make

CFLAGS := -iquote $(top_srcdir) $(LANG_FLAGS) $(DIAG_FLAGS) \
          $(OPT_FLAGS) $(DBG_FLAGS) $(PLATFORM_FLAGS) $(EXTRA_FLAGS)
CFLAGS += -iquote mpi-$(mpi_version)
CFLAGS := $(filter-out $(REMOVE_FLAGS),$(CFLAGS))

ifneq ($(subst g++,@,$(notdir $(CC))),$(notdir $(CC)))
CFLAGS := $(filter-out -Wmissing-prototypes -Wstrict-prototypes,$(CFLAGS))
endif

# TXR objects
OBJS := txr.o lex.yy.o y.tab.o match.o lib.o regex.o gc.o unwind.o stream.o
OBJS += arith.o hash.o utf8.o filter.o eval.o rand.o combi.o
OBJS-$(debug_support) += debug.o
OBJS-$(have_syslog) += syslog.o
OBJS-$(have_posix_sigs) += signal.o

# MPI objects
MPI_OBJ_BASE=mpi.o mplogic.o

MPI_OBJS := $(addprefix mpi-$(mpi_version)/,$(MPI_OBJ_BASE))

OBJS += $(MPI_OBJS)

PROG := txr

$(PROG): $(OBJS) $(OBJS-y)
	$(CC) $(CFLAGS) -o $@ $^ -lm $(LEXLIB)

VPATH := $(top_srcdir)

-include $(top_srcdir)/dep.mk

$(OBJS) $(OBJS-y): config.make

lex.yy.c: parser.l config.make
	rm -f $@
	$(LEX) $(LEX_DBG_FLAGS) $<
	chmod a-w $@

y.tab.c y.tab.h: parser.y config.make
	rm -f y.tab.c
	if $(YACC) -v -d $< ; then chmod a-w y.tab.c ; true ; else rm y.tab.c ; false ; fi

# Suppress useless sccs id array and unused label warning in byacc otuput.
# Bison-generated parser also tests for this lint define.
y.tab.o: CFLAGS += -Dlint

# txr.c needs to know the relative datadir path to do some sysroot
# calculations.

txr.o: CFLAGS += -DTXR_REL_PATH=\"$(bindir_rel)/$(PROG)$(EXE)\"
txr.o: CFLAGS += -DEXE_SUFF=\"$(EXE)\" -DPROG_NAME=\"$(PROG)\"
txr.o: CFLAGS += -DTXR_VER=\"$(txr_ver)\"

$(MPI_OBJS): CFLAGS += -DXMALLOC=chk_malloc -DXREALLOC=chk_realloc
$(MPI_OBJS): CFLAGS += -DXCALLOC=chk_calloc -DXFREE=free

.PHONY: rebuild
rebuild: clean repatch $(PROG)

.PHONY: clean
clean: conftest.clean
	rm -f $(PROG)$(EXE) $(OBJS) $(OBJS-y) \
	  y.tab.c lex.yy.c y.tab.h y.output $(TESTS:.ok=.out)

.PHONY: repatch
repatch:
	cd mpi-$(mpi_version); quilt pop -af
	cd mpi-$(mpi_version); quilt push -a

.PHONY: distclean
distclean: clean
	rm -f config.h config.make config.log
	rm -rf mpi-$(mpi_version)

.PHONY: depend
depend:
	txr $(top_srcdir)/depend.txr $(OBJS) $(OBJS-y) > $(top_srcdir)/dep.mk

TESTS := $(patsubst $(top_srcdir)/%.txr,./%.ok,\
                    $(shell find $(top_srcdir)/tests -name '*.txr' | sort))

.PHONY: tests $(TEST)
tests: $(TESTS)
	@echo "** tests passed!"

tests/001/%: TXR_ARGS := $(top_srcdir)/tests/001/data
tests/001/query-1.ok: TXR_OPTS := -B
tests/001/query-2.ok: TXR_OPTS := -B
tests/001/query-4.ok: TXR_OPTS := -B
tests/002/%: TXR_OPTS := -DTESTDIR=$(top_srcdir)/tests/002
tests/004/%: TXR_ARGS := -a 123 -b -c
tests/005/%: TXR_ARGS := $(top_srcdir)/tests/005/data
tests/005/%: TXR_OPTS := -B
tests/006/%: TXR_ARGS := $(top_srcdir)/tests/006/data
tests/006/%: TXR_OPTS := -B
tests/006/freeform-3.ok: TXR_ARGS := $(top_srcdir)/tests/006/passwd
tests/008/tokenize.ok: TXR_ARGS := $(top_srcdir)/tests/008/data
tests/008/configfile.ok: TXR_ARGS := $(top_srcdir)/tests/008/configfile
tests/008/students.ok: TXR_ARGS := $(top_srcdir)/tests/008/students.xml
tests/008/soundex.ok: TXR_ARGS := soundex sowndex lloyd lee jackson robert
tests/008/filtenv.ok: TXR_OPTS := -B
tests/009/json.ok: TXR_ARGS = $(addprefix $(top_srcdir)/tests/009/,webapp.json pass1.json)
tests/010/align-columns.ok: TXR_ARGS := $(top_srcdir)/tests/010/align-columns.dat
tests/010/block.ok: TXR_OPTS := -B
tests/010/reghash.ok: TXR_OPTS := -B

tests/002/%: TXR_SCRIPT_ON_CMDLINE := y

tests/011/%: TXR_DBG_OPTS := 

%.ok: %.txr
	mkdir -p $(dir $@)
	$(if $(TXR_SCRIPT_ON_CMDLINE),\
	  ./$(PROG) $(TXR_DBG_OPTS) $(TXR_OPTS) -c "$$(cat $^)" \
	    $(TXR_ARGS) > $(@:.ok=.out),\
	  ./$(PROG) $(TXR_DBG_OPTS) $(TXR_OPTS) $^ $(TXR_ARGS) > $(@:.ok=.out))
	diff -u $(^:.txr=.expected) $(@:.ok=.out)

%.expected: %.txr
	./$(PROG) $(TXR_OPTS) $^ $(TXR_ARGS) > $@

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

PREINSTALL := :

.PHONY: install
install: $(PROG)
	$(PREINSTALL)
	$(call INSTALL,0755,txr$(EXE),$(DESTDIR)$(bindir))
	$(call INSTALL,0444,$(top_srcdir)/LICENSE,$(DESTDIR)$(datadir))
	$(call INSTALL,0444,$(top_srcdir)/METALICENSE,$(DESTDIR)$(datadir))
	$(call INSTALL,0444,$(top_srcdir)/txr.1,$(DESTDIR)$(mandir)/man1)
	$(call INSTALL,0444,$(top_srcdir)/share/txr/stdlib/*.txr,$(DESTDIR)$(datadir)/stdlib)

.PHONY: unixtar gnutar zip

unixtar gnutar zip: DESTDIR=pkg
zip: prefix=/txr
unixtar gnutar zip: PREINSTALL=rm -rf pkg

unixtar: install
	cd pkg; pwd; find . | pax -M uidgid -w -f ../txr-$(txr_ver)-bin.tar -x ustar ; ls ../*.tar
	pwd
	compress txr-$(txr_ver)-bin.tar

gnutar: install
	tar --owner=0 --group=0 -C pkg -czf txr-$(txr_ver)-bin.tar.gz .

zip: install
	cd pkg; zip -r ../txr-$(txr_ver)-bin.zip .

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
	man2html $< | ./$(PROG) genman.txr - > $@

txr-manpage.pdf: txr.1
	tbl $< | pdfroff -man --no-toc - > $@

config.make config.h:
	@echo "$@ missing: you didn't run ./configure"
	@exit 1

#
# Special targets used by ./configure
#

conftest: conftest.c
	$(CC) $(CFLAGS) -o $@ $^ -lm

conftest2: conftest1.c conftest2.c
	$(CC) $(CFLAGS) -o $@ $^ -lm

conftest.syms: conftest.o
	$(NM) -n -t o -P $^ > $@

.PHONY: conftest.yacc
conftest.yacc:
	@echo $(YACC)

.PHONY: conftest.ccver
conftest.ccver:
	@$(CC) --version

.PHONY: conftest.clean
conftest.clean:
	@rm -f conftest$(EXE) conftest.[co] \
	conftest2$(EXE) conftest[12].[oc] \
	conftest.err conftest.syms
