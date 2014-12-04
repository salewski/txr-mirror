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
OBJS-y := # make sure OBJ-y is a value variable, not a macro variable
OBJS := txr.o lex.yy.o y.tab.o match.o lib.o regex.o gc.o unwind.o stream.o
OBJS += arith.o hash.o utf8.o filter.o eval.o rand.o combi.o sysif.o
OBJS-$(debug_support) += debug.o
OBJS-$(have_syslog) += syslog.o
OBJS-$(have_posix_sigs) += signal.o
SRCS := $(filter-out lex.yy.c y.tab.c y.tab.h,\
           $(shell git ls-files "*.c" "*.h" "*.l" "*.y"))

# MPI objects
MPI_OBJ_BASE=mpi.o mplogic.o

MPI_OBJS := $(addprefix mpi-$(mpi_version)/,$(MPI_OBJ_BASE))

OBJS += $(MPI_OBJS)

PROG := txr
TXR := ./$(PROG)

.SUFFIXES:
MAKEFLAGS += --no-builtin-rules

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

$(PROG): $(OBJS) $(OBJS-y)
	$(CC) $(CFLAGS) -o $@ $^ -lm

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
	  y.tab.c lex.yy.c y.tab.h y.output $(TESTS_OUT) $(TESTS_OK)

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

TESTS_OUT := $(patsubst $(top_srcdir)/%.txr,./%.out,\
		          $(shell find $(top_srcdir)/tests -name '*.txr' | sort))
TESTS_OK := $(TESTS_OUT:.out=.ok)

$(TESTS_OK): $(PROG)

.PHONY: tests
tests: $(TESTS_OK)
	@rm -f $(TESTS_OUT) $(TESTS_OK)
	@echo "** tests passed!"

tests/001/%: TXR_ARGS := $(top_srcdir)/tests/001/data
tests/001/query-1.out: TXR_OPTS := -B
tests/001/query-2.out: TXR_OPTS := -B
tests/001/query-4.out: TXR_OPTS := -B
tests/002/%: TXR_OPTS := -DTESTDIR=$(top_srcdir)/tests/002
tests/004/%: TXR_ARGS := -a 123 -b -c
tests/005/%: TXR_ARGS := $(top_srcdir)/tests/005/data
tests/005/%: TXR_OPTS := -B
tests/006/%: TXR_ARGS := $(top_srcdir)/tests/006/data
tests/006/%: TXR_OPTS := -B
tests/006/freeform-3.out: TXR_ARGS := $(top_srcdir)/tests/006/passwd
tests/008/tokenize.out: TXR_ARGS := $(top_srcdir)/tests/008/data
tests/008/configfile.out: TXR_ARGS := $(top_srcdir)/tests/008/configfile
tests/008/students.out: TXR_ARGS := $(top_srcdir)/tests/008/students.xml
tests/008/soundex.out: TXR_ARGS := soundex sowndex lloyd lee jackson robert
tests/008/filtenv.out: TXR_OPTS := -B
tests/009/json.out: TXR_ARGS := $(addprefix $(top_srcdir)/tests/009/,webapp.json pass1.json)
tests/010/align-columns.out: TXR_ARGS := $(top_srcdir)/tests/010/align-columns.dat
tests/010/block.out: TXR_OPTS := -B
tests/010/reghash.out: TXR_OPTS := -B

tests/002/%: TXR_SCRIPT_ON_CMDLINE := y

tests/011/%: TXR_DBG_OPTS :=

.PRECIOUS: %.out
%.out: %.txr
	mkdir -p $(dir $@)
	$(if $(TXR_SCRIPT_ON_CMDLINE),\
	  $(TXR) $(TXR_DBG_OPTS) $(TXR_OPTS) -c "$$(cat $<)" \
	    $(TXR_ARGS) > $@,\
	  $(TXR) $(TXR_DBG_OPTS) $(TXR_OPTS) $< $(TXR_ARGS) > $@)

%.ok: %.out
	diff -u $(top_srcdir)/$(<:.out=.expected) $<
	@touch $@

%.expected: %.out
	cp $< $@

define GREP_CHECK
	@if [ $$(grep -E $(1) $(SRCS) | wc -l) -ne $(3) ] ; then \
	  echo "New '$(2)' occurrences have been found:" ; \
	  grep -n -E $(1) $(SRCS) \
	    | sed -e 's/\(.*:.*:\).*/\1 $(2)/' \
	    | grep $(4) ; \
	  exit 1 ; \
	fi
endef

.PHONY: enforce
enforce:
	$(call GREP_CHECK,'\<void[	 ]*\*',void *,1,-v 'typedef void \*yyscan_t')
	$(call GREP_CHECK,'	',tabs,0,'.')
	$(call GREP_CHECK,' $$',trailing spaces,0,'.')

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
	 make -s -n tests top_srcdir=. TXR=$(bindir)/txr) \
	 > run.sh
	$(call INSTALL,0755,run.sh,$(DESTDIR)$(datadir)/tests)

#
# Generate web page from man page
#
txr-manpage.html: txr.1 genman.txr
	man2html $< | $(TXR) genman.txr - > $@

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
