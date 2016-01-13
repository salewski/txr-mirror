# Copyright 2009-2016
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

-include config/config.make

VERBOSE :=
CFLAGS := -iquote $(conf_dir) $(if $(top_srcdir),-iquote $(top_srcdir)) \
          $(LANG_FLAGS) $(DIAG_FLAGS) \
          $(DBG_FLAGS) $(PLATFORM_CFLAGS) $(EXTRA_FLAGS)
CFLAGS := $(filter-out $(REMOVE_FLAGS),$(CFLAGS))
LDFLAGS := -lm $(CONF_LDFLAGS) $(PLATFORM_LDFLAGS) $(EXTRA_LDFLAGS)

ifneq ($(subst g++,@,$(notdir $(CC))),$(notdir $(CC)))
CFLAGS := $(filter-out -Wmissing-prototypes -Wstrict-prototypes,$(CFLAGS))
endif

# TXR objects
ADD_CONF = $(addprefix $(1)/,$(2))
EACH_CONF = $(foreach conf,opt dbg,\
               $(foreach tgt,$(1),$(call ADD_CONF,$(conf),$(tgt))))

OBJS-y := # make sure OBJ-y is a value variable, not a macro variable
EXTRA_OBJS-y :=

OBJS := txr.o lex.yy.o y.tab.o match.o lib.o regex.o gc.o unwind.o stream.o
OBJS += arith.o hash.o utf8.o filter.o eval.o parser.o rand.o combi.o sysif.o
OBJS += args.o lisplib.o cadr.o struct.o jmp.o
OBJS-$(debug_support) += debug.o
OBJS-$(have_syslog) += syslog.o
OBJS-$(have_glob) += glob.o
OBJS-$(have_posix_sigs) += signal.o
OBJS-$(have_termios) += linenoise/linenoise.o
EXTRA_OBJS-$(add_win_res) += win/txr.res

ifneq ($(have_git),)
SRCS := $(addprefix $(top_srcdir),\
                    $(filter-out lex.yy.c y.tab.c y.tab.h,\
                                 $(shell git $(if $(top_srcdir), \
						--work-tree=$(top_srcdir)) \
                                             --git-dir=$(top_srcdir).git \
                                              ls-files "*.c" "*.h" "*.l" "*.y")))
endif

# MPI objects
MPI_OBJ_BASE=mpi.o mplogic.o

MPI_OBJS := $(addprefix mpi/,$(MPI_OBJ_BASE))

OBJS += $(MPI_OBJS)

DBG_OBJS := $(call ADD_CONF,dbg,$(OBJS) $(OBJS-y))
OPT_OBJS := $(call ADD_CONF,opt,$(OBJS) $(OBJS-y))
OBJS := $(DBG_OBJS) $(OPT_OBJS)

TXR := ./$(PROG)

.SUFFIXES:
MAKEFLAGS += --no-builtin-rules

V = $(if $(VERBOSE),,@)

# Filtering out $(DEP_$@) allows the abbreviated output to show just the direct
# prerequisites without the long laundry list of additional dependencies.
ABBREV = $(if $(VERBOSE),\
           @:,\
           @printf "%s %s -> %s\n" $(1) \
	      "$(patsubst $(top_srcdir)%,%,$(filter-out $(DEP_$@),$^))" $@)
ABBREV3 = $(if $(VERBOSE),@:,@printf "%s %s -> %s\n" $(1) "$(3)" $(2))

define DEPGEN
$(V)sed -e '1s/^/DEP_/' -e '1s/: [^ ]\+/ :=/' < $(1) > $(1:.d=.v)
endef

define COMPILE_C
$(call ABBREV,CC)
$(V)$(CC) $(OPT_FLAGS) $(CFLAGS) -c -o $@ $<
endef

define COMPILE_C_WITH_DEPS
$(call ABBREV,CC)
$(V)mkdir -p $(dir $@)
$(V)$(CC) -MMD -MT $@ $(1) $(CFLAGS) -c -o $@ $<
$(call DEPGEN,${@:.o=.d})
endef

define LINK_PROG
$(call ABBREV,LINK)
$(V)$(CC) $(1) $(CFLAGS) -o $@ $^ $(LDFLAGS)
endef

define WINDRES
$(call ABBREV,RES)
$(V)mkdir -p $(dir $@)
$(V)windres -O coff -DTXR_VER=$(txr_ver) $< $@
endef

ifneq ($(top_srcdir),)
dbg/%.o: $(top_srcdir)%.c
	$(call COMPILE_C_WITH_DEPS,$(DBG_ONLY_FLAGS))

opt/%.o: $(top_srcdir)%.c
	$(call COMPILE_C_WITH_DEPS,$(OPT_FLAGS))

dbg/%.o: $(top_srcdir)%.S
	$(call COMPILE_C_WITH_DEPS,)

opt/%.o: $(top_srcdir)%.S
	$(call COMPILE_C_WITH_DEPS,)
endif

dbg/%.o: %.c
	$(call COMPILE_C_WITH_DEPS,$(DBG_ONLY_FLAGS))

opt/%.o: %.c
	$(call COMPILE_C_WITH_DEPS,$(OPT_FLAGS))

dbg/%.o: %.S
	$(call COMPILE_C_WITH_DEPS,)

opt/%.o: %.S
	$(call COMPILE_C_WITH_DEPS,)

dbg/%-win.o: $(top_srcdir)%.c
	$(call COMPILE_C_WITH_DEPS,-DCONFIG_WIN_MAIN=1)

opt/%-win.o: $(top_srcdir)%.c
	$(call COMPILE_C_WITH_DEPS,-DCONFIG_WIN_MAIN=1 $(OPT_FLAGS))

%.res: %.rc
	$(call WINDRES)

# The following pattern rule is used for test targets built by configure
%.o: %.c
	$(call COMPILE_C)

ifeq ($(PROG),)
.PHONY: notconfigured
notconfigured:
	$(V)echo "configuration missing: you didn't run the configure script!"
	$(V)exit 1
endif

.PHONY: all
all: $(BUILD_TARGETS)

$(PROG): $(OPT_OBJS) $(EXTRA_OBJS-y)
	$(call LINK_PROG,$(OPT_FLAGS))

$(PROG)-dbg: $(DBG_OBJS) $(EXTRA_OBJS-y)
	$(call LINK_PROG,)

$(PROG)-win: $(patsubst %/txr.o,%/txr-win.o,$(OPT_OBJS)) $(EXTRA_OBJS-y)
	$(call LINK_PROG,-mwindows $(OPT_FLAGS))

$(PROG)-win-dbg: $(patsubst %/txr.o,%/txr-win.o,$(DBG_OBJS)) $(EXTRA_OBJS-y)
	$(call LINK_PROG,-mwindows)

# Newline constant
define NL


endef

define DEP
$(1): $(2)

$(eval $(foreach item,$(1),DEP_$(item) += $(2)$(NL)))
endef

# Pull in dependencies
-include $(OBJS:.o=.d) $(OBJS:.o=.v)

# Add dependencies
$(call DEP,$(OBJS) $(EXTRA_OBJS-y),\
           $(conf_dir)/config.make $(conf_dir)/config.h)

$(eval $(foreach item,lex.yy.o txr.o match.o parser.o,\
          $(call DEP,opt/$(item) dbg/$(item),y.tab.h)))

$(eval $(foreach item,y.tab.c y.tab.h lex.yy.c,\
          $(call DEP,$(item),$(conf_dir)/config.make $(conf_dir)/config.h)))

lex.yy.c: $(top_srcdir)parser.l
	$(call ABBREV,LEX)
	$(V)rm -f $@
	$(V)$(LEX) $(LEX_DBG_FLAGS) $<
	$(V)chmod a-w $@

y.tab.h: y.tab.c
	$(V)if ! [ -e y.tab.h ] ; then                            \
	  echo "Someone removed y.tab.h but left y.tab.c" ;       \
	  echo "Remove y.tab.c and re-run make" ;                 \
	  exit 1;                                                 \
	fi

y.tab.c: $(top_srcdir)parser.y
	$(call ABBREV,YACC)
	$(V)rm -f y.tab.c
	$(V)if $(YACC) -v -d $< ; then                            \
	  chmod a-w y.tab.c ;                                     \
	  sed -e '/yyparse/d' < y.tab.h > y.tab.h.tmp &&          \
	    mv y.tab.h.tmp y.tab.h ;                              \
	else                                                      \
	  rm y.tab.c ;                                            \
	  false ;                                                 \
	fi

# Suppress useless sccs id array and unused label warning in byacc otuput.
# Bison-generated parser also tests for this lint define.
$(call EACH_CONF,y.tab.o): CFLAGS += -Dlint

# txr.c needs to know the relative datadir path to do some sysroot
# calculations.

opt/txr.o: CFLAGS += -DPROG_NAME=\"$(PROG)\" \
                     -DTXR_REL_PATH=\"$(bindir_rel)/$(PROG)$(EXE)\"
dbg/txr.o: CFLAGS += -DPROG_NAME=\"$(PROG)-dbg\" \
                     -DTXR_REL_PATH=\"$(bindir_rel)/$(PROG)-dbg$(EXE)\"
opt/txr-win.o: CFLAGS += -DPROG_NAME=\"$(PROG)-win\" \
                         -DTXR_REL_PATH=\"$(bindir_rel)/$(PROG)-win$(EXE)\"
dbg/txr-win.o: CFLAGS += -DPROG_NAME=\"$(PROG)-win-dbg\" \
                         -DTXR_REL_PATH=\"$(bindir_rel)/$(PROG)-win-dbg$(EXE)\"
$(call EACH_CONF,txr.o txr-win.o): CFLAGS += -DEXE_SUFF=\"$(EXE)\"
$(call EACH_CONF,txr.o txr-win.o): CFLAGS += -DTXR_VER=\"$(txr_ver)\"

$(call EACH_CONF,linenoise/linenoise.o): CFLAGS += -D$(termios_define)
.PHONY: rebuild clean repatch distclean

ifeq ($(PROG),)
rebuild clean repatch: notconfigured

distclean:
	$(V)echo "executing generic cleanup for non-configured directory"
	rm -f txr txr.exe txr-dbg txr-dbg.exe y.tab.c lex.yy.c y.tab.h y.output
	rm -rf config
	rm -rf config.*
	rm -rf mpi-1.?.?
else
rebuild: clean repatch $(PROG)

clean: conftest.clean tests.clean
	rm -f $(PROG)$(EXE) $(PROG)-dbg$(EXE) y.tab.c lex.yy.c y.tab.h y.output
	rm -rf opt dbg $(EXTRA_OBJS-y)

distclean: clean
	rm -rf $(conf_dir)
endif

TESTS_TMP := txr.test.out
TESTS_OUT := $(addprefix tst/,\
                $(patsubst %.tl,%.out,\
                   $(patsubst %.txr,%.out,\
                      $(wildcard $(addprefix tests/*/*.,txr tl)))))
TESTS_OK := $(TESTS_OUT:.out=.ok)

.PHONY: tests
tests: $(TESTS_OK)
	$(V)echo "** tests passed!"

tst/tests/001/%: TXR_ARGS := tests/001/data
tst/tests/001/query-1.out: TXR_OPTS := -B
tst/tests/001/query-2.out: TXR_OPTS := -B
tst/tests/001/query-4.out: TXR_OPTS := -B
tst/tests/002/%: TXR_OPTS := -DTESTDIR=tests/002
tst/tests/004/%: TXR_ARGS := -a 123 -b -c
tst/tests/005/%: TXR_ARGS := tests/005/data
tst/tests/005/%: TXR_OPTS := -B
tst/tests/006/%: TXR_ARGS := tests/006/data
tst/tests/006/%: TXR_OPTS := -B
tst/tests/006/freeform-3.out: TXR_ARGS := tests/006/passwd
tst/tests/008/tokenize.out: TXR_ARGS := tests/008/data
tst/tests/008/configfile.out: TXR_ARGS := tests/008/configfile
tst/tests/008/students.out: TXR_ARGS := tests/008/students.xml
tst/tests/008/soundex.out: TXR_ARGS := soundex sowndex lloyd lee jackson robert
tst/tests/008/filtenv.out: TXR_OPTS := -B
tst/tests/009/json.out: TXR_ARGS := $(addprefix tests/009/,webapp.json pass1.json)
tst/tests/010/align-columns.out: TXR_ARGS := tests/010/align-columns.dat
tst/tests/010/block.out: TXR_OPTS := -B
tst/tests/010/reghash.out: TXR_OPTS := -B
tst/tests/013/maze.out: TXR_ARGS := 20 20

tst/tests/002/%: TXR_SCRIPT_ON_CMDLINE := y

tst/tests/011/%: TXR_DBG_OPTS :=
tst/tests/012/%: TXR_DBG_OPTS :=
tst/tests/013/%: TXR_DBG_OPTS :=

.PRECIOUS: tst/%.out
tst/%.out: %.txr
	$(call ABBREV,TXR)
	$(V)mkdir -p $(dir $@)
	$(V)$(if $(TXR_SCRIPT_ON_CMDLINE),\
	       $(TXR) $(TXR_DBG_OPTS) $(TXR_OPTS) -c "$$(cat $<)" \
	          $(TXR_ARGS) > $(TESTS_TMP),\
	       $(TXR) $(TXR_DBG_OPTS) $(TXR_OPTS) $< $(TXR_ARGS) > $(TESTS_TMP))
	$(V)mv $(TESTS_TMP) $@

tst/%.out: %.tl
	$(call ABBREV,TXR)
	$(V)mkdir -p $(dir $@)
	$(V)$(TXR) $(TXR_DBG_OPTS) $(TXR_OPTS) $< $(TXR_ARGS) > $(TESTS_TMP)
	$(V)mv $(TESTS_TMP) $@

%.ok: %.out
	$(V)diff -u $(patsubst tst/%.out,%.expected,$<) $<
	$(V)touch $@

%.expected: %.out
	cp $< $@

.PHONY: tests.clean
tests.clean:
	rm -rf tst

.PHONY: retest
retest: tests.clean tests

define GREP_CHECK
	$(V)if [ $$(grep -E $(1) $(SRCS) | wc -l) -ne $(3) ] ; then \
	      echo "New '$(2)' occurrences have been found:" ; \
	      grep -n -E $(1) $(SRCS) \
	        | sed -e 's/\(.*:.*:\).*/\1 $(2)/' \
	        | grep $(4) ; \
	      exit 1 ; \
	    fi
endef

.PHONY: enforce
enforce:
ifneq ($(have_git),)
	$(call GREP_CHECK,'\<void[	 ]*\*',void *,1,-v 'typedef void \*yyscan_t')
	$(call GREP_CHECK,'	',tabs,0,'.')
	$(call GREP_CHECK,' $$',trailing spaces,0,'.')
else
	$(V)echo "enforce requires the git program and a git repository"
	$(V)exit 1
endif

#
# Installation macro.
#
# $1 - chmod perms
# $2 - source file(s)
# $3 - dest directory
#
define INSTALL
	$(call ABBREV3,INSTALL,$(3),$(2))
	$(V)mkdir -p $(3)
	$(V)cp -f $(2) $(3)
	$(V)for x in $(2) ; do \
	  y=$(strip $(3))/$$(basename $$x) ; \
	  touch -r $$x $$y ; \
	  chmod $(1) $$y ; \
	done
endef

PREINSTALL := :

.PHONY: install
install: $(PROG)
	$(V)$(PREINSTALL)
	$(call INSTALL,0755,txr$(EXE),$(DESTDIR)$(bindir))
	$(call INSTALL,0444,$(top_srcdir)LICENSE,$(DESTDIR)$(datadir))
	$(call INSTALL,0444,$(top_srcdir)METALICENSE,$(DESTDIR)$(datadir))
	$(call INSTALL,0444,$(top_srcdir)txr.1,$(DESTDIR)$(mandir)/man1)
	$(call INSTALL,0444,\
	   $(addprefix $(top_srcdir)share/txr/stdlib/,\
	                  *.txr *.tl),\
	   $(DESTDIR)$(datadir)/stdlib)

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
	$(V)rm -rf tst
	$(V)mkdir -p $(DESTDIR)$(datadir)
	$(call ABBREV3,INSTALL,$(DESTDIR)$(datadir),tests)
	$(V)(find tests | cpio -o 2> /dev/null) \
	    | (cd $(DESTDIR)$(datadir) ; cpio -idum 2> /dev/null)
	$(V)(echo "#!/bin/sh" ; \
	     echo "set -ex" ; \
	     echo "cd $(datadir)" ; \
	     make -s -n tests VERBOSE=y TXR=$(bindir)/txr) \
	     > run.sh
	$(call INSTALL,0755,run.sh,$(DESTDIR)$(datadir)/tests)

#
# Generate web page from man page
#
txr-manpage.html: txr.1 genman.txr
	man2html $< | $(TXR) genman.txr - > $@

txr-manpage.pdf: txr.1
	tbl $< | pdfroff -man --no-toc - > $@

#
# Special targets used by ./configure
#

conftest: conftest.c
	$(call LINK_PROG,)

conftest2: conftest1.c conftest2.c
	$(call LINK_PROG,)

conftest.syms: conftest.o
	$(NM) -n -t o -P $^ > $@

.PHONY: conftest.yacc
conftest.yacc:
	$(V)echo $(YACC)

.PHONY: conftest.ccver
conftest.ccver:
	$(V)$(CC) --version

.PHONY: conftest.clean
conftest.clean:
	$(V)rm -f conftest$(EXE) conftest.[co] \
	conftest2$(EXE) conftest[12].[oc] \
	conftest.err conftest.syms
