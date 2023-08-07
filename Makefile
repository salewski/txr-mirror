# Copyright 2009-2023
# Kaz Kylheku <kaz@kylheku.com>
# Vancouver, Canada
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

-include config.make

ifeq ($(parallelmake),)
.NOTPARALLEL:
endif

VERBOSE :=
TXR_CFLAGS := -iquote . $(if $(top_srcdir), -iquote $(top_srcdir)) \
              $(LANG_FLAGS) $(DIAG_FLAGS) $(DBG_FLAGS) $(PLATFORM_CFLAGS) \
				  $(EXTRA_FLAGS) $(CFLAGS) $(CPPFLAGS)
TXR_CFLAGS := $(filter-out $(REMOVE_FLAGS),$(TXR_CFLAGS))
TXR_LDFLAGS := $(CONF_LDFLAGS) $(PLATFORM_LDFLAGS) $(EXTRA_LDFLAGS) $(LDFLAGS)
TXR_LDLIBS := -lm $(CONF_LDLIBS) $(PLAFORM_LDLIBS) $(EXTRA_LDLIBS) $(LDLIBS)

ifneq ($(subst g++,@,$(notdir $(TXR_CC))),$(notdir $(TXR_CC)))
TXR_CFLAGS := $(filter-out -Wmissing-prototypes -Wstrict-prototypes,$(TXR_CFLAGS))
endif

# TXR objects
ADD_CONF = $(addprefix $(1)/,$(2))
EACH_CONF = $(foreach conf,opt dbg,\
               $(foreach tgt,$(1),$(call ADD_CONF,$(conf),$(tgt))))

OBJS-y := # make sure OBJ-y is a value variable, not a macro variable
EXTRA_OBJS-y :=

OBJS := txr.o lex.yy.o y.tab.o match.o lib.o regex.o gc.o unwind.o stream.o
OBJS += arith.o hash.o utf8.o filter.o eval.o parser.o rand.o combi.o sysif.o
OBJS += args.o autoload.o cadr.o struct.o itypes.o buf.o jmp.o protsym.o ffi.o
OBJS += strudel.o vm.o tree.o time.o psquare.o
OBJS += chksum.o chksums/sha1.o chksums/sha256.o chksums/crc32.o chksums/md5.o
OBJS += linenoise/linenoise.o
OBJS-$(debug_support) += debug.o
OBJS-$(have_syslog) += syslog.o
OBJS-$(have_glob) += glob.o
OBJS-$(have_ftw) += ftw.o
OBJS-$(have_posix_sigs) += signal.o
OBJS-$(have_sockets) += socket.o
OBJS-$(have_termios) += termios.o
OBJS-$(have_zlib) += gzio.o
EXTRA_OBJS-$(add_win_res) += win/txr.res

STDLIB_SRCS := $(wildcard stdlib/*.tl)
STDLIB_TLOS := $(patsubst %.tl,%.tlo,$(STDLIB_SRCS))

STDLIB_EARLY_PATS := %/optimize.tlo %/param.tlo %/compiler.tlo %/asm.tlo
STDLIB_EARLY_TLOS := $(filter $(STDLIB_EARLY_PATS),$(STDLIB_TLOS))
STDLIB_LATE_TLOS := $(filter-out $(STDLIB_EARLY_TLOS),$(STDLIB_TLOS))

ifneq ($(have_git),)
SRCS := $(addprefix $(top_srcdir),\
                    $(filter-out lex.yy.c y.tab.c y.tab.h,\
                                 $(shell git $(if $(top_srcdir),\
                                               --work-tree=$(top_srcdir)) \
                                             --git-dir=$(top_srcdir).git \
                                             ls-files "*.c" "*.h" "*.l" "*.y")))
endif

SHIPPED := lex.yy.c y.tab.c y.tab.h

# MPI objects
MPI_OBJ_BASE=mpi.o

MPI_OBJS := $(addprefix mpi/,$(MPI_OBJ_BASE))

OBJS += $(MPI_OBJS)

DBG_OBJS := $(call ADD_CONF,dbg,$(OBJS) $(OBJS-y))
OPT_OBJS := $(call ADD_CONF,opt,$(OBJS) $(OBJS-y))
OBJS := $(DBG_OBJS) $(OPT_OBJS)

TXR := ./$(PROG)

.SUFFIXES:
MAKEFLAGS += --no-builtin-rules

V = $(if $(VERBOSE),,@)

ABBREV = $(if $(VERBOSE),\
           @:,\
           @printf "%s %s -> %s\n" $(1) \
           "$(patsubst $(top_srcdir)%,%,$<)" $@)
# Filtering out $(DEP_$@) allows the abbreviated output to show just the direct
# prerequisites without the long laundry list of additional dependencies.
ABBREVN = $(if $(VERBOSE),\
            @:,\
            @printf "%s %s -> %s\n" $(1) \
            "$(patsubst $(top_srcdir)%,%,$(filter-out $(DEP_$@),$^))" $@)
ABBREV3 = $(if $(VERBOSE),@:,@printf "%s %s -> %s\n" $(1) "$(3)" $(2))
ABBREV3SH = $(if $(VERBOSE),:,printf "%s %s -> %s\n" $(1) "$(3)" $(2))

define DEPGEN
$(V)sed -e ':x'                                         \
        -e '/\\$$/ {'                                   \
        -e   'N; s/\\\n//'                              \
        -e   'tx'                                       \
        -e '}' < $(1) |                                 \
  sed -e '1s/^/DEP_/' -e '1s/: [^ ]\+/ :=/' > $(1:.d=.v)
endef

define SH
$(if $(VERBOSE),                                        \
  $(1),                                                 \
  $(V)cmd='$(subst ','\'',$(1))' ;                      \
  if ! eval "$$cmd" ; then                              \
    printf "make: failing command:\n%s\n" "$$cmd";      \
    exit 1;                                             \
  fi)
endef

define COMPILE_C
$(call ABBREV,CC)
$(call SH,$(TXR_CC) $(OPT_FLAGS) $(TXR_CFLAGS) -c -o $@ $<)
endef

define COMPILE_C_WITH_DEPS
$(call ABBREV,CC)
$(call SH,mkdir -p $(dir $@))
$(call SH,$(TXR_CC) -MMD -MT $@ $(1) $(TXR_CFLAGS) -c -o $@ $<)
$(call DEPGEN,${@:.o=.d})
endef

define LINK_PROG
$(call ABBREVN,LINK)
$(call SH,$(TXR_CC) $(1) $(TXR_CFLAGS) $(TXR_LDFLAGS) -o $@ $^ $(TXR_LDLIBS))
endef

define WINDRES
$(call ABBREV,RES)
$(call SH,mkdir -p $(dir $@))
$(call SH,windres -O coff -DTXR_VER=$(txr_ver) $< $@)
endef

define COMPILE_TL
$(call ABBREV,TXR)
$(call SH,rm -f $@.tmp)
$(call SH,$(TXR) --in-package=sys --compile=$<:$@.tmp)
$(call SH,mv $@.tmp $@)
endef

LN := ln

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
	$(call COMPILE_C_WITH_DEPS,)

opt/%-win.o: $(top_srcdir)%.c
	$(call COMPILE_C_WITH_DEPS, $(OPT_FLAGS))

win/%.res: $(top_srcdir)win/%.rc $(top_srcdir)win/%.ico
	$(call WINDRES)

%.tlo: %.tl | $(PROG)
	$(call COMPILE_TL)

# The following pattern rule is used for test targets built by configure
%.o: %.c
	$(call COMPILE_C)

ifeq ($(PROG),)
.PHONY: notconfigured
notconfigured:
	$(V)echo "configuration missing: you didn't run the configure script!"
	$(V)exit 1
endif

ifneq ($(build_in_srcdir),y)
ifneq ($(wildcard $(top_srcdir)/config.h $(top_srcdir)/y.tab.h),)
.PHONY: tainted
tainted:
	$(V)echo "Source directory $(top_srcdir) is in a configured state."
	$(V)echo "Generated files there will interfere with the build here."
	$(V)echo "Please run \"make distclean\" in $(top_srcdir) first."
	$(V)exit 1
endif
endif

.PHONY: all

all: $(BUILD_TARGETS) $(STDLIB_TLOS)

$(STDLIB_LATE_TLOS): | $(STDLIB_EARLY_TLOS)

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

CM := ,

define DEP
$(1): $(2)

$(eval $(foreach item,$(1),DEP_$(item) += $(2)$(NL)))
endef

# Pull in dependencies
-include $(OBJS:.o=.d) $(OBJS:.o=.v)

# Add dependencies
$(call DEP,$(OBJS) $(EXTRA_OBJS-y),config.make config.h)

$(eval $(foreach item,lex.yy.o txr.o match.o parser.o,\
          $(call DEP,opt/$(item) dbg/$(item),y.tab.h)))

$(eval $(foreach item,y.tab.c y.tab.h lex.yy.c,\
          $(call DEP,$(item),config.make config.h)))

ifeq ($(maintainer),y)

lex.yy.c: $(top_srcdir)parser.l
	$(call ABBREV,LEX)
	$(call SH,rm -f $@)
	$(call SH,                                              \
	  if $(TXR_LEX) $(LEX_DBG_FLAGS) $< ; then              \
	    sed -e s@//.*@@ < $@ > $@.tmp ;                     \
	    mv $@.tmp $@ ;                                      \
	  else                                                  \
	    exit 1 ;                                            \
	  fi)
	$(call SH,chmod a-w $@)

%.tab.c %.tab.h: $(top_srcdir)parser.%
	$(call ABBREV,YACC)
	$(call SH,rm -f y.tab.c y.tab.h)
	$(call SH,                                              \
	  if $(TXR_YACC) -v -d $< ; then                        \
	    chmod a-w y.tab.c ;                                 \
	  else                                                  \
	    rm y.tab.c ;                                        \
	    false ;                                             \
	  fi)

shipped:
	$(V)$(foreach NAME,$(SHIPPED),                          \
	   if ! cmp -s $(NAME) $(NAME).shipped; then            \
	      $(call ABBREV3SH,COPY,$(NAME),$(NAME).shipped);   \
	      cp $(NAME) $(NAME).shipped;                       \
	   fi;)

else

%: $(top_srcdir)%.shipped
	$(call ABBREV,COPY)
	$(call SH,rm -f $@)
	$(call SH,cp $< $@)

endif

# Suppress useless sccs id array and unused label warning in byacc otuput.
# Bison-generated parser also tests for this lint define.
$(call EACH_CONF,y.tab.o): TXR_CFLAGS += -Dlint

opt/ffi.o: TXR_CFLAGS += $(LIBFFI_CFLAGS)

dbg/ffi.o: TXR_CFLAGS += $(LIBFFI_CFLAGS)

# txr.c needs to know the relative datadir path to do some sysroot
# calculations.

opt/txr.o: TXR_CFLAGS += -DPROG_NAME=\"$(PROG)\" \
                         -DTXR_REL_PATH=\"$(bindir_rel)/$(PROG)$(EXE)\" \
								 -DTXR_INST_PATH=\"$(bindir)/$(PROG)$(EXE)\"
dbg/txr.o: TXR_CFLAGS += -DPROG_NAME=\"$(PROG)-dbg\" \
                         -DTXR_REL_PATH=\"$(bindir_rel)/$(PROG)-dbg$(EXE)\" \
								 -DTXR_INST_PATH=\"$(bindir)/$(PROG)$(EXE)\"
opt/txr-win.o: TXR_CFLAGS += -DPROG_NAME=\"$(PROG)-win\" \
                             -DTXR_REL_PATH=\"$(bindir_rel)/$(PROG)-win$(EXE)\"
dbg/txr-win.o: TXR_CFLAGS += -DPROG_NAME=\"$(PROG)-win-dbg\" \
                             -DTXR_REL_PATH=\"$(bindir_rel)/$(PROG)-win-dbg$(EXE)\"

old_build_id=$(shell cat .build_id 2> /dev/null)
ifneq ($(build_id_exp),$(old_build_id))
$(shell rm -f $(call EACH_CONF,txr.o txr-win.o))
endif
$(shell printf "%s" "$(build_id_exp)" > .build_id)

ifneq ($(build_id_exp),)
$(call EACH_CONF,txr.o txr-win.o): TXR_CFLAGS += -DTXR_BUILD_ID=\"$(build_id_exp)\"
endif
$(call EACH_CONF,txr.o txr-win.o): TXR_CFLAGS += -DEXE_SUFF=\"$(EXE)\"
$(call EACH_CONF,txr.o txr-win.o): TXR_CFLAGS += -DTXR_VER=\"$(txr_ver)\"

$(call EACH_CONF,linenoise/linenoise.o): TXR_CFLAGS += -D$(termios_define)
.PHONY: rebuild clean clean-tlo distclean

ifeq ($(PROG),)
rebuild clean: notconfigured

distclean:
	$(V)echo "executing generic cleanup for non-configured directory"
	rm -f txr txr.exe txr-dbg txr-dbg.exe txr-win.exe txr-win-dbg.exe .build_id
	rm -f y.tab.c lex.yy.c y.tab.h y.output
	rm -rf opt dbg tst
	rm -f stdlib/*.tlo run.sh
	rm -f config.h config.make reconfigure txr-manpage.html txr-manpage.pdf
	# artifacts from prior versions of TXR
	rm -rf config config.* share/txr/stdlib mpi-1.?.?
	-rmdir share/txr
	-rmdir share
else
rebuild: clean $(PROG)

clean: conftest.clean clean-c clean-tlo

clean-c:
	rm -f $(PROG)$(EXE) $(PROG)-dbg$(EXE) y.tab.c lex.yy.c y.tab.h y.output
	rm -f y.tab.h.old
	rm -f $(PROG)-win$(EXE) $(PROG)-win-dbg$(EXE) .build_id
	rm -rf opt dbg tst
	rm -f $(EXTRA_OBJS-y) run.sh

clean-tlo:
	rm -f $(STDLIB_TLOS)

distclean: clean
	rm -f config.h config.make reconfigure txr-manpage.html txr-manpage.pdf
endif

TESTS_OK := $(addprefix tst/,\
               $(patsubst %.tl,%.ok,\
                  $(patsubst %.txr,%.ok,\
                     $(wildcard $(addprefix tests/*/*.,txr tl)))))

ifneq ($(have_ubsan),)
TESTS_OK := $(filter-out %/012/stack.ok,$(TESTS_OK))
endif

.PHONY: tests
tests: $(TESTS_OK)
	$(V)echo "** tests passed!"

.PHONY: retest
retest:
	$(V)rm -rf tst
	$(V)$(MAKE) tests

%.expected:
	$(V)touch $@

tst/tests/000/binding.ok: TXR_OPTS := -B
tst/tests/001/%: TXR_ARGS := tests/001/data
tst/tests/001/query-1.ok: TXR_OPTS := -B
tst/tests/001/query-2.ok: TXR_OPTS := -B
tst/tests/001/query-4.ok: TXR_OPTS := -B
tst/tests/001/query-5.ok: TXR_OPTS := -B
tst/tests/002/%: TXR_OPTS := -C 142 -DTESTDIR=tests/002
tst/tests/002/variant.ok: TXR_ARGS := tests/002/variant
tst/tests/002/variant.ok: TXR_OPTS := -C 300
tst/tests/004/%: TXR_ARGS := -a 123 -b -c
tst/tests/005/%: TXR_ARGS := tests/005/data
tst/tests/005/%: TXR_OPTS := -B
tst/tests/006/%: TXR_ARGS := tests/006/data
tst/tests/006/%: TXR_OPTS := -B
tst/tests/006/freeform-3.ok: TXR_ARGS := tests/006/passwd
tst/tests/008/tokenize.ok: TXR_ARGS := tests/008/data
tst/tests/008/configfile.ok: TXR_ARGS := tests/008/configfile
tst/tests/008/students.ok: TXR_ARGS := tests/008/students.xml
tst/tests/008/soundex.ok: TXR_ARGS := soundex sowndex lloyd lee jackson robert
tst/tests/008/filtenv.ok: TXR_OPTS := -B
tst/tests/008/no-stdin-hang.ok: TXR_OPTS := -n
tst/tests/009/json.ok: TXR_ARGS := $(addprefix tests/009/,webapp.json pass1.json)
tst/tests/010/align-columns.ok: TXR_ARGS := tests/010/align-columns.dat
tst/tests/010/block.ok: TXR_OPTS := -B
tst/tests/010/reghash.ok: TXR_OPTS := -B
tst/tests/010/eof-status.ok: TXR_OPTS := -B
tst/tests/013/maze.ok: TXR_ARGS := 20 20
tst/tests/018/chmod.ok: TXR_ARGS := tst/tests/018/tempfile

tst/tests/002/%: TXR_SCRIPT_ON_CMDLINE := y

tst/tests/011/%: TXR_DBG_OPTS :=
tst/tests/012/%: TXR_DBG_OPTS :=
tst/tests/013/%: TXR_DBG_OPTS :=
tst/tests/014/%: TXR_DBG_OPTS :=
tst/tests/015/%: TXR_DBG_OPTS :=
tst/tests/016/%: TXR_DBG_OPTS :=
tst/tests/017/%: TXR_DBG_OPTS :=
tst/tests/018/%: TXR_DBG_OPTS :=
tst/tests/019/%: TXR_DBG_OPTS :=
tst/tests/019/load-hook.ok: TXR_DBG_OPTS := --free-all

tst/tests/014/dgram-stream.ok: | tst/tests/014/socket-basic.ok

TST_EXPECTED  = $(word 2,$^)
TST_OUT = $(patsubst %.expected,tst/%.out,$(TST_EXPECTED))

.PRECIOUS: tst/%.out
tst/%.ok: %.txr %.expected $(TXR)
	$(call ABBREV,TXR)
	$(call SH,mkdir -p $(dir $@))
	$(call SH,                                                            \
	  $(if $(TXR_SCRIPT_ON_CMDLINE),                                      \
	    $(TXR) $(TXR_DBG_OPTS) $(TXR_OPTS) -c "$$(cat $<)"                \
	    $(TXR_ARGS) > $(TST_OUT),                                         \
	    $(TXR) $(TXR_DBG_OPTS) $(TXR_OPTS) $< $(TXR_ARGS) > $(TST_OUT)) ; \
	  case $$? in                                                         \
	     ( 0 ) diff -u $(TST_EXPECTED) $(TST_OUT) ;;                      \
	     ( 13 ) printf "SKIP %s\n" $< ; exit 0 ;;                         \
	     ( * ) exit 1 ;;                                                  \
	  esac)
	$(call SH,touch $@)

tst/%.ok: %.tl %.expected $(TXR)
	$(call ABBREV,TXR)
	$(call SH,mkdir -p $(dir $@))
	$(call SH,\
	  $(TXR) $(TXR_DBG_OPTS) $(TXR_OPTS) $< $(TXR_ARGS) > $(TST_OUT))
	$(call SH,                                                            \
	  if ! diff -u $(TST_EXPECTED) $(TST_OUT) ; then                      \
	    exit 1 ;                                                          \
	  fi)
	$(call SH,touch $@)

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
	$(call SH,mkdir -p $(3))
	$(call SH,cp -f $(2) $(3))
	$(call SH,                                \
	  for x in $(2) ; do                      \
	    y=$(strip $(3))/$$(basename $$x) ;    \
	    touch -r $$x $$y ;                    \
	    chmod $(1) $$y ;                      \
	  done)
endef

define HARDLINK
  $(call ABBREV3,HARDLINK,$(1),$(2))
  $(call SH,$(LN) -f $(1) $(2) || printf "(HARDLINK failed)\n")
endef

PREINSTALL := :

.PHONY: install
install: $(PROG)
	$(V)$(PREINSTALL)
	$(call INSTALL,0755,txr$(EXE),$(DESTDIR)$(bindir))
	$(call HARDLINK,\
	   $(DESTDIR)$(bindir)/txr$(EXE),\
	   $(DESTDIR)$(bindir)/txrlisp$(EXE))
	$(call HARDLINK,\
	   $(DESTDIR)$(bindir)/txr$(EXE),\
	   $(DESTDIR)$(bindir)/txrvm$(EXE))
	$(call INSTALL,0444,$(top_srcdir)LICENSE,$(DESTDIR)$(datadir))
	$(call INSTALL,0444,$(top_srcdir)METALICENSE,$(DESTDIR)$(datadir))
	$(call INSTALL,0444,$(top_srcdir)txr.1,$(DESTDIR)$(mandir)/man1)
	$(call INSTALL,0444,\
	   $(addprefix stdlib/,*.txr *.tl *.tlo),\
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
	$(call ABBREV3,INSTALL,$(DESTDIR)$(datadir),tests)
	$(call SH,rm -rf tst)
	$(call SH,mkdir -p $(DESTDIR)$(datadir))
	$(call SH,(find tests | cpio -o 2> /dev/null) \
	              | (cd $(DESTDIR)$(datadir) ; cpio -idum 2> /dev/null))
	$(call SH,                                                     \
	  (echo "#!/bin/sh" ;                                          \
	   echo "set -ex" ;                                            \
	   echo 'cd "$$(dirname "$$(dirname "$$0")")"' ;               \
	   make -s -n tests VERBOSE=y TXR=$(bindir)/txr)               \
	  > run.sh)
	$(call INSTALL,0755,run.sh,$(DESTDIR)$(datadir)/tests)

#
# Generate web page from man page
#
txr-manpage.html: txr.1 genman.txr
	man2html $< | $(TXR) genman.txr > $@

txr-manpage.pdf: txr.1 checkman.txr
	$(TXR) checkman.txr $<
	tbl $< | pdfroff -ww -man --no-toc - > $@
	[ $$SOURCE_DATE_EPOCH ] && $(TXR) pdf-clobber-stamps.tl || true

#
# Special targets used by ./configure
#

conftest: conftest.c
	$(call LINK_PROG,)

conftest2: conftest1.c conftest2.c
	$(call LINK_PROG,)

.PHONY: conftest.yacc
conftest.yacc:
	$(V)echo $(TXR_YACC)

.PHONY: conftest.yacc-version
conftest.yacc-version:
	$(V)$(TXR_YACC) --version 2> /dev/null || true

.PHONY: conftest.ccver
conftest.ccver:
	$(V)$(TXR_CC) --version

.PHONY: conftest.darwin
conftest.darwin:
	$(V)if echo | $(CC) -dM -E - | grep -s __APPLE__ >  /dev/null 2>&1 ; then \
	  echo yes ; \
	fi

.PHONY: conftest.android
conftest.android:
	$(V)if echo | $(CC) -dM -E - | grep -s __ANDROID__ >  /dev/null 2>&1 ; then \
	  echo yes ; \
	fi

.PHONY: conftest.clean
conftest.clean:
	$(V)rm -f conftest$(EXE) conftest.[co] \
	conftest2$(EXE) conftest[12].[oc] \
	conftest.err
