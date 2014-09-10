/* Copyright 2009-2014
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * Redistribution of this software in source and binary forms, with or without
 * modification, is permitted provided that the following two conditions are met.
 *
 * Use of this software in any manner constitutes agreement with the disclaimer
 * which follows the two conditions.
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DAMAGES, HOWEVER CAUSED,
 * AND UNDER ANY THEORY OF LIABILITY, ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <dirent.h>
#include <setjmp.h>
#include <stdarg.h>
#include <wchar.h>
#include <signal.h>
#include "config.h"
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_WINDOWS_H
#include <windows.h>
#endif
#include "lib.h"
#include "stream.h"
#include "gc.h"
#include "signal.h"
#include "unwind.h"
#include "parser.h"
#include "match.h"
#include "utf8.h"
#include "debug.h"
#include "syslog.h"
#include "eval.h"
#include "regex.h"
#include "arith.h"
#include "txr.h"

const wchli_t *version = wli(TXR_VER);
const wchar_t *progname = L"txr";
static const char *progname_u8;
static val progpath = nil;
int opt_compat;

/*
 * Can implement an emergency allocator here from a fixed storage
 * pool, which sets an OOM flag. Program can check flag
 * and gracefully terminate instead of aborting like this.
 */
static mem_t *oom_realloc_handler(mem_t *old, size_t size)
{
  format(std_error, lit("~a: out of memory\n"), prog_string, nao);
  put_line(lit("false"), std_error);
  abort();
}

static void help(void)
{
  val text = lit(
"\n"
"txr version ~a\n"
"\n"
"copyright 2014, Kaz Kylheku <kaz@kylheku.com>\n"
"\n"
"usage:\n"
"\n"
"  ~a [ options ] query-file { data-file }*\n"
"\n"
"The query-file or data-file arguments may be specified as -, in which case\n"
"standard input is used. All data-file arguments which begin with a !\n"
"character are treated as command pipes. Those which begin with a $\n"
"are interpreted as directories to read. Leading arguments which begin\n"
"with a - followed by one or more characters, and which are not arguments to\n"
"options are interpreted as options. The -- option indicates the end of the\n"
"options.\n"
"\n"
"If no data-file arguments sare supplied, then the query itself must open a\n"
"a data source prior to attempting to make any pattern match, or it will\n"
"simply fail due to a match which has run out of data.\n"
"\n"
"options:\n"
"\n"
"-Dvar=value            Pre-define variable var, with the given value.\n"
"                       A list value can be specified using commas.\n"
"-Dvar                  Predefine variable var, with empty string value.\n"
"-q                     Quiet: don't report errors during query matching.\n"
"-v                     Verbose: extra logging from matcher.\n"
"-b                     Don't dump list of bindings, or 'false'\n"
"                       on unsuccessful termination.\n"
"-B                     Force list of bindings to be dumped, or false\n"
"                       if termination is unsuccessful.\n"
"-l                     If dumping bindings, use TXR Lisp format.\n"
"-d                     Debugger mode.\n"
"-a N                   Generate array variables up to N dimensions.\n"
"                       N is a decimal integer. The default value is 1.\n"
"                       Additional dimensions beyond N are fudged\n"
"                       by generating numeric suffixes. Implies -B.\n"
"-c query-text          The query is read from the query-text argument\n"
"                       itself. The query-file argument is omitted in\n"
"                       this case; the first argument is a data file.\n"
"-f query-file          Specify the query-file as an option argument.\n"
"                       option, instead of the query-file argument.\n"
"                       This allows #! scripts to pass options through\n"
"                       to the utility.\n"
"-C N                   Request backward-compatible behavior to the\n"
"                       specified version of TXR.\n"
"--help                 You already know!\n"
"--version              Display program version\n"
"--license              Display software license\n"
"                       Use of txr implies agreement with the disclaimer\n"
"                       section at the bottom of the license.\n"
"--lisp-bindings        Synonym for -l\n"
"--debugger             Synonym for -d\n"
"--compat=N             Synonym for -C N\n"
"--gc-delta=N           Invoke garbage collection when malloc activity\n"
"                       increments by N megabytes since last collection.\n"
"\n"
"Options that take no argument can be combined. The -q and -v options\n"
"are mutually exclusive; the right-most one dominates.\n"
"\n"
);
  format(std_output, text, auto_str(version), prog_string, nao);
}

static void hint(void)
{
  format(std_error, lit("~a: incorrect arguments: try --help\n"),
         prog_string, nao);
}

static val remove_hash_bang_line(val spec)
{
  if (!consp(spec))
    return spec;

  {
    val firstline = first(spec);
    val firstelem = first(firstline);
    val item;

    if (stringp(firstelem))
      item = firstelem;
    else if (consp(firstelem) && first(firstelem) == text_s)
      item = second(firstelem);
    else
      return spec;

    if (stringp(item)) {
      val twochars = sub_str(item, zero, two);
      if (equal(twochars, lit("#!")))
        return rest(spec);
    }

    return spec;
  }
}

#if __linux__
static val get_self_path(void)
{
  char self[PATH_MAX] = { 0 };
  int nchar = readlink("/proc/self/exe", self, sizeof self);

  if (nchar < 0 || nchar >= sizeof self)
    return nil;
  return string_utf8(self);
}
#elif HAVE_WINDOWS_H
static val get_self_path(void)
{
  wchar_t self[MAX_PATH] = { 0 };
  DWORD nchar;

  SetLastError(0);
  nchar = GetModuleFileNameW(NULL, self, MAX_PATH);

  if (nchar == 0 ||
      (nchar == MAX_PATH &&
       ((GetLastError() == ERROR_INSUFFICIENT_BUFFER) ||
        (self[MAX_PATH - 1] != 0))))
    return nil;

  return string(self);
}
#else
static val get_self_path(void)
{
  char self[PATH_MAX];

  if (!progname_u8)
    return nil;

  if (realpath(progname_u8, self))
    return string_utf8(self);

  return nil;
}
#endif

static val sysroot_helper(val exepart, val target)
{
  if (match_str(progpath, exepart, negone))
    return format(nil, lit("~a~a"),
                  sub_str(progpath, 0, neg(length(exepart))),
                  target, nao);
  return nil;
}

static val sysroot(val target)
{
  uses_or2;
  return or4(sysroot_helper(lit(TXR_REL_PATH), target),
             sysroot_helper(lit(TXR_REL_PATH EXE_SUFF), target),
             sysroot_helper(lit(PROG_NAME), target),
             sysroot_helper(lit(PROG_NAME EXE_SUFF), target));
}

static void sysroot_init(void)
{
#if HAVE_WINDOWS_H
  val slash = regex_compile(lit("\\\\"), nil);
#endif
  prot1(&progpath);
  progpath = get_self_path();
#if HAVE_WINDOWS_H
  progpath = regsub(slash, lit("/"), progpath);
#endif
  reg_var(intern(lit("stdlib"), user_package),
          sysroot(lit("share/txr/stdlib")));
  reg_var(intern(lit("*txr-version*"), user_package),
          toint(lit(TXR_VER), nil));
}

static int license(void)
{
  int retval = EXIT_SUCCESS;

  uw_catch_begin(cons(error_s, nil), esym, eobj);

  {
    val path = sysroot(lit("share/txr/LICENSE"));
    val lic = open_file(path, lit("r"));
    val line;

    put_char(chr('\n'), std_output);

    while ((line = get_line(lic)))
      put_line(line, std_output);

    put_char(chr('\n'), std_output);

    close_stream(lic, nil);
  }

  uw_catch (esym, eobj) {
    format(std_output, lit("~a:\nThis TXR installation might be unlicensed.\n"), eobj, nao);
    retval = EXIT_FAILURE;
  }

  uw_unwind { }

  uw_catch_end;

  return retval;
}

int txr_main(int argc, char **argv);

int main(int argc, char **argv)
{
  val stack_bottom = nil;
  progname = argv[0] ? utf8_dup_from(argv[0]) : progname;
  progname_u8 = argv[0];
  init(progname, oom_realloc_handler, &stack_bottom);
  match_init();
  parse_init();
  debug_init();
#if HAVE_SYSLOG
  syslog_init();
#endif
  sysroot_init();
  return txr_main(argc, argv);
}

static void requires_arg(val opt)
{
    format(std_error, lit("~a: option --~a requires an argument\n"),
           prog_string, opt, nao);
}

static int do_fixnum_opt(int (*opt_func)(val), val opt, val arg)
{
  if (arg) {
    val optval = int_str(arg, nil);

    if (!optval || !fixnump(optval)) {
      format(std_error, lit("~a: option -~a needs a small integer "
                            "argument, not ~a\n"), prog_string, opt,
                            arg, nao);
      return 0;
    }

    return opt_func(optval);
  }

  requires_arg(opt);
  return 0;
}

static int compat(val optval)
{
  if ((opt_compat = c_num(optval)) < 97) {
    format(std_error, lit("~a: compatibility with versions "
                          "lower than 97 not supported by version ~a\n"),
           prog_string, auto_str(version), nao);
    return 0;
  }
  compat_fixup(opt_compat);
  return 1;
}

static int array_dim(val optval)
{
  opt_arraydims = c_num(optval);
  opt_print_bindings = 1;
  return 1;
}

static int gc_delta(val optval)
{
  opt_gc_delta = c_num(mul(optval, num_fast(1048576)));
  return 1;
}

int txr_main(int argc, char **argv)
{
  val specstring = nil;
  val spec = nil;
  val spec_file = nil;
  val bindings = nil;
  val evaled = nil;
  val spec_file_str;
  int match_loglevel = opt_loglevel;
  val arg_undo = nil, arg;
  val parse_stream = std_input;
  list_collect_decl(arg_list, arg_tail);

  setvbuf(stderr, 0, _IOLBF, 0);

  if (argc <= 1) {
    hint();
    return EXIT_FAILURE;
  }

  while (*argv)
    arg_tail = list_collect(arg_tail, string_utf8(*argv++));

  reg_var(intern(lit("*full-args*"), user_package), arg_list);

  arg_list = cdr(arg_list);

  for (arg = upop(&arg_list, &arg_undo);
       arg && car(arg) == chr('-');
       arg = upop(&arg_list, &arg_undo))
  {
    if (equal(arg, lit("--")))
      break;

    if (equal(arg, lit("-")))
      break;

    /* Odd case 1: --args is followed by an arbitrary delimiting
     * character, not necessarily = */
    if (match_str(arg, lit("--args"), zero) && ge(length(arg), num(7))) {
      val sep = sub_str(arg, num(6), num(7));
      arg = sub_str(arg, num(7), nil);
      arg_list = append2(split_str(arg, sep), arg_list);
      continue;
    }

    /* Odd case 2: -Dfoo=bar syntax. */
    if (equal(sub(arg, zero, two), lit("-D"))) {
      val dopt_arg = sub(arg, two, t);
      cons_bind(var, def, split_str(dopt_arg, lit("=")));
      val deflist = if2(def, split_str(car(def), lit(",")));

      if (rest(deflist))
        bindings = cons(cons(intern(var, nil), deflist), bindings);
      else if (deflist)
        bindings = cons(cons(intern(var, nil), car(deflist)), bindings);
      else 
        bindings = cons(cons(intern(var, nil), t), bindings);

      continue;
    }

    /* Regular long opts */
    if (match_str(arg, lit("--"), zero)) {
      val no_dashes = sub(arg, two, t);
      val pair = partition_star(no_dashes, pos(chr('='), no_dashes, nil, nil));
      val opt = pop(&pair);
      val org = pop(&pair);

      /* Long opts with arguments */
      if (equal(opt, lit("gc-delta"))) {
        if (!do_fixnum_opt(gc_delta, opt, arg))
          return EXIT_FAILURE;
        continue;
      }

      if (equal(opt, lit("compat"))) {
        if (!do_fixnum_opt(compat, opt, org))
          return EXIT_FAILURE;
        continue;
      }

      /* Long opts with no arguments */
      if (org) {
        format(std_error,
               lit("~a: option --~a takes no argument, ~a given\n"),
               prog_string, opt, org, nao);
        return EXIT_FAILURE;
      }

      if (equal(opt, lit("version"))) {
        format(std_output, lit("~a: version ~a\n"),
               prog_string, auto_str(version), nao);
        return 0;
      }

      if (equal(opt, lit("help"))) {
        help();
        return 0;
      }

      if (equal(opt, lit("license")))
        return license();

      if (equal(opt, lit("gc-debug"))) {
        opt_gc_debug = 1;
        continue;
      } else if (equal(opt, lit("vg-debug"))) {
#if HAVE_VALGRIND
        opt_vg_debug = 1;
        continue;
#else
        format(std_error,
               lit("~a: option ~a requires Valgrind support compiled in\n"),
               prog_string, arg, nao);
        return EXIT_FAILURE;
#endif
      } else if (equal(opt, lit("dv-regex"))) {
        opt_derivative_regex = 1;
        continue;
      } else if (equal(opt, lit("lisp-bindings"))) {
        opt_lisp_bindings = 1;
        opt_print_bindings = 1;
        continue;
      } else if (equal(arg, lit("debugger"))) {
#if CONFIG_DEBUG_SUPPORT
        opt_debugger = 1;
        continue;
#else
        format(std_error,
               lit("~a: option ~a requires debug support compiled in\n"),
               prog_string, arg, nao);
        return EXIT_FAILURE;
#endif
      }
    }


    /* Single letter options with args: non-clumping. */
    if (length(arg) == two && find(ref(arg, one), lit("acfepC"), nil, nil))
    {
      val opt = chr_str(arg, one);

      if (!arg_list) {
        requires_arg(opt);
        return EXIT_FAILURE;
      }

      arg = upop(&arg_list, &arg_undo);

      switch (c_chr(opt)) {
      case 'a':
        if (!do_fixnum_opt(array_dim, opt, arg))
          return EXIT_FAILURE;
        break;
      case 'C':
        if (!do_fixnum_opt(compat, opt, arg))
          return EXIT_FAILURE;
        break;
      case 'c':
        specstring = arg;
        break;
      case 'f':
        spec_file = arg;
        break;
      case 'e':
        eval_intrinsic(lisp_parse(arg, std_error, colon_k),
                       make_env(bindings, nil, nil));
        evaled = t;
        break;
      case 'p':
        obj_print(eval_intrinsic(lisp_parse(arg, std_error, colon_k),
                                 make_env(bindings, nil, nil)), std_output);
        put_char(chr('\n'), std_output);
        evaled = t;
        break;
      }

      continue;
    }

    /* Clumping single-letter options. */
    {
      val optchars = cdr(arg);

      for (; optchars != nil; optchars = cdr(optchars)) {
        val opch = car(optchars);

        switch (c_chr(opch)) {
        case 'v':
          match_loglevel = 2;
          break;
        case 'q':
          match_loglevel = 0;
          break;
        case 'b': /* deprecated */
          break;
        case 'B':
          opt_print_bindings = 1;
          break;
        case 'l':
          opt_lisp_bindings = 1;
          opt_print_bindings = 1;
          break;
        case 'd':
#if CONFIG_DEBUG_SUPPORT
          opt_debugger = 1;
#else
          format(std_error,
                 lit("~a: option ~a requires debug support compiled in\n"),
                 prog_string, opch, nao);
          return EXIT_FAILURE;
#endif
          break;
        case 'a':
        case 'c':
        case 'e':
        case 'p':
        case 'f':
        case 'C':
        case 'D':
          format(std_error, lit("~a: option -~a does not clump\n"),
                 prog_string, opch, nao);
          return EXIT_FAILURE;
        case '-':
          format(std_error, lit("~a: unrecognized long option: --~a\n"),
                 prog_string, cdr(optchars), nao);
          return EXIT_FAILURE;
        default:
          format(std_error, lit("~a: unrecognized option: -~a\n"),
                 prog_string, opch, nao);
          return EXIT_FAILURE;
        }
      }
    }
  }

  if (specstring && spec_file) {
    format(std_error, lit("~a: cannot specify both -f and -c\n"),
           prog_string, nao);
    return EXIT_FAILURE;
  }

  if (specstring) {
    spec_file_str = lit("cmdline");
    if (gt(length_str(specstring), zero) &&
        chr_str(specstring, minus(length_str(specstring), one)) != chr('\n'))
      specstring = cat_str(list(specstring, string(L"\n"), nao), nil);
    parse_stream = make_string_byte_input_stream(specstring);
    if (arg)
      arg_list = arg_undo;
  } else if (spec_file) {
    if (wcscmp(c_str(spec_file), L"-") != 0) {
      FILE *in = w_fopen(c_str(spec_file), L"r");
      if (in == 0)
        uw_throwf(file_error_s, lit("unable to open ~a"), spec_file, nao);
      parse_stream = make_stdio_stream(in, spec_file);
      spec_file_str = spec_file;
    } else {
      spec_file_str = lit("stdin");
    }
    if (arg)
      arg_list = arg_undo;
  } else {
    if (!arg) {
      if (evaled)
        return EXIT_SUCCESS;
      hint();
      return EXIT_FAILURE;
    }

    if (!equal(arg, lit("-"))) {
      FILE *in = w_fopen(c_str(arg), L"r");
      if (in == 0)
        uw_throwf(file_error_s, lit("unable to open ~a"), arg, nao);
      parse_stream = make_stdio_stream(in, arg);
      spec_file_str = arg;
    } else {
      spec_file_str = lit("stdin");
    }
  }

  reg_var(intern(lit("*args*"), user_package), arg_list);

  {
    int gc = gc_state(0);
    parser_t parser;
    parse(parse_stream, spec_file_str, &parser);
    gc_state(gc);

    if (parser.errors)
      return EXIT_FAILURE;

    spec = remove_hash_bang_line(parser.syntax_tree);

    opt_loglevel = match_loglevel;

    if (opt_loglevel >= 2) {
      format(std_error, lit("spec:\n~s\n"), spec, nao);
      format(std_error, lit("bindings:\n~s\n"), bindings, nao);
    }

    reg_var(intern(lit("*self-path*"), user_package), spec_file_str);

    {
      int retval = extract(spec, arg_list, bindings);
      return parser.errors ? EXIT_FAILURE : retval;
    }
  }
}
