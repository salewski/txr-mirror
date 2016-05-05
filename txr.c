/* Copyright 2009-2016
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
#include <limits.h>
#include <dirent.h>
#include <stdarg.h>
#include <wchar.h>
#include <signal.h>
#include "config.h"
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_WINDOWS_H
#include <windows.h>
#undef TEXT
#endif
#include "lib.h"
#include "stream.h"
#include "gc.h"
#include "signal.h"
#include "unwind.h"
#include "y.tab.h"
#include "parser.h"
#include "match.h"
#include "utf8.h"
#include "debug.h"
#include "eval.h"
#include "regex.h"
#include "arith.h"
#include "sysif.h"
#include "txr.h"

const wchli_t *version = wli(TXR_VER);
const wchar_t *progname = L"txr";
static const char *progname_u8;
static val prog_path = nil, sysroot_path = nil;
int opt_noninteractive;
int opt_compat;
int opt_dbg_expansion;
val stdlib_path;

/*
 * Can implement an emergency allocator here from a fixed storage
 * pool, which sets an OOM flag. Program can check flag
 * and gracefully terminate instead of aborting like this.
 */
static mem_t *oom_realloc_handler(mem_t *old, size_t size)
{
  format(std_error, lit("~a: out of memory\n"), prog_string, nao);
  if (opt_print_bindings)
    put_line(lit("false"), std_output);
  abort();
}

static void help(void)
{
  val text = lit(
"\n"
"TXR Version ~a\n"
"\n"
"Copyright 2015, Kaz Kylheku <kaz@kylheku.com>\n"
"\n"
"Usage:\n"
"\n"
"  ~a [ options ] query-file { data-file }*\n"
"\n"
#if HAVE_TERMIOS
"If no arguments are present, TXR will enter into interactive listener mode.\n"
"\n"
#endif
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
"-i                     Interactive TXR Lisp listener mode.\n"
"                       (Requires compiled-in support.)\n"
"-d                     Debugger mode.\n"
"-n                     Noninteractive input mode for standard input stream,\n"
"                       even if its connected to a terminal device.\n"
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
"-e expression          Evaluate TXR Lisp expression. Can be specified\n"
"                       multiple times. The query-file arg becomes optional.\n"
"-p expression          Like -e, but prints the result of the expression\n"
"                       using the prinl function.\n"
"-P expression          Like -p, but prints using pprinl.\n"
"-t expression          Like -p, but prints using tprint.\n"
"-C N                   Request backward-compatible behavior to the\n"
"                       specified version of TXR.\n"
"--help                 You already know!\n"
"--version              Display program version\n"
"--license              Display software license\n"
"                       Use of txr implies agreement with the disclaimer\n"
"                       section at the bottom of the license.\n"
"--lisp                 Treat unsuffixed query files as TXR Lisp.\n"
"--lisp-bindings        Synonym for -l\n"
"--debugger             Synonym for -d\n"
"--noninteractive       Synonym for -n\n"
"--compat=N             Synonym for -C N\n"
"--gc-delta=N           Invoke garbage collection when malloc activity\n"
"                       increments by N megabytes since last collection.\n"
#if HAVE_FORK_STUFF
"--reexec               Re-execute TXR with remaining arguments.\n"
#endif
"--debug-autoload       Allow debugger to step through library auto-loading.\n"
"--debug-expansion      Allow debugger to step through macro-expansion of query.\n"
"--yydebug              Debug Yacc parser, if compiled with YYDEBUG support.\n"
"--gc-debug             Enable a garbage collector stress test (slow).\n"
"--vg-debug             Enable Valgrind integration, if compiled in.\n"
"--dv-regex             Handle all regexes using derivative-based back-end.\n"
"\n"
"Options that take no argument can be combined. The -q and -v options\n"
"are mutually exclusive; the right-most one dominates.\n"
"\n"
);
  format(std_output, text, static_str(version), prog_string, nao);
}

#if HAVE_TERMIOS
static void banner(void)
{
  format(std_output,
         lit("This is the TXR Lisp interactive listener of TXR ~a.\n"
             "Use the :quit command or type Ctrl-D on empty line to exit.\n"),
         static_str(version), nao);
}
#else
static void hint(void)
{
  format(std_error, lit("~a: incorrect arguments: try --help\n"),
         prog_string, nao);
}
#endif

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

  if (nchar < 0 || nchar >= convert(int, sizeof self))
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

static val maybe_sysroot(val exepart)
{
  return if2(match_str(prog_path, exepart, negone),
             sysroot_path = sub_str(prog_path, 0, neg(length(exepart))));
}

static val substitute_basename(val edited_path, val source_path)
{
  val lslash = rpos(chr('/'), edited_path, nil, nil);
  val rslash = rpos(chr('/'), source_path, nil, nil);
  val basename = if3(rslash,
                     sub_str(source_path, succ(rslash), t),
                     source_path);

  return if3(lslash,
             format(nil, lit("~a~a"),
                    sub_str(edited_path, 0, succ(lslash)),
                    basename, nao),
             basename);
}

static val sysroot(val target)
{
  return format(nil, lit("~a~a"), sysroot_path, target, nao);
}

static void sysroot_init(void)
{
#if HAVE_WINDOWS_H
  val slash = regex_compile(lit("\\\\"), nil);
#endif
  protect(&prog_path, &sysroot_path, &stdlib_path, (val *) 0);
  prog_path = get_self_path();
#if HAVE_WINDOWS_H
  prog_path = regsub(slash, lit("/"), prog_path);
#endif

  if (!(maybe_sysroot(lit(TXR_REL_PATH)) ||
        maybe_sysroot(lit(TXR_REL_PATH EXE_SUFF)) ||
        maybe_sysroot(lit(PROG_NAME)) ||
        maybe_sysroot(lit(PROG_NAME EXE_SUFF)) ||
        maybe_sysroot(substitute_basename(lit(TXR_REL_PATH), prog_path))))
  {
    format(std_error, lit("~a: unable to calculate sysroot\n"),
           prog_string, nao);
    sysroot_path = lit("");
  }

  stdlib_path = sysroot(lit("share/txr/stdlib"));

  reg_varl(intern(lit("stdlib"), user_package), stdlib_path);
  reg_varl(intern(lit("*txr-version*"), user_package),
           toint(lit(TXR_VER), nil));
  reg_varl(intern(lit("txr-version"), user_package),
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
  repress_privilege();
  progname = argv[0] ? utf8_dup_from(argv[0]) : progname;
  progname_u8 = argv[0];
  init(progname, oom_realloc_handler, &stack_bottom);
  match_init();
  debug_init();
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
  int compat = c_num(optval);
  int min = compat_fixup(compat);

  if (min) {
    format(std_error, lit("~a: compatibility with versions "
                          "lower than ~a not supported by version ~a\n"),
           prog_string, num(min), static_str(version), nao);
    return 0;
  }

  opt_compat = compat;
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

#ifndef CONFIG_DEBUG_SUPPORT
static void no_dbg_support(val arg)
{
  format(std_error,
         lit("~a: option ~a requires debug support compiled in\n"),
         prog_string, arg, nao);
}
#endif

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
  val txr_lisp_p = nil;
  val enter_repl = nil;
  val args_s = intern(lit("*args*"), user_package);
  val self_path_s = intern(lit("self-path"), user_package);
  val compat_var = lit("TXR_COMPAT");
  val compat_val = getenv_wrap(compat_var);
  list_collect_decl(arg_list, arg_tail);

  setvbuf(stderr, 0, _IOLBF, 0);

  if (compat_val && length(compat_val) != zero) {
    val value = int_str(compat_val, nil);
    if (!value) {
      format(std_error,
             lit("~a: environment variable ~a=~a must be decimal integer\n"),
             prog_string, compat_var, compat_val, nao);
      return EXIT_FAILURE;
    }
    if (!compat(value)) {
      format(std_error, lit("~a: caused by environment variable ~a=~a\n"),
             prog_string, compat_var, compat_val, nao);
      return EXIT_FAILURE;
    }
  }

  if (argc <= 1) {
    drop_privilege();
#if HAVE_TERMIOS
    banner();
    goto repl;
#else
    hint();
    return EXIT_FAILURE;
#endif
  }

  while (*argv)
    arg_tail = list_collect(arg_tail, string_utf8(*argv++));

  reg_var(intern(lit("*full-args*"), user_package), arg_list);
  reg_var(intern(lit("*args-full*"), user_package), arg_list);

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

    /* Odd case 2: --eargs is followed by an arbitrary delimiting
     * character, not necessarily = */
    if (match_str(arg, lit("--eargs"), zero) && ge(length(arg), num(8))) {
      val sep = sub_str(arg, num(7), num(8));
      val arg2;
      if (!arg_list) {
        format(std_error,
               lit("~a: --eargs=[...] must be followed by an argument\n"),
               prog_string, nao);
        return EXIT_FAILURE;
      }
      arg = sub_str(arg, num(8), nil);
      arg2 = upop(&arg_list, &arg_undo);
      arg_list = append2(mapcar(curry_123_3(func_n3(regsub),
                                            regex_compile(lit("{}"), nil),
                                            arg2),
                                split_str(arg, sep)),
                         arg_list);
      continue;
    }

    /* Odd case 3: -Dfoo=bar syntax. */
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
        drop_privilege();
        format(std_error,
               lit("~a: option --~a takes no argument, ~a given\n"),
               prog_string, opt, org, nao);
        return EXIT_FAILURE;
      }

      if (equal(opt, lit("version"))) {
        drop_privilege();
        format(std_output, lit("~a: version ~a\n"),
               prog_string, static_str(version), nao);
        return 0;
      }

      if (equal(opt, lit("help"))) {
        drop_privilege();
        help();
        return 0;
      }

      if (equal(opt, lit("license"))) {
        drop_privilege();
        return license();
      }

      if (equal(opt, lit("gc-debug"))) {
        drop_privilege();
        opt_gc_debug = 1;
        continue;
      } else if (equal(opt, lit("vg-debug"))) {
        drop_privilege();
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
      } else if (equal(opt, lit("lisp"))) {
        txr_lisp_p = t;
        continue;
#if HAVE_FORK_STUFF
      } else if (equal(opt, lit("reexec"))) {
        exec_wrap(prog_path, arg_list);
        return EXIT_FAILURE;
#endif
      } else if (equal(opt, lit("debugger"))) {
        drop_privilege();
#if CONFIG_DEBUG_SUPPORT
        opt_debugger = 1;
        continue;
#else
        no_dbg_support(arg);
        return EXIT_FAILURE;
#endif
      } else if (equal(opt, lit("debug-autoload"))) {
        drop_privilege();
#if CONFIG_DEBUG_SUPPORT
        opt_debugger = 1;
        opt_dbg_autoload = 1;
        continue;
#else
        no_dbg_support(opt);
        return EXIT_FAILURE;
#endif
      } else if (equal(opt, lit("debug-expansion"))) {
        drop_privilege();
#if CONFIG_DEBUG_SUPPORT
        opt_debugger = 1;
        opt_dbg_expansion = 1;
        continue;
#else
        no_dbg_support(opt);
        return EXIT_FAILURE;
#endif
      } else if (equal(opt, lit("yydebug"))) {
        drop_privilege();
        if (have_yydebug) {
          yydebug_onoff(1);
          format(std_error,
                 lit("~a: option --~a takes no argument, ~a given\n"),
                 prog_string, opt, org, nao);
          continue;
        } else {
          format(std_error,
                 lit("~a: option ~a requires YYDEBUG support compiled in\n"),
                 prog_string, arg, nao);
          return EXIT_FAILURE;
        }
      } else if (equal(opt, lit("noninteractive"))) {
        opt_noninteractive = 1;
        stream_set_prop(std_input, real_time_k, nil);
        continue;
      }
    }

    /* Single letter options with args: non-clumping. */
    if (length(arg) == two && find(ref(arg, one), lit("acfepPtC"), nil, nil))
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
        drop_privilege();
        reg_varl(self_path_s, lit("cmdline-expr"));
        reg_var(args_s, arg_list);

        eval_intrinsic(lisp_parse(arg, std_error, colon_k,
                                  lit("cmdline-expr"), colon_k),
                       make_env(bindings, nil, nil));
        evaled = t;
        arg_list = cdr(lookup_global_var(args_s));
        break;
      case 'p':
      case 'P':
      case 't':
        {
          val (*pf)(val obj, val out) = if3(c_chr(opt) == 'p',
                                            prinl,
                                            if3(c_chr(opt) == 'P',
                                                pprinl,
                                                tprint));
          drop_privilege();
          reg_varl(self_path_s, lit("cmdline-expr"));
          reg_var(args_s, arg_list);
          pf(eval_intrinsic(lisp_parse(arg, std_error, colon_k,
                                       lit("cmdline-expr"), colon_k),
                            make_env(bindings, nil, nil)), std_output);
          evaled = t;
          arg_list = cdr(lookup_global_var(args_s));
        }
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
        case 'i':
          drop_privilege();
#if HAVE_TERMIOS
          enter_repl = t;
          break;
#else
          format(std_error,
                 lit("~a: option ~a requires a platform with termios\n"),
                 prog_string, arg, nao);
          return EXIT_FAILURE;
#endif
        case 'd':
          drop_privilege();
#if CONFIG_DEBUG_SUPPORT
          opt_debugger = 1;
#else
          no_dbg_support(opch);
          return EXIT_FAILURE;
#endif
          break;
        case 'n':
          opt_noninteractive = 1;
          stream_set_prop(std_input, real_time_k, nil);
          break;
        case 'a':
        case 'c':
        case 'e':
        case 'p':
        case 'P':
        case 'f':
        case 'C':
        case 't':
        case 'D':
          drop_privilege();
          format(std_error, lit("~a: option -~a does not clump\n"),
                 prog_string, opch, nao);
          return EXIT_FAILURE;
        case '-':
          drop_privilege();
          format(std_error, lit("~a: unrecognized long option: --~a\n"),
                 prog_string, cdr(optchars), nao);
          return EXIT_FAILURE;
        default:
          drop_privilege();
          format(std_error, lit("~a: unrecognized option: -~a\n"),
                 prog_string, opch, nao);
          return EXIT_FAILURE;
        }
      }
    }
  }

  if (specstring && spec_file) {
    drop_privilege();
    format(std_error, lit("~a: cannot specify both -f and -c\n"),
           prog_string, nao);
    return EXIT_FAILURE;
  }

  if (specstring) {
    drop_privilege();
    spec_file_str = lit("cmdline");
    if (gt(length_str(specstring), zero) &&
        chr_str(specstring, minus(length_str(specstring), one)) != chr('\n'))
      specstring = cat_str(list(specstring, string(L"\n"), nao), nil);
    parse_stream = make_string_byte_input_stream(specstring);
    if (arg)
      arg_list = arg_undo;
  } else if (spec_file) {
    if (wcscmp(c_str(spec_file), L"-") != 0) {
      open_txr_file(spec_file, &txr_lisp_p, &spec_file_str, &parse_stream);
      simulate_setuid_setgid(parse_stream);
    } else {
      drop_privilege();
      spec_file_str = lit("stdin");
    }
    if (arg)
      arg_list = arg_undo;
  } else {
    if (!arg) {
      drop_privilege();
      if (enter_repl)
        goto repl;
      if (evaled)
        return EXIT_SUCCESS;
#if HAVE_TERMIOS
      banner();
      goto repl;
#else
      hint();
      return EXIT_FAILURE;
#endif
    }

    if (!equal(arg, lit("-"))) {
      open_txr_file(arg, &txr_lisp_p, &spec_file_str, &parse_stream);
      simulate_setuid_setgid(parse_stream);
    } else {
      drop_privilege();
      spec_file_str = lit("stdin");
    }
  }

  reg_var(args_s, arg_list);
  reg_varl(intern(lit("self-path"), user_package), spec_file_str);

  if (!txr_lisp_p)
  {
    int gc = gc_state(0);
    parser_t parser;
    parse_once(parse_stream, spec_file_str, &parser);
    gc_state(gc);

    close_stream(parse_stream, nil);

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
      val result = extract(spec, arg_list, bindings);
      cons_bind (new_bindings, success, result);

      if (enter_repl) {
        bindings = new_bindings;
        goto repl;
      }

      return (parser.errors || !success) ? EXIT_FAILURE : 0;
    }
  }

  {
    val result = read_eval_stream(parse_stream, std_error, t);

    close_stream(parse_stream, nil);

    if (!enter_repl)
      return result ? 0 : EXIT_FAILURE;
  }

repl:
#if HAVE_TERMIOS
  if (compat_val)
    format(std_output,
           lit("Note: operating in TXR ~a compatibility mode "
               "due to environment variable.\n"),
           num(opt_compat), nao);
  repl(bindings, std_input, std_output);
#endif
  return 0;
}
