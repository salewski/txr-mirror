/* Copyright 2009-2014
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * BSD License:
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in
 *      the documentation and/or other materials provided with the
 *      distribution.
 *   3. The name of the author may not be used to endorse or promote
 *      products derived from this software without specific prior
 *      written permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
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
#include "txr.h"

const wchli_t *version = wli("83");
const wchar_t *progname = L"txr";

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
"copyright 2012, Kaz Kylheku <kaz@kylheku.com>\n"
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
"-a num                 Generate array variables up to num-dimensions.\n"
"                       Default is 1. Additional dimensions are fudged\n"
"                       by generating numeric suffixes\n"
"-c query-text          The query is read from the query-text argument\n"
"                       itself. The query-file argument is omitted in\n"
"                       this case; the first argument is a data file.\n"
"-f query-file          Specify the query-file as an option argument.\n"
"                       option, instead of the query-file argument.\n"
"                       This allows #! scripts to pass options through\n"
"                       to the utility.\n"
"--help                 You already know!\n"
"--version              Display program version\n"
"--lisp-bindings        Synonym for -l\n"
"--debugger             Synonym for -d\n"
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

int txr_main(int argc, char **argv);

int main(int argc, char **argv)
{
  val stack_bottom = nil;
  progname = argv[0] ? utf8_dup_from(argv[0]) : progname;
  init(progname, oom_realloc_handler, &stack_bottom);
  match_init();
  parse_init();
  debug_init();
#if HAVE_SYSLOG
  syslog_init();
#endif
  return txr_main(argc, argv);
}

int txr_main(int argc, char **argv)
{
  val specstring = nil;
  val spec = nil;
  val spec_file = nil;
  val bindings = nil;
  val evaled = nil;
  int match_loglevel = opt_loglevel;
  val arg;
  list_collect_decl(arg_list, arg_tail);

  prot1(&spec_file_str);

  setvbuf(stderr, 0, _IOLBF, 0);

  yyin_stream = std_input;

  if (argc <= 1) {
    hint();
    return EXIT_FAILURE;
  }

  while (*argv)
    arg_tail = list_collect(arg_tail, string_utf8(*argv++));

  reg_var(intern(lit("*full-args*"), user_package), arg_list);

  arg_list = cdr(arg_list);

  for (arg = pop(&arg_list); arg && car(arg) == chr('-'); arg = pop(&arg_list))
  {
    if (equal(arg, lit("--")))
      break;

    if (equal(arg, lit("-"))) {
      push(arg, &arg_list);
      break;
    }

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

    if (equal(arg, lit("--version"))) {
      format(std_output, lit("~a: version ~a\n"),
             prog_string, auto_str(version), nao);
      return 0;
    }

    if (equal(arg, lit("--help"))) {
      help();
      return 0;
    }

    if (memqual(arg, list(lit("-a"), lit("-c"), lit("-f"),
                          lit("-e"), lit("-p"), nao)))
    {
      val opt = chr_str(arg, one);

      if (!arg_list) {
        format(std_error, lit("~a: option -~a needs argument\n"),
               prog_string, opt, nao);

        return EXIT_FAILURE;
      }

      arg = pop(&arg_list);

      switch (c_chr(opt)) {
      case 'a':
        {
          val optval = int_str(arg, nil);

          if (!optval || !fixnump(optval)) {
            format(std_error, lit("~a: option -~a needs a small integer "
                                  "argument, not ~a\n"), prog_string, opt,
                                  arg, nao);
            return EXIT_FAILURE;
          }

          opt_arraydims = c_num(optval);
        }
        break;
      case 'c':
        specstring = arg;
        break;
      case 'f':
        spec_file = arg;
        break;
      case 'e':
        eval_intrinsic(lisp_parse(arg, std_error), nil);
        evaled = t;
        break;
      case 'p':
        obj_print(eval_intrinsic(lisp_parse(arg, std_error),
                                 nil), std_output);
        put_char(chr('\n'), std_output);
        evaled = t;
        break;
      }

      continue;
    }

    if (equal(arg, lit("--gc-debug"))) {
      opt_gc_debug = 1;
      continue;
    } else if (equal(arg, lit("--vg-debug"))) {
#if HAVE_VALGRIND
      opt_vg_debug = 1;
      continue;
#else
      format(std_error,
             lit("~a: option ~a requires Valgrind support compiled in\n"),
             prog_string, arg, nao);
      return EXIT_FAILURE;
#endif
    } else if (equal(arg, lit("--dv-regex"))) {
      opt_derivative_regex = 1;
      continue;
    } else if (equal(arg, lit("--lisp-bindings"))) {
      opt_lisp_bindings = 1;
      continue;
    } else if (equal(arg, lit("--debugger"))) {
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
        case 'b':
          opt_nobindings = 1;
          break;
        case 'B':
          opt_nobindings = -1;
          break;
        case 'l':
          opt_lisp_bindings = 1;
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
    yyin_stream = make_string_byte_input_stream(specstring);
    if (arg)
      push(arg, &arg_list);
  } else if (spec_file) {
    if (wcscmp(c_str(spec_file), L"-") != 0) {
      FILE *in = w_fopen(c_str(spec_file), L"r");
      if (in == 0)
        uw_throwf(file_error_s, lit("unable to open ~a"), spec_file, nao);
      yyin_stream = make_stdio_stream(in, spec_file);
      spec_file_str = spec_file;
    } else {
      spec_file_str = lit("stdin");
    }
    if (arg)
      push(arg, &arg_list);
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
      yyin_stream = make_stdio_stream(in, arg);
      spec_file_str = arg;
    } else {
      spec_file_str = lit("stdin");
    }
  }

  reg_var(intern(lit("*args*"), user_package), arg_list);

  {
    int gc = gc_state(0);
    yyparse();
    yylex_destroy();
    gc_state(gc);

    if (errors)
      return EXIT_FAILURE;

    spec = remove_hash_bang_line(get_spec());

    opt_loglevel = match_loglevel;

    if (opt_loglevel >= 2) {
      format(std_error, lit("spec:\n~s\n"), spec, nao);
      format(std_error, lit("bindings:\n~s\n"), bindings, nao);
    }

    reg_var(intern(lit("*self-path*"), user_package), spec_file_str);

    {
      int retval = extract(spec, arg_list, bindings);
      return errors ? EXIT_FAILURE : retval;
    }
  }
}
