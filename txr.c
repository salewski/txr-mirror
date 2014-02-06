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

const wchli_t *version = wli("78");
const wchar_t *progname = L"txr";
val self_path;

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

  prot1(&spec_file_str);
  prot1(&self_path);

  setvbuf(stderr, 0, _IOLBF, 0);

  yyin_stream = std_input;

  if (argc <= 1) {
    hint();
    return EXIT_FAILURE;
  }

  argc--, argv++;

  while (argc > 0 && (*argv)[0] == '-') {
    if (!strcmp(*argv, "--")) {
      argv++, argc--;
      break;
    }

    if (!strcmp(*argv, "-"))
      break;

    if (!strncmp(*argv, "-D", 2)) {
      char *var = *argv + 2;
      char *equals = strchr(var, '=');
      char *has_comma = (equals != 0) ? strchr(equals, ',') : 0;

      if (has_comma) {
        char *pval = equals + 1;
        val list = nil;

        *equals = 0;

        for (;;) {
          size_t piece = strcspn(pval, ",");
          char comma_p = pval[piece];

          pval[piece] = 0;

          list = cons(string_utf8(pval), list);

          if (!comma_p)
            break;

          pval += piece + 1;
        }

        list = nreverse(list);
        bindings = cons(cons(intern(string_utf8(var), nil), list), bindings);
      } else if (equals) {
        char *pval = equals + 1;
        *equals = 0;
        bindings = cons(cons(intern(string_utf8(var), nil),
                             string_utf8(pval)), bindings);
      } else {
        bindings = cons(cons(intern(string_utf8(var), nil),
                             null_string), bindings);
      }

      argc--, argv++;
      continue;
    }

    if (!strcmp(*argv, "--version")) {
      format(std_output, lit("~a: version ~a\n"),
             prog_string, auto_str(version), nao);
      return 0;
    }

    if (!strcmp(*argv, "--help")) {
      help();
      return 0;
    }

    if (!strcmp(*argv, "-a") || !strcmp(*argv, "-c") || !strcmp(*argv, "-f") ||
        !strcmp(*argv, "-e") || !strcmp(*argv, "-p"))
    {
      long optval;
      char *errp;
      char opt = (*argv)[1];

      if (argc == 1) {
        format(std_error, lit("~a: option -~a needs argument\n"),
               prog_string, chr(opt), nao);

        return EXIT_FAILURE;
      }

      argv++, argc--;

      switch (opt) {
      case 'a':
        optval = strtol(*argv, &errp, 10);
        if (*errp != 0) {
          format(std_error, lit("~a: option -~a needs numeric argument, "
                                "not ~a\n"), prog_string, chr(opt),
                                string_utf8(*argv), nao);
          return EXIT_FAILURE;
        }

        opt_arraydims = optval;
        break;
      case 'c':
        specstring = string_utf8(*argv);
        break;
      case 'f':
        spec_file = string_utf8(*argv);
        break;
      case 'e':
        eval_intrinsic(lisp_parse(string_utf8(*argv), std_error), nil);
        evaled = t;
        break;
      case 'p':
        obj_print(eval_intrinsic(lisp_parse(string_utf8(*argv), std_error),
                                 nil), std_output);
        put_char(chr('\n'), std_output);
        evaled = t;
        break;
      }

      argv++, argc--;
      continue;
    }

    if (!strcmp(*argv, "--gc-debug")) {
      opt_gc_debug = 1;
      argv++, argc--;
      continue;
    } else if (!strcmp(*argv, "--vg-debug")) {
#if HAVE_VALGRIND
      opt_vg_debug = 1;
      argv++, argc--;
      continue;
#else
      format(std_error,
             lit("~a: option ~a requires Valgrind support compiled in\n"),
             prog_string, string_utf8(*argv), nao);
      return EXIT_FAILURE;
#endif
    } else if (!strcmp(*argv, "--dv-regex")) {
      opt_derivative_regex = 1;
      argv++, argc--;
      continue;
    } else if (!strcmp(*argv, "--lisp-bindings")) {
      opt_lisp_bindings = 1;
      argv++, argc--;
      continue;
    } else if (!strcmp(*argv, "--debugger")) {
#if CONFIG_DEBUG_SUPPORT
      opt_debugger = 1;
#else
      format(std_error,
             lit("~a: option ~a requires debug support compiled in\n"),
             prog_string, string_utf8(*argv), nao);
      return EXIT_FAILURE;
#endif
      argv++, argc--;
      continue;
    }


    {
      char *popt;
      for (popt = (*argv)+1; *popt != 0; popt++) {
        switch (*popt) {
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
                 prog_string, chr(*popt), nao);
          return EXIT_FAILURE;
#endif
          break;
        case 'a':
        case 'c':
        case 'e':
        case 'p':
        case 'D':
          format(std_error, lit("~a: option -~a does not clump\n"),
                 prog_string, chr(*popt), nao);
          return EXIT_FAILURE;
        case '-':
          format(std_error, lit("~a: unrecognized long option: --~a\n"),
                 prog_string, string_utf8(popt + 1), nao);
          return EXIT_FAILURE;
        default:
          format(std_error, lit("~a: unrecognized option: -~a\n"),
                 prog_string, chr(*popt), nao);
          return EXIT_FAILURE;
        }
      }

      argc--, argv++;
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
  } else {
    if (argc < 1) {
      if (evaled)
        return EXIT_SUCCESS;
      hint();
      return EXIT_FAILURE;
    }

    if (strcmp(*argv, "-") != 0) {
      FILE *in = fopen(*argv, "r");
      val name = string_utf8(*argv);
      if (in == 0)
        uw_throwf(file_error_s, lit("unable to open ~a"), name, nao);
      yyin_stream = make_stdio_stream(in, name);
      spec_file_str = string_utf8(*argv);
    } else {
      spec_file_str = lit("stdin");
    }
    argc--, argv++;
  }


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

    self_path = spec_file_str;

    {
      int retval;
      list_collect_decl(filenames, iter);

      while (*argv)
        iter = list_collect(iter, string_utf8(*argv++));

      retval = extract(spec, filenames, bindings);

      return errors ? EXIT_FAILURE : retval;
    }
  }
}
