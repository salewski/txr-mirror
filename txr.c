/* Copyright 2009
 * Kaz Kylheku <kkylheku@gmail.com>
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
#include <assert.h>
#include <limits.h>
#include <dirent.h>
#include <setjmp.h>
#include <wchar.h>
#include <locale.h>
#include "lib.h"
#include "stream.h"
#include "gc.h"
#include "unwind.h"
#include "parser.h"
#include "match.h"
#include "utf8.h"
#include "txr.h"

const wchar_t *version = L"020";
const wchar_t *progname = L"txr";
const wchar_t *spec_file = L"stdin";
obj_t *spec_file_str;

/*
 * Can implement an emergency allocator here from a fixed storage
 * pool, which sets an OOM flag. Program can check flag
 * and gracefully terminate instead of aborting like this.
 */
void *oom_realloc_handler(void *old, size_t size)
{
  fwprintf(stderr, L"%ls: out of memory\n", progname);
  fputws(L"false", stderr);
  abort();
}

void help(void)
{
  const wchar_t *text =
L"\n"
L"txr version %ls\n"
L"\n"
L"copyright 2009, Kaz Kylheku <kkylheku@gmail.com>\n"
L"\n"
L"usage:\n"
L"\n"
L"  %ls [ options ] query-file { data-file }*\n"
L"\n"
L"The query-file or data-file arguments may be specified as -, in which case\n"
L"standard input is used. All data-file arguments which begin with a !\n"
L"character are treated as command pipes. Those which begin with a $\n"
L"are interpreted as directories to read. Leading arguments which begin\n"
L"with a - followed by one or more characters, and which are not arguments to\n"
L"options are interpreted as options. The -- option indicates the end of the\n"
L"options.\n"
L"\n"
L"If no data-file arguments sare supplied, then the query itself must open a\n"
L"a data source prior to attempting to make any pattern match, or it will\n"
L"simply fail due to a match which has run out of data.\n"
L"\n"
L"options:\n"
L"\n"
L"-Dvar=value            Pre-define variable var, with the given value.\n"
L"                       A list value can be specified using commas.\n"
L"-Dvar                  Predefine variable var, with empty string value.\n"
L"-q                     Quiet: don't report errors during query matching.\n"
L"-v                     Verbose: extra logging from matcher.\n"
L"-b                     Don't dump list of bindings.\n"
L"-a num                 Generate array variables up to num-dimensions.\n"
L"                       Default is 1. Additional dimensions are fudged\n"
L"                       by generating numeric suffixes\n"
L"-c query-text          The query is read from the query-text argument\n"
L"                       itself. The query-file argument is omitted in\n"
L"                       this case; the first argument is a data file.\n"
L"-f query-file          Specify the query-file as an option argument.\n"
L"                       option, instead of the query-file argument.\n"
L"                       This allows #! scripts to pass options through\n"
L"                       to the utility.\n"
L"--help                 You already know!\n"
L"--version              Display program version\n"
L"\n"
L"Options that take no argument can be combined. The -q and -v options\n"
L"are mutually exclusive; the right-most one dominates.\n"
L"\n"
  ;
  fwprintf(stdout, text, version, progname);
}

void hint(void)
{
  fwprintf(stderr, L"%ls: incorrect arguments: try --help\n", progname);
}

obj_t *remove_hash_bang_line(obj_t *spec)
{
  if (!consp(spec))
    return spec;

  {
    obj_t *shbang = string(L"#!");
    obj_t *firstline = first(spec);
    obj_t *items = rest(firstline);

    if (stringp(first(items))) {
      obj_t *twochars = sub_str(first(items), zero, two);
      if (equal(twochars, shbang))
        return rest(spec);
    }

    return spec;
  }
}

static int txr_main(int argc, char **argv);

int main(int argc, char **argv)
{
  obj_t *stack_bottom = nil;
  progname = argv[0] ? utf8_dup_from(argv[0]) : progname;
  init(progname, oom_realloc_handler, &stack_bottom);
  setlocale(LC_CTYPE, "en_US.UTF-8");
  return txr_main(argc, argv);
}

static int txr_main(int argc, char **argv)
{
  obj_t *specstring = nil;
  obj_t *spec = nil;
  obj_t *bindings = nil;
  int match_loglevel = opt_loglevel;

  prot1(&spec_file_str);

  yyin_stream = std_input;
  prot1(&yyin_stream);

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
        char *val = equals + 1;
        obj_t *list = nil;

        *equals = 0;

        for (;;) {
          size_t piece = strcspn(val, ",");
          char comma_p = val[piece];

          val[piece] = 0;

          list = cons(string_utf8(val), list);

          if (!comma_p)
            break;

          val += piece + 1;
        }

        list = nreverse(list);
        bindings = cons(cons(intern(string_utf8(var)), list), bindings);
      } else if (equals) {
        char *val = equals + 1;
        *equals = 0;
        bindings = cons(cons(intern(string_utf8(var)), string_utf8(val)), bindings);
      } else {
        bindings = cons(cons(intern(string_utf8(var)), null_string), bindings);
      }

      argc--, argv++;
      continue;
    }

    if (!strcmp(*argv, "--version")) {
      wprintf(L"%ls: version %ls\n", progname, version);
      return 0;
    }

    if (!strcmp(*argv, "--help")) {
      help();
      return 0;
    }

    if (!strcmp(*argv, "-a") || !strcmp(*argv, "-c") || !strcmp(*argv, "-f")) {
      long val;
      char *errp;
      char opt = (*argv)[1];

      if (argc == 1) {
        fwprintf(stderr, L"%ls: option %c needs argument\n", progname, opt);

        return EXIT_FAILURE;
      }

      argv++, argc--;

      switch (opt) {
      case 'a':
        val = strtol(*argv, &errp, 10);
        if (*errp != 0) {
          fwprintf(stderr, L"%ls: option %c needs numeric argument, not %s\n",
                   progname, opt, *argv);
          return EXIT_FAILURE;
        }

        opt_arraydims = val;
        break;
      case 'c':
        specstring = string_utf8(*argv);
        break;
      case 'f':
        spec_file_str = string_utf8(*argv);
        break;
      }

      argv++, argc--;
      continue;
    }

    if (!strcmp(*argv, "--gc-debug")) {
      opt_gc_debug = 1;
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
        case 'a':
        case 'c':
        case 'D':
          fwprintf(stderr, L"%ls: option -%c does not clump\n",
                   progname, *popt);
          return EXIT_FAILURE;
        case '-':
          fwprintf(stderr, L"%ls: unrecognized long option: --%s\n",
                   progname, popt + 1);
          return EXIT_FAILURE;
        default:
          fwprintf(stderr, L"%ls: unrecognized option: %c\n", progname, *popt);
          return EXIT_FAILURE;
        }
      }

      argc--, argv++;
    }
  }

  if (specstring && spec_file_str) {
    fwprintf(stderr, L"%ls: cannot specify both -f and -c\n", progname);
    return EXIT_FAILURE;
  }

  if (specstring) {
    spec_file = L"cmdline";
    spec_file_str = string(spec_file);
    if (gt(length_str(specstring), zero) && 
        chr_str(specstring, minus(length_str(specstring), one)) != chr('\n'))
      specstring = cat_str(list(specstring, string(L"\n"), nao), nil);
    yyin_stream = make_string_byte_input_stream(specstring);
  } else if (spec_file_str) {
    if (wcscmp(c_str(spec_file_str), L"-") != 0) {
      FILE *in = w_fopen(c_str(spec_file_str), L"r");
      if (in == 0)
        uw_throwcf(file_error, L"unable to open %ls", c_str(spec_file_str));
      yyin_stream = make_stdio_stream(in, spec_file_str, t, nil);
    } else {
      spec_file = L"stdin";
    }
  } else {
    if (argc < 1) {
      hint();
      return EXIT_FAILURE;
    }

    if (strcmp(*argv, "-") != 0) {
      FILE *in = fopen(*argv, "r");
      if (in == 0)
        uw_throwcf(file_error, L"unable to open %s", *argv);
      yyin_stream = make_stdio_stream(in, string_utf8(*argv), t, nil);
      spec_file = utf8_dup_from(*argv);
    } else {
      spec_file = L"stdin";
    }
    argc--, argv++;
    spec_file_str = string(spec_file);
  }


  {
    int gc = gc_state(0);
    yyparse();
    gc_state(gc);

    if (errors)
      return EXIT_FAILURE;

    spec = remove_hash_bang_line(get_spec());

    opt_loglevel = match_loglevel;

    if (opt_loglevel >= 2) {
      format(std_error, L"spec:\n~s\n", spec, nao);
      format(std_error, L"bindings:\n~s\n", bindings, nao);
    }

    {
      int retval;
      list_collect_decl(filenames, iter);

      while (*argv)
        list_collect(iter, string_utf8(*argv++));

      retval = extract(spec, filenames, bindings);

      return errors ? EXIT_FAILURE : retval;
    }
  }
}
