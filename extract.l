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

%{

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <limits.h>
#include <errno.h>
#include <dirent.h>
#include "y.tab.h"
#include "lib.h"
#include "gc.h"
#include "extract.h"

#define YY_NO_UNPUT

const char *version = "011";
const char *progname = "txr";
const char *spec_file = "stdin";
long lineno = 1;
int opt_loglevel = 1;   /* 0 - quiet; 1 - normal; 2 - verbose */
int opt_nobindings = 0;
int opt_arraydims = 1;

static int nesting;
static int closechar;
static int errors;

/*
 * Can implement an emergency allocator here from a fixed storage
 * pool, which sets an OOM flag. Program can check flag
 * and gracefully terminate instead of aborting like this.
 */
void *oom_realloc_handler(void *old, size_t size)
{
  fprintf(stderr, "%s: out of memory\n", progname);
  puts("false");
  abort();
}

void yyerror(const char *s)
{
  yyerrorlf(1, lineno, "%s", s);
  errors++;
}

void yyerrorf(int level, const char *s, ...)
{
  if (opt_loglevel >= level) {
    va_list vl;
    va_start (vl, s);
    fprintf(stderr, "%s: (%s:%ld): ", progname, spec_file, lineno);
    vfprintf(stderr, s, vl);
    putc('\n', stderr);
    va_end (vl);
  }

  if (level < 2)
    errors++;
}

void yyerrorlf(int level, long spec_lineno, const char *s, ...)
{
  if (opt_loglevel >= level) {
    va_list vl;
    va_start (vl, s);
    fprintf(stderr, "%s: (%s:%ld): ", progname, spec_file, spec_lineno);
    vfprintf(stderr, s, vl);
    putc('\n', stderr);
    va_end (vl);
  }

  if (level < 2)
    errors++;
}

void yybadtoken(int tok, const char *context)
{
  const char *problem = 0;

  switch (tok) {
  case TEXT:    problem = "text"; break;
  case IDENT:   problem = "identifier"; break;
  case ALL:     problem = "\"all\""; break;
  case SOME:    problem = "\"some\""; break;
  case NONE:    problem = "\"none\""; break;
  case MAYBE:   problem = "\"maybe\""; break;
  case AND:     problem = "\"and\""; break;
  case OR:      problem = "\"or\""; break;
  case END:     problem = "\"end\""; break;
  case COLLECT: problem = "\"collect\""; break;
  case UNTIL:   problem = "\"until\""; break;
  case COLL:    problem = "\"coll\""; break;
  case OUTPUT:  problem = "\"output\""; break;
  case REPEAT:  problem = "\"repeat\""; break;
  case REP:     problem = "\"rep\""; break;
  case SINGLE:  problem = "\"single\""; break;
  case FIRST:   problem = "\"first\""; break;
  case LAST:    problem = "\"last\""; break;
  case EMPTY:   problem = "\"empty\""; break;
  case NUMBER:  problem = "\"number\""; break;
  case REGCHAR: problem = "regular expression character"; break;
  }

  if (problem != 0)
    if (context)
      yyerrorlf(1, lineno, "misplaced %s in %s", problem, context);
    else
      yyerrorlf(1, lineno, "unexpected %s", problem);
  else
    if (context)
      yyerrorlf(1, lineno, "unterminated %s", context);
    else
      yyerrorlf(1, lineno, "unexpected end of input");
}

static int char_esc(int letter)
{
  switch (letter) {
  case 'a': return '\a';
  case 'b': return '\b';
  case 't': return '\t';
  case 'n': return '\n';
  case 'v': return '\v';
  case 'f': return '\f';
  case 'r': return '\r';
  case 'e': return 27;
  }

  abort();
}

static int num_esc(char *num)
{
  if (num[0] == 'x') {
    if (strlen(num) > 3)
      yyerror("too many digits in hex character escape");
    return strtol(num + 1, 0, 16);
  } else {
    if (strlen(num) > 3)
      yyerror("too many digits in octal character escape");
    return strtol(num, 0, 8);
  }
}

%}

TOK     [a-zA-Z_][a-zA-Z0-9_]*|[+-]?[0-9]+
WS      [\t ]*
%x      SPECIAL REGEX REGCLASS

%%

<SPECIAL>{TOK}  {
                  long val;
                  char *errp;

                  errno = 0;

                  val = strtol(yytext, &errp, 10);

                  if (nesting == 0)
                    BEGIN(INITIAL);

                  if (*errp != 0) {
                    /* not a number */
                    yylval.lexeme = strdup(yytext);
                    return IDENT;
                  }

                  if ((val == LONG_MAX || val == LONG_MIN) && errno == ERANGE)
                    yyerror("numeric overflow in token");

                  yylval.num = val;
                  return NUMBER;
                }


<SPECIAL>\({WS}all{WS}\)        {
                                  BEGIN(INITIAL);
                                  return ALL;
                                }

<SPECIAL>\({WS}some{WS}\)       {
                                  BEGIN(INITIAL);
                                  return SOME;
                                }

<SPECIAL>\({WS}none{WS}\)       {
                                  BEGIN(INITIAL);
                                  return NONE;
                                }

<SPECIAL>\({WS}maybe{WS}\)      {
                                  BEGIN(INITIAL);
                                  return MAYBE;
                                }

<SPECIAL>\({WS}and{WS}\)        {
                                  BEGIN(INITIAL);
                                  return AND;
                                }

<SPECIAL>\({WS}or{WS}\)        {
                                  BEGIN(INITIAL);
                                  return OR;
                                }

<SPECIAL>\({WS}end{WS}\)        {
                                  BEGIN(INITIAL);
                                  return END;
                                }

<SPECIAL>\({WS}collect{WS}\)    {
                                  BEGIN(INITIAL);
                                  return COLLECT;
                                }

<SPECIAL>\({WS}coll{WS}\)       {
                                  BEGIN(INITIAL);
                                  return COLL;
                                }

<SPECIAL>\({WS}until{WS}\)      {
                                  BEGIN(INITIAL);
                                  return UNTIL;
                                }

<SPECIAL>\({WS}output{WS}\)     {
                                  BEGIN(INITIAL);
                                  return OUTPUT;
                                }

<SPECIAL>\({WS}repeat{WS}\)     {
                                  BEGIN(INITIAL);
                                  return REPEAT;
                                }


<SPECIAL>\({WS}rep{WS}\)        {
                                  BEGIN(INITIAL);
                                  return REP;
                                }

<SPECIAL>\({WS}single{WS}\)     {
                                  BEGIN(INITIAL);
                                  return SINGLE;
                                }

<SPECIAL>\({WS}first{WS}\)      {
                                  BEGIN(INITIAL);
                                  return FIRST;
                                }

<SPECIAL>\({WS}last{WS}\)       {
                                  BEGIN(INITIAL);
                                  return LAST;
                                }

<SPECIAL>\({WS}empty{WS}\)      {
                                  BEGIN(INITIAL);
                                  return EMPTY;
                                }

<SPECIAL>\{|\(  {
                  nesting++;
                  if (yytext[0] == '{')
                    closechar = '}';
                  else
                    closechar = ')';
                  return yytext[0];
                }

<SPECIAL>\}|\)  {
                  if (yytext[0] != closechar) {
                    yyerror("paren mismatch");
                    BEGIN(INITIAL);
                  } else {
                    if (--nesting == 0)
                      BEGIN(INITIAL);
                    return yytext[0];
                  }
                }

<SPECIAL>[\t ]+ {
                  /* Eat whitespace in directive */
                }

<SPECIAL>@      {
                  if (nesting == 0) {
                    BEGIN(INITIAL);
                    yylval.lexeme = strdup("@");
                    return TEXT;
                  } else {
                    yyerrorf(0, "bad character in directive: %c", yytext[0]);
                  }
                }

<SPECIAL>\n     {
                  lineno++;
                  yyerror("newline in directive");
                }

<SPECIAL>[/]    {
                  BEGIN(REGEX);
                  return '/';
                }

<SPECIAL>\.     {
                  yylval.chr = '.';
                  return '.';
                }

<SPECIAL>[\\][abtnvfre] {
                          char lexeme[2];
                          lexeme[0] = char_esc(yytext[1]);
                          lexeme[1] = 0;
                          yylval.lexeme = strdup(lexeme);
                          BEGIN(INITIAL);
                          return TEXT;
                        }

<SPECIAL>[\\](x[0-9a-fA-F]+|[0-7]+)     {
                                          char lexeme[2];
                                          lexeme[0] = num_esc(yytext + 1);
                                          lexeme[1] = 0;
                                          yylval.lexeme = strdup(lexeme);
                                          BEGIN(INITIAL);
                                          return TEXT;
                                        }

<SPECIAL>.      {
                  yyerrorf(0, "bad character in directive: '%c'", yytext[0]);
                }

<REGEX>[/]      {
                  if (nesting == 0)
                    BEGIN(INITIAL);
                  else
                    BEGIN(SPECIAL);
                  yylval.chr = '/';
                  return '/';
                }


<REGEX>[\\][abtnvfre]   {
                          yylval.chr = char_esc(yytext[1]);
                          return REGCHAR;
                        }

<REGEX>[\\](x[0-9a-fA-F]+|[0-9]+)       {
                                          yylval.chr = num_esc(yytext + 1);
                                          return REGCHAR;
                                        }

<REGEX>\n       {
                  lineno++;
                  yyerror("newline in regex");
                }

<REGEX>[.*?+^]  {
                  yylval.chr = yytext[0];
                  return yytext[0];
                }


<REGEX>[\[\]\-] {
                  yylval.chr = yytext[0];
                  return yytext[0];
                }

<REGEX>[()|]    {
                  yylval.chr = yytext[0];
                  return yytext[0];
                }

<REGEX>[\\].    {
                  yylval.chr = yytext[1];
                  return REGCHAR;
                }

<REGEX>.        {
                  yylval.chr = yytext[0];
                  return REGCHAR;
                }

<INITIAL>[^@\n]+        {
                          yylval.lexeme = strdup(yytext);
                          return TEXT;
                        }

<INITIAL>\n     {
                  lineno++;
                  return '\n';
                }

<INITIAL>@{WS}\*        {
                          BEGIN(SPECIAL);
                          return '*';
                        }

<INITIAL>@      {
                  BEGIN(SPECIAL);
                }

<INITIAL>^@#.*\n        {
                          /* eat whole line comment */
                          lineno++;
                        }

<INITIAL>@#.*   {
                   /* comment to end of line */
                }

%%

void help(void)
{
  const char *text =
"\n"
"txr version %s\n"
"\n"
"copyright 2009, Kaz Kylheku <kkylheku@gmail.com>\n"
"\n"
"usage:\n"
"\n"
"  %s [ options ] query-file { data-file }*\n"
"\n"
"The query-file or data-file arguments may be specified as -, in which case\n"
"standard input is used. If these arguments end with a | character, then\n"
"they are treated as command pipes. Leading arguments which begin with a -\n"
"followed by one or more characters, and which are not arguments to options\n"
"are interpreted as options. The -- option indicates the end of the options.\n"
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
"-b                     Don't dump list of bindings.\n"
"-a num                 Generate array variables up to num-dimensions.\n"
"                       Default is 1. Additional dimensions are fudged\n"
"                       by generating numeric suffixes\n"
"--help                 You already know!\n"
"--version              Display program version\n"
"\n"
"Options that take no argument can be combined. The -q and -v options\n"
"are mutually exclusive; the right-most one dominates.\n"
"\n"
  ;
  fprintf(stdout, text, version, progname);
}

void hint(void)
{
  fprintf(stderr, "%s: incorrect arguments: try --help\n", progname);
}

int main(int argc, char **argv)
{
  obj_t *stack_top = nil;
  obj_t *spec = nil;
  obj_t *bindings = nil;
  int match_loglevel = opt_loglevel;
  progname = argv[0] ? argv[0] : progname;

  gc_stack_top = &stack_top;

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

          list = cons(string(strdup(val)), list);

          if (!comma_p)
            break;

          val += piece + 1;
        }

        list = nreverse(list);
        bindings = cons(cons(intern(string(strdup(var))), list), bindings);
      } else if (equals) {
        char *val = equals + 1;
        *equals = 0;
        bindings = cons(cons(intern(string(strdup(var))),
                        string(strdup(val))), bindings);
      } else {
        bindings = cons(cons(intern(string(strdup(var))),
                        null_string), bindings);
      }

      argc--, argv++;
      continue;
    }

    if (!strcmp(*argv, "--version")) {
      printf("%s: version %s\n", progname, version);
      return 0;
    }

    if (!strcmp(*argv, "--help")) {
      help();
      return 0;
    }

    if (!strcmp(*argv, "-a")) {
      long val;
      char *errp;
      char opt = (*argv)[1];

      if (argc == 1) {
        fprintf(stderr, "%s: option %c needs argument\n", progname, opt);

        return EXIT_FAILURE;
      }

      argv++, argc--;

      switch (opt) {
      case 'a':
        val = strtol(*argv, &errp, 10);
        if (*errp != 0) {
          fprintf(stderr, "%s: option %c needs numeric argument, not %s\n",
                  progname, opt, *argv);
          return EXIT_FAILURE;
        }

        opt_arraydims = val;
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
        default:
          fprintf(stderr, "%s: unrecognized option: %c\n", progname, *popt);
          return EXIT_FAILURE;
        }
      }

      argc--, argv++;
    }
  }

  if (argc < 1) {
    hint();
    return EXIT_FAILURE;
  }

  if (strcmp(*argv, "-") != 0) {
    yyin = fopen(*argv, "r");
    if (yyin == 0) {
      fprintf(stderr, "%s: unable to open %s\n", progname, *argv);
      return EXIT_FAILURE;
    }
    spec_file = *argv;
  }

  argc--, argv++;

  {
    int gc;
    init(progname, oom_realloc_handler);

    gc = gc_state(0);
    yyparse();
    gc_state(gc);

    if (errors)
      return EXIT_FAILURE;
    spec = get_spec();


    opt_loglevel = match_loglevel;

    if (opt_loglevel >= 2) {
      fputs("spec:\n", stderr);
      dump(spec, stderr);

      fputs("bindings:\n", stderr);
      dump(bindings, stderr);
    }

    {
      int retval;
      list_collect_decl(filenames, iter);

      while (*argv)
        list_collect(iter, string(*argv++));

      retval = extract(spec, filenames, bindings);

      return errors ? EXIT_FAILURE : retval;
    }
  }
}
