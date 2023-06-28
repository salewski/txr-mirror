/* Copyright 2009-2023
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
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
#if __APPLE__
#include <mach-o/dyld.h>
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
#include "itypes.h"
#if HAVE_GLOB
#include "glob.h"
#endif
#include "txr.h"

#if CONFIG_FULL_REPL
#define if_full_repl(THEN, ELSE) (THEN)
#else
#define if_full_repl(THEN, ELSE) (ELSE)
#endif

const wchli_t *version = wli(TXR_VER);
#ifdef TXR_BUILD_ID
const wchli_t *build_id = wli(TXR_BUILD_ID);
#endif
wchar_t *progname;
static const char *progname_u8;
static val prog_path = nil, sysroot_path = nil;
int opt_noninteractive = if_full_repl(0, 1);
int opt_noprofile;
int opt_compat;
int opt_dbg_expansion;
int opt_free_all;
val stdlib_path;
val self_path_s;

#if HAVE_FORK_STUFF
#define IF_HAVE_FORK_STUFF(THEN, ELSE) THEN
#else
#define IF_HAVE_FORK_STUFF(THEN, ELSE) ELSE
#endif

static void help(void)
{
  val text = lit(
"\n"
"TXR Version ~a\n"
"Copyright 2009-2023 Kaz Kylheku <kaz@kylheku.com>\n"
"\n"
"Usage:\n"
"\n"
"  ~a [ options ] script-file { argument }*\n"
"\n"
"If no arguments are present, TXR will enter into interactive listener mode.\n"
"\n"
"The script-file or data-file arguments may be specified as -, in which case\n"
"standard input is used. All data-file arguments which begin with a !\n"
"character are treated as command pipes. Those which begin with a $\n"
"are interpreted as directories to read. Leading arguments which begin\n"
"with a - followed by one or more characters, and which are not arguments to\n"
"options are interpreted as options. The -- option indicates the end of the\n"
"options.\n"
"\n"
"If no data-file arguments are supplied, then the query itself must open a\n"
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
"-B                     Force list of bindings to be dumped, or false\n"
"                       if termination is unsuccessful.\n"
"-l                     If dumping bindings, use TXR Lisp format.\n"
"-i                     Interactive TXR Lisp listener mode.\n"
"                       (Requires compiled-in support.)\n"
"-d                     Enable debugger. Implies --backtrace.\n"
"-n                     Noninteractive input mode for standard input stream,\n"
"                       even if its connected to a terminal device.\n"
"                       Also, forces the interactive listener into\n"
"                       'plain mode' without editing features.\n"
"-a N                   Generate array variables up to N dimensions.\n"
"                       N is a decimal integer. The default value is 1.\n"
"                       Additional dimensions beyond N are fudged\n"
"                       by generating numeric suffixes. Implies -B.\n"
"-c query-text          The query is read from the query-text argument\n"
"                       itself. The script-file argument is omitted in\n"
"                       this case; the first argument is a data file.\n"
"-f script-file         Specify the script-file as an option argument\n"
"                       instead of as the script-file argument.\n"
"                       This allows #! scripts to pass options through\n"
"                       to the txr utility.\n"
"-e expressions         Evaluate zero or more TXR Lisp expressions.\n"
"                       Can be specified multiple times. The script-file\n"
"                       arg becomes optional.\n"
"-p expression          Evaluate a single expression, and print the value\n"
"                       using the prinl function.\n"
"-P expression          Like -p, but prints using pprinl.\n"
"-t expression          Like -p, but prints using tprint.\n"
"-b var=value           Bind a Lisp global variable as if by defparml.\n"
"                       var and value are parsed as Lisp syntax.\n"
"                       value is not evaluated.\n"
"-C N                   Request backward-compatible behavior to the\n"
"                       specified version of TXR.\n"
"--help                 Reproduce this help text.\n"
"--version              Display program version.\n"
"--build-id             Print build ID string if compiled in.\n"
"--license              Display software license.\n"
"                       Use of txr implies agreement with the disclaimer\n"
"                       section at the bottom of the license.\n"
"--lisp                 Treat unsuffixed query files as TXR Lisp.\n"
"--compiled             Treat unsuffixed query files as compiled TXR Lisp.\n"
"--lisp-bindings        Synonym for -l.\n"
"--debugger             Synonym for -d.\n"
"--backtrace            Enable backtraces.\n"
"--noninteractive       Synonym for -n.\n"
"--compat=N             Synonym for -C N.\n"
"--in-package=name      Switch to specified package\n"
"--compile=src[:target] Compile a file.\n"
"--gc-delta=N           Invoke garbage collection when malloc activity\n"
"                       increments by N megabytes since last collection.\n"
"--args...              Allows multiple arguments to be encoded as a single\n"
"                       argument. This is useful in hash-bang scripts.\n"
"                       Peculiar syntax. See the manual.\n"
"--eargs... arg         Extended version of --args: additionally consumes\n"
"                       the following argument arg and allows one or more\n"
"                       copies of it to be to be embedded in the\n"
"                       encoded arguments. See the manual.\n"
"--noprofile            Do not read .txr_profile when entering listener.\n"
IF_HAVE_FORK_STUFF(
"--reexec               Re-execute TXR with remaining arguments.\n",
""
)
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

static void banner(val self)
{
  if (!isatty(c_int(stream_fd(std_input), self)))
    return;

  format(std_output,
         if3(opt_noninteractive,
             lit("This is the TXR Lisp plain mode listener of TXR ~a.\n"
                 "Quit with :quit or Ctrl-D on an empty line.\n"),
             if_full_repl(lit("This is the TXR Lisp interactive "
                              "listener of TXR ~a.\n"
                              "Quit with :quit or Ctrl-D on an empty line. "
                              "Ctrl-X ? for cheatsheet.\n"), nil)),
         static_str(version), nao);
}

static val check_hash_bang(val stream, val args, int *occurs)
{
  val line = get_line(stream);

  if (line) {
    if (match_str(line, lit("#!"), nil)) {
      val pos = search_str(line, lit("\xdc00"), nil, nil);
      *occurs = 1;

      if (pos) {
        val after_null = sub_str(line, succ(pos), t);
        val prepend_args = split_str(after_null, lit(" "));
        args = nappend2(prepend_args, args);
      }
    } else {
      seek_stream(stream, zero, from_start_k);
    }
  }

  return args;
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
#elif __APPLE__
static val get_self_path(void)
{
  char self[PATH_MAX] = { 0 };
  uint32_t size = sizeof self;

  if (_NSGetExecutablePath(self, &size) != 0)
    return nil;
  return string_utf8(self);
}
#elif HAVE_GETEXECNAME
static val get_self_path(void)
{
  val execname = string_utf8(getexecname());
  if (car(execname) == chr('/'))
    return execname;
  return scat3(getcwd_wrap(), chr('/'), execname);
}
#else
static val get_self_path(void)
{
  char self[PATH_MAX];

  if (progname_u8 && realpath(progname_u8, self))
    return string_utf8(self);

  return lit(TXR_INST_PATH);
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
             scat2(sub_str(edited_path, 0, succ(lslash)), basename),
             basename);
}

static val sysroot(val target)
{
  return scat2(sysroot_path, target);
}

static void sysroot_init(void)
{
  val prog_dir;
  const wchar_t *psc = coerce(const wchar_t *, path_sep_chars);
  int share_txr_stdlib = 1;

  protect(&prog_path, &sysroot_path, &stdlib_path, convert(val *, 0));
  prog_path = get_self_path();
#if HAVE_WINDOWS_H
  prog_path = regsub(lit("\\"), lit("/"), prog_path);
#endif
  prog_dir = dir_name(prog_path);

  if (ref(prog_dir, negone) != chr(psc[0]))
    prog_dir = scat3(prog_dir, chr(psc[0]), null_string);

  if (!(maybe_sysroot(lit(TXR_REL_PATH)) ||
        maybe_sysroot(substitute_basename(lit(TXR_REL_PATH), prog_path)) ||
        (share_txr_stdlib = 0) ||
        maybe_sysroot(lit(PROG_NAME EXE_SUFF))))
  {
    sysroot_path = prog_dir;
  }

  stdlib_path = sysroot(if3(share_txr_stdlib,
                            lit("share/txr/stdlib/"),
                            lit("stdlib/")));

  {
    loc lsd = lookup_var_l(nil, load_search_dirs_s);
    set(lsd, cons(sysroot(if3(share_txr_stdlib,
                            lit("share/txr/lib/"),
                            lit("lib/"))),
                  nil));
  }

  reg_varl(intern(lit("stdlib"), user_package), stdlib_path);
  reg_varl(intern(lit("*txr-version*"), user_package),
           toint(lit(TXR_VER), nil));
  reg_varl(intern(lit("txr-version"), user_package),
           toint(lit(TXR_VER), nil));
  reg_varl(intern(lit("txr-path"), user_package), prog_dir);
  reg_varl(intern(lit("txr-exe-path"), user_package), prog_path);
}

static void sysroot_compat_fixup(int compat_ver)
{
  if (compat_ver <= 143)
    reg_varl(intern(lit("stdlib"), user_package),
             sysroot(lit("share/txr/stdlib")));
}

static int license(void)
{
  int retval = EXIT_SUCCESS;

  uw_catch_begin(cons(error_s, nil), esym, eargs);

  {
    val iter;
#if HAVE_GLOB
    val glob_pattern = sysroot(lit("share/txr/LICENSE*"));
    val path_list = glob_wrap(glob_pattern, zero, nil);
#else
    val glob_pattern = sysroot(lit("share/txr/LICENSE"));
    val path_list = cons(glob_pattern, nil);
#endif

    if (!path_list)
      uw_throwf(error_s, lit("pattern ~a didn't match any files."),
                glob_pattern, nao);

    put_char(chr('\n'), std_output);

    for (iter = path_list; iter; iter = cdr(iter)) {
      val lic = open_file(car(iter), lit("r"));

      put_lines(lazy_stream_cons(lic, nil), std_output);
      put_char(chr('\n'), std_output);
    }
  }

  uw_catch (esym, eargs) {
    format(std_output,
           lit("~a: unable to display license file: ~a\n"),
           prog_string, car(eargs), nao);
    retval = EXIT_FAILURE;
  }

  uw_unwind { }

  uw_catch_end;

  return retval;
}

static void free_all(void)
{
  if (opt_free_all) {
    regex_free_all();
    gc_free_all();
    arith_free_all();
    free(progname);
  }
}

int txr_main(int argc, char **argv);

int main(int argc, char **argv)
{
  val stack_bottom = nil;
  repress_privilege();
  progname = utf8_dup_from(argv[0] ? argv[0]: "txr");
  progname_u8 = argv[0];
  atexit(free_all);
  init(&stack_bottom);
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

static void do_compile_opt(val arg)
{
  val compile_update_file = intern(lit("compile-update-file"), user_package);
  val col_pos = search_str(arg, lit(":"), nil, nil);
  val source = arg;
  val target = nil;

  if (col_pos) {
    target = sub_str(source, succ(col_pos), t);
    source = sub_str(source, zero, col_pos);
  }

  reg_varl(self_path_s, source);

  funcall2(compile_update_file, source, target);
}

static int do_in_package_opt(val opt, val arg)
{
  val pkg_binding = lookup_var(nil, package_s);
  val package = find_package(arg);

  if (!package) {
    format(std_error, lit("~a: option --~a: ~a package not found\n"),
           prog_string, opt, arg, nao);
    return 0;
  }

  rplacd(pkg_binding, package);
  return 1;
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
  int compat = c_num(optval, lit("txr"));
  int min = compat_fixup(compat);

  if (min) {
    format(std_error, lit("~a: compatibility with versions "
                          "lower than ~a not supported by version ~a\n"),
           prog_string, num(min), static_str(version), nao);
    return 0;
  }

  sysroot_compat_fixup(compat);
  match_compat_fixup(compat);

  opt_compat = compat;
  reg_varl(intern(lit("compat"), system_package), num(compat));
  return 1;
}

static int array_dim(val optval)
{
  opt_arraydims = c_num(optval, lit("txr"));
  opt_print_bindings = 1;
  return 1;
}

static int gc_delta(val optval)
{
  opt_gc_delta = c_num(mul(optval, num_fast(1048576)), lit("gc"));
  return 1;
}

#ifndef CONFIG_DEBUG_SUPPORT
static void no_dbg_support(val arg)
{
  drop_privilege();
  format(std_error,
         lit("~a: option ~a requires debug support compiled in\n"),
         prog_string, arg, nao);
}
#endif

static int parse_once_noerr(val stream, val name)
{
  val pfx = scat2(name, lit(":"));
  ignerr_func_body(int, 0, parse_once(prog_string, stream, name),
                   std_error, pfx);
}

static val read_compiled_file_noerr(val self, val stream, val name, val error_stream)
{
  val pfx = scat2(name, lit(":"));
  ignerr_func_body(val, nil, read_compiled_file(self, stream, error_stream),
                   std_error, pfx);
}

static val read_eval_stream_noerr(val self, val stream, val name, val error_stream)
{
  val pfx = scat2(name, lit(":"));
  ignerr_func_body(val, nil, read_eval_stream(self, stream, error_stream),
                   std_error, pfx);
}

int txr_main(int argc, char **argv)
{
  val self = lit("txr startup");
  uses_or2;
  val specstring = nil;
  val spec = nil;
  val spec_file = nil;
  val bindings = nil;
  val evaled = nil;
  val spec_file_str;
  int match_loglevel = opt_loglevel;
  val arg_undo = nil, arg;
  val parse_stream = nil;
  val txr_lisp_p = nil;
  val enter_repl = nil;
  val args_s = intern(lit("*args*"), user_package);
  val compat_var = lit("TXR_COMPAT");
  val compat_val = getenv_wrap(compat_var);
  val orig_args = nil, ref_arg_list = nil;
  int hb_occurs = 0;
  list_collect_decl(arg_list, arg_tail);
  list_collect_decl(eff_arg_list, eff_arg_tail);

  static char alt_args_buf[128 + 7] = "@(txr):", *alt_args = alt_args_buf + 7;

  self_path_s = intern(lit("self-path"), user_package);

  if (ends_with(lit("lisp" EXE_SUFF), prog_path, nil, nil))
    txr_lisp_p = t;
  else if (ends_with(lit("vm" EXE_SUFF), prog_path, nil, nil))
    txr_lisp_p = chr('o');

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

  while (*argv)
    arg_tail = list_collect(arg_tail, string_utf8(*argv++));

  reg_varl(intern(lit("compat"), system_package), zero);
  reg_var(intern(lit("*full-args*"), user_package), arg_list);
  reg_var(intern(lit("*args-full*"), user_package), arg_list);
  reg_var(intern(lit("*args-eff*"), user_package), eff_arg_list);

  if (arg_list) {
    eff_arg_tail = list_collect(eff_arg_tail, car(arg_list));
    arg_list = cdr(arg_list);
  }

  if (*alt_args) {
    orig_args = arg_list;
    arg_list = list(string_utf8(alt_args), nao);
  } else if (argc <= 1) {
    drop_privilege();
    banner(self);
    goto repl;
  }

  for (ref_arg_list = arg_list, arg = upop(&arg_list, &arg_undo);
       arg;
       arg = upop(&arg_list, &arg_undo))
  {
    eff_arg_tail = list_collect(eff_arg_tail, arg);

    if (equal(arg, lit("--")))
      break;

    if (car(arg) != chr('-')) {
      if (!parse_stream) {
        spec_file_str = arg;
        open_txr_file(arg, &txr_lisp_p, &spec_file_str, &parse_stream, t, self);
        simulate_setuid_setgid(parse_stream);
        dyn_env = make_env(nil, nil, dyn_env);
        env_vbind(dyn_env, load_path_s, spec_file_str);
        arg_list = check_hash_bang(parse_stream, arg_undo, &hb_occurs);
        set(eff_arg_tail, butlastn(one, deref(eff_arg_tail)));
        continue;
      }
      break;
    }

    if (equal(arg, lit("-")))
      break;

    /* Odd case 1: --args is followed by an arbitrary delimiting
     * character, not necessarily = */
    if (match_str(arg, lit("--args"), zero)) {
      val sep = sub_str(arg, num(6), num(7));
      if (empty(sep)) {
        format(std_error,
               lit("~a: --args requires argument material\n"),
               prog_string, nao);
        return EXIT_FAILURE;
      }
      arg = sub_str(arg, num(7), nil);
      arg_list = append2(split_str(arg, sep), arg_list);
      set(eff_arg_tail, butlastn(one, deref(eff_arg_tail)));
      continue;
    }

    /* Odd case 2: --eargs is followed by an arbitrary delimiting
     * character, not necessarily = */
    if (match_str(arg, lit("--eargs"), zero)) {
      val sep = sub_str(arg, num(7), num(8));
      val arg2;
      if (empty(sep)) {
        format(std_error,
               lit("~a: --eargs requires argument material\n"),
               prog_string, nao);
        return EXIT_FAILURE;
      }
      if (!arg_list) {
        format(std_error,
               lit("~a: --eargs must be followed by an argument\n"),
               prog_string, nao);
        return EXIT_FAILURE;
      }
      arg = sub_str(arg, num(8), nil);
      arg2 = upop(&arg_list, &arg_undo);
      arg_list = append2(mapcar(iffi(pa_12_2(equal_f, lit("{}")),
                                     retf(arg2), nil),
                                split_str(arg, sep)),
                         arg_list);
      set(eff_arg_tail, butlastn(one, deref(eff_arg_tail)));
      continue;
    }

    /* Odd case 3: -Dfoo=bar syntax. */
    if (equal(sub(arg, zero, two), lit("-D"))) {
      val dopt_arg = sub(arg, two, t);
      val eq_pos = search_str(dopt_arg, lit("="), nil, nil);

      if (eq_pos) {
        val var = sub_str(dopt_arg, zero, eq_pos);
        val def = sub_str(dopt_arg, succ(eq_pos), t);
        val deflist = split_str(def, lit(","));
        val sym = intern(var, cur_package);

        if (rest(deflist))
          bindings = cons(cons(sym, deflist), bindings);
        else
          bindings = cons(cons(sym, car(deflist)), bindings);

        match_reg_var(sym);
      } else {
        if (search_str(dopt_arg, lit(","), nil, nil)) {
          format(std_error,
                 lit("~a: bad -D syntax: ~a\n"), prog_string, arg, nao);
          return EXIT_FAILURE;
        } else {
          val sym = intern(dopt_arg, cur_package);
          bindings = cons(cons(sym, null_string), bindings);
          match_reg_var(sym);
        }
      }

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

      if (equal(opt, lit("compile"))) {
        if (!org) {
          requires_arg(opt);
          return EXIT_FAILURE;
        }
        reg_var(args_s, or2(orig_args, arg_list));
        do_compile_opt(org);
        evaled = t;
        continue;
      }

      if (equal(opt, lit("in-package"))) {
        if (!org) {
          requires_arg(opt);
          return EXIT_FAILURE;
        }
        if (!do_in_package_opt(opt, org))
          return EXIT_FAILURE;
        continue;
      }

      /* Long opts with no arguments */
      if (0) noarg: {
        drop_privilege();
        format(std_error,
               lit("~a: option --~a takes no argument, ~a given\n"),
               prog_string, opt, org, nao);
        return EXIT_FAILURE;
      }

      if (equal(opt, lit("version"))) {
        if (org)
          goto noarg;
        drop_privilege();
        format(std_output, lit("~a: version ~a\n"),
               prog_string, static_str(version), nao);
        return 0;
      }

      if (equal(opt, lit("build-id"))) {
        if (org)
          goto noarg;
        drop_privilege();
#ifdef TXR_BUILD_ID
        format(std_output, lit("~a\n"), static_str(build_id), nao);
#endif
        return 0;
      }

      if (equal(opt, lit("help"))) {
        drop_privilege();
        help();
        return 0;
      }

      if (equal(opt, lit("license"))) {
        if (org)
          goto noarg;
        drop_privilege();
        return license();
      }

      if (equal(opt, lit("gc-debug"))) {
        if (org)
          goto noarg;
        drop_privilege();
        opt_gc_debug = 1;
        continue;
      } else if (equal(opt, lit("vg-debug"))) {
#if HAVE_VALGRIND
        if (org)
          goto noarg;
        drop_privilege();
        opt_vg_debug = 1;
        continue;
#else
        drop_privilege();
        format(std_error,
               lit("~a: option ~a requires Valgrind support compiled in\n"),
               prog_string, arg, nao);
        return EXIT_FAILURE;
#endif
      } else if (equal(opt, lit("dv-regex"))) {
        if (org)
          goto noarg;
        opt_derivative_regex = 1;
        continue;
      } else if (equal(opt, lit("lisp-bindings"))) {
        if (org)
          goto noarg;
        opt_lisp_bindings = 1;
        opt_print_bindings = 1;
        continue;
      } else if (equal(opt, lit("lisp"))) {
        if (org)
          goto noarg;
        txr_lisp_p = t;
        continue;
      } else if (equal(opt, lit("compiled"))) {
        if (org)
          goto noarg;
        txr_lisp_p = chr('o');
        continue;
#if HAVE_FORK_STUFF
      } else if (equal(opt, lit("reexec"))) {
        if (org)
          goto noarg;
        exec_wrap(prog_path, arg_list);
        return EXIT_FAILURE;
#endif
      } else if (equal(opt, lit("debugger"))) {
#if CONFIG_DEBUG_SUPPORT
        if (org)
          goto noarg;
        drop_privilege();
        opt_debugger = 1;
        debug_set(DBG_ENABLE | DBG_BACKTRACE);
        continue;
#else
        no_dbg_support(arg);
        return EXIT_FAILURE;
#endif
      } else if (equal(opt, lit("debug-autoload"))) {
#if CONFIG_DEBUG_SUPPORT
        if (org)
          goto noarg;
        drop_privilege();
        opt_debugger = 1;
        opt_dbg_autoload = 1;
        debug_set(DBG_ENABLE | DBG_BACKTRACE);
        continue;
#else
        no_dbg_support(opt);
        return EXIT_FAILURE;
#endif
      } else if (equal(opt, lit("debug-expansion"))) {
#if CONFIG_DEBUG_SUPPORT
        if (org)
          goto noarg;
        drop_privilege();
        opt_debugger = 1;
        opt_dbg_expansion = 1;
        debug_set(DBG_ENABLE | DBG_BACKTRACE);
        continue;
#else
        no_dbg_support(opt);
        return EXIT_FAILURE;
#endif
      } else if (equal(opt, lit("yydebug"))) {
        if (org)
          goto noarg;
        drop_privilege();
        if (have_yydebug) {
          yydebug_onoff(1);
          format(std_error,
                 lit("~a: option --~a takes no argument, ~a given\n"),
                 prog_string, opt, org, nao);
          continue;
        } else {
          drop_privilege();
          format(std_error,
                 lit("~a: option ~a requires YYDEBUG support compiled in\n"),
                 prog_string, arg, nao);
          return EXIT_FAILURE;
        }
      } else if (equal(opt, lit("backtrace"))) {
        if (org)
          goto noarg;
#if CONFIG_DEBUG_SUPPORT
        debug_set(DBG_BACKTRACE);
        continue;
#else
        no_dbg_support(arg);
        return EXIT_FAILURE;
#endif
      } else if (equal(opt, lit("noninteractive"))) {
        if (org)
          goto noarg;
        opt_noninteractive = 1;
        stream_set_prop(std_input, real_time_k, nil);
        continue;
      } else if (equal(opt, lit("free-all"))) {
        if (org)
          goto noarg;
        opt_free_all = 1;
        continue;
      } else if (equal(opt, lit("noprofile"))) {
        if (org)
          goto noarg;
        opt_noprofile = 1;
        continue;
      } else {
        drop_privilege();
        format(std_error, lit("~a: unrecognized long option: --~a\n"),
               prog_string, opt, nao);
        return EXIT_FAILURE;
      }
    }

    /* Single letter options with args: non-clumping. */
    if (length(arg) == two && find(ref(arg, one), lit("abcfepPtC"), nil, nil))
    {
      val opt = chr_str(arg, one);

      if (!arg_list) {
        drop_privilege();
        requires_arg(opt);
        return EXIT_FAILURE;
      }

      arg = upop(&arg_list, &arg_undo);

      eff_arg_tail = list_collect(eff_arg_tail, arg);

      switch (c_chr(opt)) {
      case 'a':
        if (!do_fixnum_opt(array_dim, opt, arg))
          return EXIT_FAILURE;
        break;
      case 'C':
        if (!do_fixnum_opt(compat, opt, arg))
          return EXIT_FAILURE;
        break;
      case 'b':
        drop_privilege();
        {
          val pair = split_str(arg, chr('='));
          if (cdr(pair)) {
            val sym = lisp_parse(pop(&pair), std_error,
                                 colon_k, lit("cmdline-expr"), colon_k);
            val obj = lisp_parse(pop(&pair), std_error,
                                 colon_k, lit("cmdline-expr"), colon_k);

            if (!bindable(sym)) {
              format(std_error,
                     lit("~a: ~s isn't a bindable symbol\n"),
                     prog_string, sym, nao);
              return EXIT_FAILURE;
            }

            reg_var(sym, obj);
          } else {
            format(std_error,
                   lit("~a: -b argument must be var=val syntax\n"),
                   prog_string, nao);
            return EXIT_FAILURE;
          }
        }
        break;
      case 'c':
        if (txr_lisp_p) {
          format(std_error,
                 lit("~a: -c not compatible with --lisp or --compiled; use -e\n"),
                 prog_string, nao);
          return EXIT_FAILURE;
        }
        if (parse_stream) {
          format(std_error,
                 lit("~a: -c ~a: input source ~a has already been established\n"),
                 prog_string, arg, spec_file_str, nao);
          return EXIT_FAILURE;
        }
        specstring = arg;
        drop_privilege();
        spec_file_str = lit("cmdline");
        if (gt(length_str(specstring), zero) &&
            chr_str(specstring, minus(length_str(specstring), one)) != chr('\n'))
          specstring = cat_str(list(specstring, string(L"\n"), nao), nil);
        parse_stream = make_string_byte_input_stream(specstring);
        break;
      case 'f':
        spec_file = arg;
        if (parse_stream) {
          if (equal(spec_file, spec_file_str))
            break;
          format(std_error,
                 lit("~a: -f ~a: input source ~a has already been established\n"),
                 prog_string, arg, spec_file_str, nao);
          return EXIT_FAILURE;
        }
        if (wcscmp(c_str(spec_file, self), L"-") != 0) {
          spec_file_str = spec_file;
          open_txr_file(spec_file, &txr_lisp_p, &spec_file_str,
                        &parse_stream, t, self);
          simulate_setuid_setgid(parse_stream);
          dyn_env = make_env(nil, nil, dyn_env);
          env_vbind(dyn_env, load_path_s, spec_file_str);
          arg_list = check_hash_bang(parse_stream, arg_list, &hb_occurs);
        } else {
          drop_privilege();
          spec_file_str = lit("stdin");
          parse_stream = std_input;
        }
        break;
      case 'e':
        drop_privilege();
        {
          val args_saved = or2(orig_args, arg_list);
          val args_new;

          reg_varl(self_path_s, lit("cmdline-expr"));
          reg_var(args_s, or2(orig_args, arg_list));

          {
            val forms = read_objects_from_string(arg, std_error, colon_k,
                                                 lit("cmdline-expr"));

            if (forms != colon_k)
              eval_intrinsic(cons(progn_s, forms),
                             make_env(bindings, nil, nil), nil);
          }

          evaled = t;

          args_new = cdr(lookup_global_var(args_s));

          if (args_new != args_saved) {
            arg_list = args_new;
            orig_args = nil;
          }
        }
        break;
      case 'p':
      case 'P':
      case 't':
        drop_privilege();
        {
          val (*pf)(val obj, val out) = if3(c_chr(opt) == 'p',
                                            prinl,
                                            if3(c_chr(opt) == 'P',
                                                pprinl,
                                                tprint));
          val args_saved = or2(orig_args, arg_list);
          val args_new;
          val obj;

          reg_varl(self_path_s, lit("cmdline-expr"));
          reg_var(args_s, or2(orig_args, arg_list));

          obj = eval_intrinsic(lisp_parse(arg, std_error, colon_k,
                                          lit("cmdline-expr"), colon_k),
                               make_env(bindings, nil, nil), nil);
          gc_hint(obj);
          pf(z(obj), std_output);

          evaled = t;

          args_new = cdr(lookup_global_var(args_s));
          if (args_new != args_saved) {
            arg_list = args_new;
            orig_args = nil;
          }
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
        case 'B':
          opt_print_bindings = 1;
          break;
        case 'l':
          opt_lisp_bindings = 1;
          opt_print_bindings = 1;
          break;
        case 'i':
          drop_privilege();
          enter_repl = t;
          break;
        case 'd':
          drop_privilege();
#if CONFIG_DEBUG_SUPPORT
          opt_debugger = 1;
          debug_set(DBG_ENABLE | DBG_BACKTRACE);
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
        case 'b':
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
          format(std_error, lit("~a: dash in the middle of options: ~a\n"),
                 prog_string, arg, nao);
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

  eff_arg_tail = list_collect_nconc(eff_arg_tail, arg_list);

  if (!equal(cdr(eff_arg_list), ref_arg_list))
    reg_var(intern(lit("*args-eff*"), user_package), eff_arg_list);

  if (!parse_stream) {
    if (!arg) {
      drop_privilege();
      if (enter_repl)
        goto repl;
      if (evaled)
        return EXIT_SUCCESS;
      banner(self);
      goto repl;
    }

    drop_privilege();
    spec_file_str = lit("stdin");
    parse_stream = std_input;
  } else if (specstring || spec_file) {
    arg_list = arg_undo;
  }

  reg_var(args_s, or2(orig_args, arg_list));
  reg_varl(self_path_s, spec_file_str);

  env_vbind(dyn_env, load_recursive_s, t);

  if (hb_occurs)
    parser_set_lineno(prog_string, parse_stream, two);

  if (!txr_lisp_p)
  {
    int gc = gc_state(0);
    val parser_obj = ensure_parser(parse_stream, spec_file_str);
    parser_t *parser = parser_get_impl(prog_string, parser_obj);
    parse_once_noerr(parse_stream, spec_file_str);
    mut(parser_obj);
    gc_state(gc);

    close_stream(parse_stream, nil);
    run_load_hooks(dyn_env);
    uw_release_deferred_warnings();

    spec = parser->syntax_tree;

    opt_loglevel = match_loglevel;

    if (opt_compat && opt_compat <= 199)
      reg_var(intern(lit("*self-path*"), user_package), spec_file_str);

    if (parser->errors) {
      if (enter_repl)
        goto repl;
      return EXIT_FAILURE;
    }

    if (opt_loglevel >= 2) {
      format(std_error, lit("spec:\n~s\n"), spec, nao);
      format(std_error, lit("bindings:\n~s\n"), bindings, nao);
    }

    {
      val result = extract(spec, arg_list, bindings);
      cons_bind (new_bindings, success, result);

      if (enter_repl) {
        bindings = new_bindings;
        goto repl;
      }

      return success ? 0 : EXIT_FAILURE;
    }
  }

  for (; bindings; bindings = cdr(bindings)) {
    val binding = car(bindings);
    reg_varl(car(binding), cdr(binding));
  }

  {
    if (txr_lisp_p == chr('o')) {
      val result = nil;
      uw_block_begin (load_s, ret);
      result = read_compiled_file_noerr(self, parse_stream, spec_file_str,
                                        std_error);
      uw_block_end;
      if (!enter_repl)
        return result ? 0 : EXIT_FAILURE;
    } else if (enter_repl) {
      uw_block_begin (load_s, ret);
      read_eval_stream_noerr(self, parse_stream, spec_file_str, std_error);
      uw_block_end;
      close_stream(parse_stream, nil);
      run_load_hooks(dyn_env);
      uw_release_deferred_warnings();
    } else {
      val result = nil;
      uw_block_begin (load_s, ret);
      result = read_eval_stream(self, parse_stream, std_error);
      uw_block_end;
      return result ? 0 : EXIT_FAILURE;
    }
  }

repl:
  if (compat_val)
    format(std_output,
           lit("Note: operating in TXR ~a compatibility mode "
               "due to environment variable.\n"),
           num(opt_compat), nao);
  reg_var(args_s, or2(orig_args, arg_list));
  reg_varl(self_path_s, lit("listener"));
  env_vbind(dyn_env, package_s,
            opt_compat && opt_compat <= 190 ? user_package : public_package);
  env_vbind(dyn_env, load_recursive_s, nil);
  repl(bindings, std_input, std_output, nil);
  return 0;
}
