/* linenoise.c -- VERSION 1.0
 *
 * Guerrilla line editing library against the idea that a line editing lib
 * needs to be 20,000 lines of C code.
 *
 * You can find the latest source code at:
 *
 *   http://github.com/antirez/linenoise
 *
 * Does a number of crazy assumptions that happen to be true in 99.9999% of
 * the 2010 UNIX computers around.
 *
 * ------------------------------------------------------------------------
 *
 * Copyright (c) 2010-2015, Salvatore Sanfilippo <antirez at gmail dot com>
 * Copyright (c) 2010-2013, Pieter Noordhuis <pcnoordhuis at gmail dot com>
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *  *  Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *  *  Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <termios.h>
#include <unistd.h>
#include <stddef.h>
#include <wchar.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <wctype.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <limits.h>
#include <assert.h>
#include <stdarg.h>
#include "config.h"
#if HAVE_POLL
#include <poll.h>
#endif
#ifdef __CYGWIN__
#include <sys/utsname.h>
#endif
#include "linenoise.h"

#ifdef __cplusplus
#define strip_qual(TYPE, EXPR) (const_cast<TYPE>(EXPR))
#define convert(TYPE, EXPR) (static_cast<TYPE>(EXPR))
#define coerce(TYPE, EXPR) (reinterpret_cast<TYPE>(EXPR))
#else
#define convert(TYPE, EXPR) ((TYPE) (EXPR))
#define coerce(TYPE, EXPR) ((TYPE) (EXPR))
#endif

#define LINENOISE_DEFAULT_HISTORY_MAX_LEN 100
#define LINENOISE_MAX_LINE 1024
#define LINENOISE_MAX_DISP (LINENOISE_MAX_LINE * 2)
#define LINENOISE_PAREN_DELAY 400000
#define LINENOISE_FLASH_DELAY 200000
#define LINENOISE_MAX_UNDO 200

/* The lino_state structure represents the state during line editing.
 * We pass this state to functions implementing specific editing
 * functionalities. */
struct lino_state {
    lino_t *next, *prev;        /* Links for global list: must be first */

    /* Lifetime enduring state */
    lino_compl_cb_t *completion_callback;
    void *cb_ctx;               /* User context for completion callback */
    lino_atom_cb_t *atom_callback;
    void *ca_ctx;               /* User context for atom callback */
    lino_enter_cb_t *enter_callback;
    void *ce_ctx;               /* User context for enter callback */
    struct termios orig_termios;        /* In order to restore at exit.*/
    int rawmode;        /* For atexit() function to check if restore is needed*/
    int mlmode;         /* Multi line mode. Default is single line. */
    int history_max_len;
    int history_len;
    wchar_t **history;
    wchar_t *clip;      /* Selection */
    wchar_t *result;    /* Previous command result. */
    mem_t *tty_ifs;     /* Terminal input file stream. */
    mem_t *tty_ofs;     /* Terminal output file stream. */
    int save_hist_idx;  /* Jump to history position on entry into edit */

    /* Volatile state pertaining to just one linenoise call */
    wchar_t buf[LINENOISE_MAX_DISP];    /* Displayed line bufer. */
    wchar_t data[LINENOISE_MAX_LINE];   /* True data corresponding to display */
    const wchar_t *prompt;      /* Prompt to display. */
    const char *suffix;         /* Suffix when creating temp file. */
    int plen;           /* Prompt length. */
    int pos;            /* Current cursor position. */
    int sel;            /* Selection start in terms of display. */
    int end;            /* Selection end in terms of display. */
    int len;            /* Current edited line display length. */
    int dlen;           /* True underlying length. */
    int dpos;           /* True underlying position. */
    int dsel;           /* Start of selection */
    int dend;           /* End of selection */
    int cols;           /* Number of columns in terminal. */
    int oldrow;         /* Row of previous cursor position (multiline mode) */
    int maxrows;        /* Maximum num of rows used so far (multiline mode) */
    int history_index;  /* The history index we are currently editing. */
    int need_resize;    /* Need resize flag. */
    int need_refresh;   /* Need refresh. */
    int selmode;        /* Visual selection being made. */
    int selinclusive;   /* Selections include character right of endpoint. */
    int noninteractive; /* No character editing, even if input is tty. */
    struct lino_undo *undo_stack;
    lino_error_t error; /* Most recent error. */
};

struct lino_undo {
    struct lino_undo *next;
    int triv;
    wchar_t *data;
    int dpos;
    int hist_index;
};

#define CTL(LETTER) ((LETTER) - '@')

enum key_action {
    TAB = 9,
    ENTER = 13,
    ESC = 27,
    BACKSPACE =  127
};

static lino_os_t lino_os;
static lino_t lino_list = { &lino_list, &lino_list };
volatile sig_atomic_t lino_list_busy;
static int atexit_registered = 0; /* Register atexit just 1 time. */

#define nelem(array) (sizeof (array) / sizeof (array)[0])

static int wcsnprintf(wchar_t *s, size_t nchar, const wchar_t *fmt, ...)
{
    int ret;
    va_list vl;
    va_start (vl, fmt);
    ret = vswprintf(s, nchar, fmt, vl);
    va_end (vl);
    if (ret >= 0)
        return ret;
    return wcslen(s);
}

/* ======================= Low level terminal handling ====================== */

/* Set if to use or not the multi line mode. */
void lino_set_multiline(lino_t *ls, int ml) {
    ls->mlmode = ml;
}

int lino_get_multiline(lino_t *ls) {
    return ls->mlmode;
}

void lino_set_selinclusive(lino_t *ls, int si) {
    ls->selinclusive = si;
}

int lino_get_selinculsive(lino_t *ls)
{
    return ls->selinclusive;
}

void lino_set_noninteractive(lino_t *ls, int ni)
{
    ls->noninteractive = ni;
}

int lino_get_noninteractive(lino_t *ls)
{
    return ls->noninteractive;
}

void lino_set_atom_cb(lino_t *l, lino_atom_cb_t *cb, void *ctx)
{
    l->atom_callback = cb;
    l->ca_ctx = ctx;
}

void lino_set_enter_cb(lino_t *l, lino_enter_cb_t *cb, void *ctx)
{
    l->enter_callback = cb;
    l->ce_ctx = ctx;
}

static void atexit_handler(void);

/* Raw mode: 1960 magic shit. */
static int enable_raw_mode(lino_t *ls) {
    struct termios raw;
    int ifd = lino_os.fileno_fn(ls->tty_ifs);

    if (!isatty(ifd))
        goto fatal;

    if (!atexit_registered) {
        atexit(atexit_handler);
        atexit_registered = 1;
    }

    if (tcgetattr(ifd, &ls->orig_termios) == -1)
        goto fatal;

    raw = ls->orig_termios;  /* modify the original mode */
    /* input modes: no break, no CR to NL, no parity check, no strip char,
     * no start/stop output control. */
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    /* we don't change any output modes (c_oflag) */
    /* control modes - set 8 bit chars */
    raw.c_cflag |= (CS8);
    /* local modes - choing off, canonical off, no extended functions,
     * no signal chars (^Z,^C) */
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    /* control chars - set return condition: min number of bytes and timer.
     * We want read to return every single byte, without timeout. */
    raw.c_cc[VMIN] = 1; raw.c_cc[VTIME] = 0; /* 1 byte, no timer */

    /* put terminal in raw mode after flushing */
    if (tcsetattr(ifd, TCSANOW, &raw) < 0)
        goto fatal;

    ls->rawmode = 1;
    return 0;

fatal:
    ls->error = lino_notty;
    return -1;
}

static void disable_raw_mode(lino_t *ls) {
    int ifd = lino_os.fileno_fn(ls->tty_ifs);

    /* Don't even check the return value as it's too late. */
    if (ls->rawmode && tcsetattr(ifd, TCSANOW, &ls->orig_termios) != -1)
        ls->rawmode = 0;
}

/* Use the ESC [6n escape sequence to query the horizontal cursor position
 * and return it. On error -1 is returned, on success the position of the
 * cursor. */
static int get_cursor_position(mem_t *ifs, mem_t *ofs) {
    wchar_t buf[32];
    int cols, rows;
    unsigned int i = 0;

    /* Report cursor location */
    if (!lino_os.puts_fn(ofs, L"\x1b[6n"))
        return -1;

    /* Read the response: ESC [ rows ; cols R */
    while (i < nelem(buf) - 1) {
        wint_t ch = lino_os.getch_fn(ifs);
        if (ch == WEOF)
            break;
        buf[i] = ch;
        if (ch == 'R')
            break;
        i++;
    }
    buf[i] = 0;

    /* Parse it. */
    if (buf[0] != ESC || buf[1] != '[')
        return -1;
    if (swscanf(buf + 2, L"%d;%d", &rows, &cols) != 2)
        return -1;
    return cols;
}

/* Try to get the number of columns in the current terminal, or assume 80
 * if it fails. */
static int get_columns(mem_t *ifs, mem_t *ofs) {
    int ofd = lino_os.fileno_fn(ofs);
#if HAVE_WINSIZE
    struct winsize ws;

    if (ioctl(ofd, TIOCGWINSZ, &ws) == 0 && ws.ws_col != 0)
        return ws.ws_col;
#endif

    {
        /* ioctl() failed or we don't have struct winsize.
           Try to query the terminal itself. */
        int start, cols;

        /* Get the initial position so we can restore it later. */
        start = get_cursor_position(ifs, ofs);
        if (start == -1) goto failed;

        /* Go to right margin and get position. */
        if (!lino_os.puts_fn(ofs, L"\x1b[999C"))
            goto failed;
        cols = get_cursor_position(ifs, ofs);
        if (cols == -1) goto failed;

        /* Restore position. */
        if (cols > start) {
            wchar_t seq[32];
            wcsnprintf(seq, nelem(seq), L"\x1b[%dD", cols - start);
            (void) lino_os.puts_fn(ofs, seq);
        }
        return cols;
    }

failed:
    return 80;
}

/* Clear the screen. Used to handle ctrl+l */
int lino_clear_screen(lino_t *ls) {
    ls->maxrows = 0;
    return (lino_os.puts_fn(ls->tty_ofs, L"\x1b[H\x1b[2J") >= 0);
}

static void refresh_line(lino_t *l);

static void handle_resize(lino_t *ls, lino_t *lc)
{
    if (ls->need_resize) {
        ls->need_resize = 0;
        if (lc != 0)
            lc->need_resize = 0;
        ls->cols = get_columns(ls->tty_ifs, ls->tty_ofs);
        if (lc)
            lc->cols = ls->cols;
        refresh_line(ls);
    }
}

/* Beep, used for completion when there is nothing to complete or when all
 * the choices were already shown. */
static int generate_beep(lino_t *ls) {
    return lino_os.puts_fn(ls->tty_ofs, L"\x7");
}

static void delete_undo(struct lino_undo **pundo)
{
    struct lino_undo *u = *pundo;

    if (u) {
        *pundo = u->next;
        lino_os.free_fn(u->data);
        u->data = 0;
        lino_os.free_fn(u);
    }
}

static void free_undo_stack(lino_t *l)
{
    while (l->undo_stack != 0)
        delete_undo(&l->undo_stack);
}

static void record_undo(lino_t *l)
{
    struct lino_undo *rec = (struct lino_undo *) lino_os.alloc_fn(sizeof *rec), *iter;
    wchar_t *data = (wchar_t *) lino_os.wstrdup_fn(l->data);
    int count;

    if (rec == 0 || data == 0) {
        lino_os.free_fn(rec);
        lino_os.free_fn(data);
        return;
    }

    rec->next = l->undo_stack;
    rec->triv = 0;
    rec->dpos = l->dpos;
    rec->hist_index = INT_MAX;
    rec->data = data;

    l->undo_stack = rec;

    for (iter = l->undo_stack, count = 0;
         iter != 0 && count < LINENOISE_MAX_UNDO;
         iter = iter->next, count++)
        /* empty */;

    if (iter != 0) {
        while (iter->next)
            delete_undo(&iter->next);
    }
}

static void record_triv_undo(lino_t *l)
{
    struct lino_undo *top = l->undo_stack;

    if (top != 0 && top->triv &&
        (top->hist_index == INT_MAX || top->hist_index == l->history_index) &&
        top->dpos < l->dpos)
        return;

    record_undo(l);
    l->undo_stack->triv = 1;
}

static void remove_noop_undo(lino_t *l)
{
    struct lino_undo *top = l->undo_stack;

    if (top != 0 &&
        (top->hist_index == INT_MAX || top->hist_index == l->history_index) &&
        wcscmp(l->data, top->data) == 0)
    {
        delete_undo(&l->undo_stack);
    }
}

static void restore_undo(lino_t *l)
{
    struct lino_undo **ptop = &l->undo_stack;

    while (*ptop) {
        struct lino_undo *top = *ptop;
        int hidx = top->hist_index;

        if (hidx == INT_MAX || hidx == l->history_index) {
            int dlen = wcslen(top->data);

            if (dlen) {
                wcscpy(l->data, top->data);
                l->dlen = dlen;
                l->dpos = top->dpos;
                l->need_refresh = 1;

                if (hidx == l->history_index) {
                    int history_pos = l->history_len - 1 - l->history_index;
                    lino_os.free_fn(l->history[history_pos]);
                    l->history[history_pos] = lino_os.wstrdup_fn(l->data);
                }
                delete_undo(ptop);
                break;
            }
            delete_undo(ptop);
        } else if (hidx >= l->history_len - 1) {
            delete_undo(ptop);
        } else {
            ptop = &top->next;
        }
    }
}

static void undo_subst_hist_idx(lino_t *l, int from_hist, int to_hist)
{
    struct lino_undo *iter;
    for (iter = l->undo_stack; iter != 0; iter = iter->next) {
        if (iter->hist_index == from_hist)
            iter->hist_index = to_hist;
    }
}

static void undo_renumber_hist_idx(lino_t *l, int delta)
{
    struct lino_undo *iter;
    for (iter = l->undo_stack; iter != 0; iter = iter->next) {
        assert (iter->hist_index != INT_MAX);
        iter->hist_index += delta;
    }
}

/* ============================== Completion ================================ */

/* Free a list of completion option populated by lino_add_completion(). */
static void free_completions(lino_completions_t *lc) {
    size_t i;
    for (i = 0; i < lc->len; i++)
        lino_os.free_fn(lc->cvec[i]);
    if (lc->cvec != NULL)
        lino_os.free_fn(lc->cvec);
}

static void sync_data_to_buf(lino_t *l);

static int compare_completions(const void *larg, const void *rarg)
{
    const wchar_t * const *lelem = convert(const wchar_t * const *, larg);
    const wchar_t * const *relem = convert(const wchar_t * const *, rarg);
    const wchar_t *lstr = *lelem, *rstr = *relem;
    size_t llen = wcslen(lstr);
    size_t rlen = wcslen(rstr);

    if (llen < rlen)
        return -1;
    if (llen > rlen)
        return 1;

    return wcscmp(lstr, rstr);
}

/* This is an helper function for edit() and is called when the
 * user types the <tab> key in order to complete the string currently in the
 * input.
 *
 * The state of the editing is encapsulated into the pointed lino_state
 * structure as described in the structure definition. */
static int complete_line(lino_t *ls, int substring) {
    lino_completions_t lc = { 0, NULL, 0 };
    wint_t c = 0;
    wchar_t save = ls->data[ls->dpos];
    lino_t *lt = lino_copy(ls);

    lc.substring = substring;

    if (lt == 0)
        return -1;

    ls->data[ls->dpos] = 0;
    ls->completion_callback(ls->data, &lc, ls->cb_ctx);
    ls->data[ls->dpos] = save;

    qsort(lc.cvec, lc.len, sizeof *lc.cvec, compare_completions);

    if (lc.len == 0) {
        generate_beep(ls);
    } else {
        int stop = 0;
        size_t i = 0;

        while(!stop) {
            /* Show completion or original buffer */
            if (i < lc.len) {
                int n = wcsnprintf(lt->data, nelem(lt->data),
                                   L"%ls%ls", lc.cvec[i], ls->data + ls->dpos);
                lt->dlen = n;
                lt->dpos = wcslen(lc.cvec[i]);
                sync_data_to_buf(lt);
                refresh_line(lt);
            } else {
                refresh_line(ls);
            }

            c = lino_os.getch_fn(ls->tty_ifs);

            if (c < 0) {
                if (errno == EINTR) {
                    handle_resize(ls, lt);
                    continue;
                }
                free_completions(&lc);
                lino_os.free_fn(lt);
                ls->error = (lino_os.eof_fn(ls->tty_ifs) ? lino_eof : lino_ioerr);
                return -1;
            }

            switch (c) {
                case TAB:
                    i = (i+1) % (lc.len+1);
                    if (i == lc.len) generate_beep(ls);
                    break;
                case CTL('C'):
                    /* Re-show original buffer */
                    if (i < lc.len) refresh_line(ls);
                    stop = 1;
                    c = 0;
                    break;
                default:
                    /* Update buffer and return */
                    if (i < lc.len) {
                        ls->dpos = lt->dpos;
                        ls->dlen = lt->dlen;
                        wcscpy(ls->data, lt->data);
                        sync_data_to_buf(ls);
                        refresh_line(ls);
                    }
                    stop = 1;
                    break;
            }
        }
    }

    free_completions(&lc);
    lino_free(lt);
    return c; /* Return last read character */
}

/* Register a callback function to be called for tab-completion. */
void lino_set_completion_cb(lino_t *ls, lino_compl_cb_t *fn, void *ctx) {
    ls->completion_callback = fn;
    ls->cb_ctx = ctx;
}

/* This function is used by the callback function registered by the user
 * in order to add completion options given the input string when the
 * user typed <tab>. See the example.c source code for a very easy to
 * understand example. */
void lino_add_completion(lino_completions_t *lc, const wchar_t *str) {
    size_t len = wcslen(str);
    wchar_t *copy, **cvec;

    copy = coerce(wchar_t *, lino_os.wmalloc_fn(len + 1));
    if (copy == NULL) return;
    wmemcpy(copy, str, len + 1);
    cvec = coerce(wchar_t **, lino_os.realloc_fn(coerce(mem_t *, lc->cvec),
                                                 (lc->len+1) * sizeof *cvec));
    if (cvec == NULL) {
        lino_os.free_fn(copy);
        return;
    }
    lc->cvec = cvec;
    lc->cvec[lc->len++] = copy;
}

static int next_hist_match(lino_t *l, wchar_t *pat, int cur, int *offs)
{
    int i;

    if (cur >= l->history_len)
        cur = l->history_len - 1;

    for (i = cur; i >= 0; i--) {
        wchar_t *hline = l->history[i];
        wchar_t *pmatch = wcsstr(hline, pat);
        if (pmatch != 0) {
            *offs = pmatch - hline;
            return i;
        }
    }
    return -1;
}

static void copy_display_params(lino_t *, const lino_t *);

static int history_search(lino_t *l)
{
    wchar_t hpat[128] = L"";
    int hi = l->history_len - l->history_index - 2 + (l->history_index == 0);
    int hp = hi, hl = 0, stop = 0;
    int dp = l->dpos;
    const wchar_t *fmt = L"[%ls]%ls";
    int ex = wcslen(fmt) - 2*wcslen(L"%ls");
    lino_t *lc = lino_copy(l), *ld = lino_copy(l);
    int c = -1;

    if (lc == 0 || ld == 0)
        goto out;

    lc->prompt = L"search:";

    while (!stop) {
        int vb = 0;
        int nw = wcsnprintf(lc->data, nelem(lc->data), fmt, hpat, l->data);
        lc->dlen = nw;
        lc->dpos = dp + hl + ex;

        if (lc->dpos > lc->dlen)
            lc->dpos = lc->dlen;

        refresh_line(lc);

        for (;;) {
            c = lino_os.getch_fn(lc->tty_ifs);

            if (c < 0) {
                if (errno == EINTR) {
                    handle_resize(lc, l);
                    continue;
                }
                stop = 1;
            } else {
                if (vb)
                    goto verbatim;

                switch (c) {
                default:
                    if (c < 32)
                        continue;
                verbatim:
                    if (hl >= convert(int, nelem(hpat)))
                        break;
                    hpat[hl++] = c;
                    /* fallthrough */
                    if (0) {
                case CTL('R'):
                        if (hl == 0) {
                            generate_beep(lc);
                            break;
                        }
                        hp = hi - 1;
                    }

                    {
                        int ni, sp = hp;

                        do {
                            ni = next_hist_match(l, hpat, sp, &dp);
                            sp = ni - 1;
                        } while (ni >= 0 && hi < l->history_len &&
                                 ni != hi &&
                                 wcscmp(l->history[hi], l->history[ni]) == 0);
                        if (ni < 0)
                            break;

                        hi = ni;
                        wcscpy(l->data, l->history[hi]);
                        l->dpos = l->dlen = wcslen(l->data);
                    }
                    break;
                case BACKSPACE: case CTL('H'):
                    if (hl == 0)
                        break;

                    hpat[--hl] = 0;
                    break;
                case ENTER:
                    stop = 1;
                    break;
                case CTL('C'):
                    wcscpy(l->data, ld->data);
                    l->dpos = ld->dpos;
                    l->dlen = ld->dlen;
                    stop = 1;
                    c = 0;
                    break;
                case CTL('F'): case CTL('B'):
                case CTL('A'): case CTL('E'):
                case CTL('N'): case CTL('P'):
                case CTL('D'): case CTL('U'): case CTL('W'): case CTL('K'):
                case CTL('T'): case CTL('X'):
                case ESC:
                    if (hi < l->history_len)
                        l->history_index = l->history_len - hi - 1;
                    l->dpos = dp;
                    stop = 1;
                    break;
                case CTL('V'):
                    vb = 1;
                    continue;
                case CTL('L'):
                    lino_clear_screen(lc);
                    break;
                case CTL('Z'):
                    disable_raw_mode(l);
                    raise(SIGTSTP);
                    enable_raw_mode(l);
                }
            }
            break;
        }
    }

out:
    copy_display_params(l, lc);
    lino_free(lc);
    lino_free(ld);
    refresh_line(l);
    return c;
}

static void show_help(lino_t *l)
{
    lino_t *lc = lino_copy(l);
    int i;
    static const wchar_t *help[] = {
        L"^B left     ^A start buf/ln ^T char swap     ^U del ln beg  ^R  hist srch\r"
        L"^F forward  ^E end buf/ln   ^D del right     ^K del ln end  Tab complete\r"
        L"^5 parmatch ^] parmatch     ^W del word left ^V next ch verbatim [p. 1/3]",

        L"^L refresh  ^P hist prev    ^S select        ^Q paste   ^J multi ln toggle\r"
        L"^C cancel   ^N hist next    ^^ sel endpt swp ^D sel cut\r"
        L"^Z suspend  ^O undo         ^Y yank          ^W sel + word cut   [p. 2/3]",

        L"^X^V verbatim ins mode  ^X^A     ins prev ln atom      ^X^E extrn editor\r"
        L"^X^R ins prev ln        ^X+Enter submit; keep hist pos ^X^Q exch clip/sel\r"
        L"^X^W ins prev ln word   ^X+Tab   substring complete              [p. 3/3]"
    };
    lc->mlmode = 1;
    lc->prompt = L"";
    for (i = 0; i < 3; i++) {
        wint_t c;
        wint_t seq[3];
        lc->dlen = wcsnprintf(lc->data, nelem(lc->data), L"%ls", help[i]);
        lc->dpos = lc->dlen;
        lc->need_refresh = 1;
        refresh_line(lc);
        c = lino_os.getch_fn(l->tty_ifs);
        if (c == CTL('C'))
            break;
        switch (c) {
        case CTL('C'):
            break;
        back:
        case CTL('H'): case BACKSPACE:
            if (i > 0)
                i -= 2;
            else
                break;
            continue;
        case ESC:
            if ((seq[0] = lino_os.getch_fn(l->tty_ifs)) < 0)
                break;
            if ((seq[1] = lino_os.getch_fn(l->tty_ifs)) < 0)
                break;

            if (seq[0] == '[') {
                if (seq[1] >= '0' && seq[1] <= '9') {
                    if ((seq[2] = lino_os.getch_fn(l->tty_ifs)) < 0)
                        break;
                    if (seq[2] == '~') {
                        switch(seq[1]) {
                        case '3': /* Delete key. */
                            goto back;
                        }
                    }
                } else {
                    switch(seq[1]) {
                    case 'A': /* Up */
                        goto back;
                    case 'B': /* Down */
                        continue;
                    case 'C': /* Right */
                        continue;
                    case 'D': /* Left */
                        goto back;
                    case 'H': /* Home */
                    home:
                        i = -1;
                        continue;
                    case 'F': /* End*/
                    end:
                        i = 1;
                        continue;
                    }
                }
            } else if (seq[0] == 'O') {
                switch(seq[1]) {
                case 'H': /* Home */
                    goto home;
                case 'F': /* End*/
                    goto end;
                }
            }
            continue;
        case CTL('Z'):
            disable_raw_mode(l);
            raise(SIGTSTP);
            enable_raw_mode(l);
            i -= 1;
            continue;
        case CTL('L'):
            lino_clear_screen(l);
            l->need_refresh = 1;
            i -= 1;
            continue;
        default:
            continue;
        }

        break;
    }
    lc->prompt = l->prompt;
    lc->dlen = l->dlen;
    lc->dpos = l->dpos;
    memcpy(lc->data, l->data, sizeof lc->data);
    lc->need_refresh = 1;
    refresh_line(lc);
    lino_free(lc);
}

/* =========================== Line editing ================================= */

/* We define a very simple "append buffer" structure, that is an heap
 * allocated string where we can append to. This is useful in order to
 * write all the escape sequences in a buffer and flush them to the standard
 * output in a single call, to avoid flickering effects. */
struct abuf {
    wchar_t *b;
    int len;
};

static void ab_init(struct abuf *ab) {
    ab->b = NULL;
    ab->len = 0;
}

static void ab_append(struct abuf *ab,
                      const wchar_t *s, int len) {
    wchar_t *ns = lino_os.wrealloc_fn(ab->b, ab->len + len + 1);

    if (ns == NULL) return;
    wmemcpy(ns+ab->len, s, len);
    ab->b = ns;
    ab->len += len;
    ab->b[ab->len] = 0;
}

static void ab_free(struct abuf *ab) {
    lino_os.free_fn(ab->b);
}

/* Convert raw data to display data, and recalculate
   display length and position. */
static void sync_data_to_buf(lino_t *l)
{
    wchar_t *dptr = l->data, *bptr = l->buf;
    int col = wcslen(l->prompt);
    int rev = l->dsel > l->dend;

    if (l->mlmode) {
        bptr += wcsnprintf(l->buf, nelem(l->buf), L"%ls",
                           l->prompt);
    }

    while (bptr - l->buf < convert(ptrdiff_t, nelem (l->buf)) - 1) {
        int dpos = dptr - l->data;
        int pos = bptr - l->buf;
        wchar_t ch = *dptr++;

        if (l->dpos == dpos)
            l->pos = pos;
        if (l->dsel == dpos)
            l->sel = pos;
        if (l->dend == dpos)
            l->end = pos;
        if (l->dsel == dpos - 1 && rev && l->selinclusive && ch && ch != '\r')
            l->sel = pos;

        if (ch) {
            if (ch == TAB) {
                do {
                    *bptr++ = ' ';
                    col++;
                } while (col % 8 != 0);
            } else if (l->mlmode && ch == '\r') {
                *bptr++ = '\r';
                *bptr++ = '\n';
                col = 0;
            } else if (ch < ' ') {
                *bptr++ = '^';
                *bptr++ = '@' + ch;
                col += 2;
            } else if (ch == 127) {
                *bptr++ = '^';
                *bptr++ = '?';
                col += 2;
            } else if (ch < 256) {
                *bptr++ = ch;
                col++;
            } else {
                *bptr++ = ch;
                col++;
                if (lino_os.wide_display_fn(ch)) {
                    *bptr++ = LINO_PAD_CHAR;
                    col++;
                }
            }

            continue;
        }
        break;
    }

    l->len = bptr - l->buf;
    *bptr++ = 0;
}

static void copy_display_params(lino_t *to, const lino_t *from)
{
    to->mlmode = from->mlmode;
    to->cols = from->cols;
    to->oldrow = from->oldrow;
    to->maxrows = from->maxrows;
}

/* Single line low level line refresh.
 *
 * Rewrite the currently edited line accordingly to the buffer content,
 * cursor position, and number of columns of the terminal. */
static void refresh_singleline(lino_t *l) {
    wchar_t seq[64];
    int plen = wcslen(l->prompt);
    wchar_t *buf = l->buf;
    int len = l->len;
    int pos = l->pos;
    struct abuf ab;
    int sel = l->sel;
    int end = l->end;

    if (sel > end) {
        int tmp = end;
        end = sel;
        sel = tmp;
    }

    while((plen+pos) >= l->cols) {
        buf++;
        len--;
        pos--;
        if (end > 0)
            end--;
        if (sel > 0)
            sel--;
    }
    while (plen+len > l->cols) {
        len--;
    }
    if (end > len)
        end = len;
    if (sel > len)
        sel = len;

    ab_init(&ab);
    /* Cursor to left edge */
    wcsnprintf(seq, 64, L"\r");
    ab_append(&ab, seq, wcslen(seq));
    /* Write the prompt and the current buffer content */
    ab_append(&ab, l->prompt, wcslen(l->prompt));
    if (!l->selmode) {
        ab_append(&ab, buf, len);
    } else {
        if (sel > 0)
            ab_append(&ab, buf, sel);
        if (end - sel > 0) {
            ab_append(&ab, L"\x1b[7m", 4);
            ab_append(&ab, buf + sel, end - sel);
            ab_append(&ab, L"\x1b[m", 3);
        }
        if (len - end > 0)
            ab_append(&ab, buf + end, len - end);
    }

    /* Erase to right */
    wcsnprintf(seq, 64, L"\x1b[0K");
    ab_append(&ab, seq, wcslen(seq));
    /* Move cursor to original position. */
    wcsnprintf(seq, 64, L"\r\x1b[%dC", pos + plen);
    ab_append(&ab, seq, wcslen(seq));
    (void) lino_os.puts_fn(l->tty_ofs, ab.b);
    ab_free(&ab);
}

struct row_values {
    int rows[2];
};

static struct row_values screen_rows(const wchar_t *str, int pos, int cols)
{
    const wchar_t *start = str;
    int col;
    struct row_values out = { { 1, 1 } };

    for (col = 0; ; str++) {
        wchar_t ch = *str;
        int atpos = (str - start == convert(ptrdiff_t, pos));

        switch (ch) {
        case '\n':
            col = 0;
            out.rows[0]++;
            /* fallthrough */
        case '\r': case 0:
            if (atpos)
                out.rows[1] = out.rows[0] + (col == cols);
            break;
        default:
            col++;
            if (col > cols) {
                col = 1;
                out.rows[0]++;
            }
            if (atpos)
                out.rows[1] = out.rows[0];
            break;
        }

        if (ch == 0)
            break;
    }

    return out;
}

static int col_offset_in_str(const wchar_t *str, int pos)
{
    int offs = 0;
    while (pos > 0 && str[--pos] != '\n')
        offs++;
    return offs;
}

/* Multi line low level line refresh.
 *
 * Rewrite the currently edited line accordingly to the buffer content,
 * cursor position, and number of columns of the terminal. */
static void refresh_multiline(lino_t *l) {
    wchar_t seq[64];
    struct row_values r = screen_rows(l->buf, l->pos, l->cols);
    int rows = r.rows[0]; /* rows used by current buf. */
    int nrow = r.rows[1]; /* cursor row after refresh. */
    int oldmaxrows = l->maxrows;
    int j;
    struct abuf ab;

    /* Update maxrows if needed. */
    if (rows > l->maxrows)
        l->maxrows = rows;

    /* First step: clear all the lines used before. To do so start by
     * going to the last row. */
    ab_init(&ab);
    if (oldmaxrows - l->oldrow > 0) {
        wcsnprintf(seq, 64, L"\x1b[%dB", oldmaxrows - l->oldrow);
        ab_append(&ab, seq, wcslen(seq));
    }

    /* Now for every row clear it, go up. */
    for (j = 0; j < oldmaxrows - 1; j++) {
        wcsnprintf(seq, 64, L"\r\x1b[0K\x1b[1A");
        ab_append(&ab, seq, wcslen(seq));
    }

    /* Clean the top line. */
    wcsnprintf(seq, 64, L"\r\x1b[0K");
    ab_append(&ab, seq, wcslen(seq));

    /* Write the current buffer content which includes the prompt */
    if (!l->selmode) {
        ab_append(&ab,l->buf,l->len);
    } else {
        int sel = l->sel;
        int end = l->end;
        int len = l->len;

        if (sel > end) {
            int tmp = end;
            end = sel;
            sel = tmp;
        }

        if (sel > 0)
            ab_append(&ab, l->buf, sel);
        if (end - sel > 0) {
            ab_append(&ab, L"\x1b[7m", 4);
            ab_append(&ab, l->buf + sel, end - sel);
            ab_append(&ab, L"\x1b[m", 3);
        }
        if (len - end > 0)
            ab_append(&ab, l->buf + end, len - end);
    }

    /* Cursor has wrapped to new row beyond rows. */
    if (nrow > rows) {
        ab_append(&ab, L"\r\n", 2);
        rows++;
        if (rows > l->maxrows)
            l->maxrows = rows;
    }

    /* Move cursor to the correct column */
    {
        int ccol = col_offset_in_str(l->buf, l->pos) % l->cols;

        /* Go up till we reach the expected positon. */
        if (rows - nrow > 0) {
            wcsnprintf(seq, 64, L"\x1b[%dA", rows - nrow);
            ab_append(&ab, seq, wcslen(seq));
        }

        /* Set column. */
        if (ccol)
            wcsnprintf(seq, 64, L"\r\x1b[%dC", ccol);
        else
            wcsnprintf(seq, 64, L"\r");

        ab_append(&ab, seq, wcslen(seq));
    }

    /* Remember current cursor row for next time */
    l->oldrow = nrow;

    (void) lino_os.puts_fn(l->tty_ofs, ab.b);
    ab_free(&ab);
}

/* Calls the two low level functions refresh_singleline() or
 * refresh_multiline() according to the selected mode. */
static void refresh_line(lino_t *ls) {

    sync_data_to_buf(ls);

    if (ls->mlmode)
        refresh_multiline(ls);
    else
        refresh_singleline(ls);
}

static int scan_match_rev(const wchar_t *s, int i, wchar_t mch)
{
    while (i > 0) {
        int ch = s[--i];

        if (ch == mch)
            return i;

        switch (ch) {
        case ')':
            if ((i = scan_match_rev(s, i, '(')) == -1)
                return -1;
            break;
        case ']':
            if ((i = scan_match_rev(s, i, '[')) == -1)
                return -1;
            break;
        case '}':
            if ((i = scan_match_rev(s, i, '{')) == -1)
                return -1;
            break;
        case '(': case '[': case '{':
            return -1;
        default:
            break;
        }
    }

    return -1;
}

static int scan_rev(const wchar_t *s, int i)
{
    switch (s[i]) {
    case ')':
        return scan_match_rev(s, i, '(');
    case ']':
        return scan_match_rev(s, i, '[');
    case '}':
        return scan_match_rev(s, i, '{');
    default:
        return -1;
    }
}

static int scan_match_fwd(const wchar_t *s, int i, wchar_t mch)
{
    while (s[++i]) {
        int ch = s[i];

        if (ch == mch)
            return i;

        switch (ch) {
        case '(':
            if ((i = scan_match_fwd(s, i, ')')) == -1)
                return -1;
            break;
        case '[':
            if ((i = scan_match_fwd(s, i, ']')) == -1)
                return -1;
            break;
        case '{':
            if ((i = scan_match_fwd(s, i, '}')) == -1)
                return -1;
            break;
        case ')': case ']': case '}':
            return -1;
        default:
            break;
        }
    }

    return -1;
}

static int scan_fwd(const wchar_t *s, int i)
{
    switch (s[i]) {
    case '(':
        return scan_match_fwd(s, i, ')');
    case '[':
        return scan_match_fwd(s, i, ']');
    case '{':
        return scan_match_fwd(s, i, '}');
    default:
        return -1;
    }
}

static int find_nearest_paren(const wchar_t *s, int i)
{
    static const wchar_t *ope = L"([{";
    static const wchar_t *clo = L")]}";
    int pre = -1, nxt = -1, j;

    for (j = i; j != -1; j--) {
        if (s[j] && (wcschr(ope, s[j]) || wcschr(clo, s[j]))) {
            pre = j;
            break;
        }
    }

    for (j = i; s[j] != 0; j++) {
        if (wcschr(ope, s[j]) || wcschr(clo, s[j])) {
            nxt = j;
            break;
        }
    }

    if (pre == -1)
        return nxt;

    if (nxt == -1)
        return pre;

    if (i - pre > nxt - i)
        return nxt;

    if (i - pre < nxt - i)
        return pre;

    if (wcschr(ope, s[pre]) && wcschr(ope, s[nxt]))
        return nxt;

    if (wcschr(clo, s[pre]) && wcschr(clo, s[nxt]))
        return pre;

    return nxt;
}

static void usec_delay(lino_t *l, long usec)
{
#if HAVE_POLL
    struct pollfd pfd;
    pfd.fd = lino_os.fileno_fn(l->tty_ifs);
    pfd.events = POLLIN;
    poll(&pfd, 1, usec/1000);
#elif HAVE_POSIX_NANOSLEEP
    struct timespec ts;
    (void) l;
    ts.tv_sec = usec / 1000000;
    ts.tv_nsec = (usec % 1000000) * 1000;
    nanosleep(&ts, 0);
#elif HAVE_POSIX_USLEEP
    (void) l;
    if (u >= 1000000)
        sleep(u / 1000000);
    usleep(u % 1000000);
#else
#error portme
#endif
}

static void paren_jump(lino_t *l)
{
    int pos = scan_rev(l->data, l->dpos - 1);

    if (pos == -1)
        pos = scan_fwd(l->data, l->dpos - 1);

    if (pos != -1) {
        int dp = l->dpos;
        l->dpos = pos;
        refresh_line(l);
        usec_delay(l, LINENOISE_PAREN_DELAY);
        l->dpos = dp;
        l->need_refresh = 1;
    }
}

static void flash(lino_t *l, int ch)
{
    int i;

    if (l->dlen >= (int) nelem (l->data) - 1)
        return;

    for (i = 0; i < 2; i++) {
        if (i > 0)
            usec_delay(l, LINENOISE_FLASH_DELAY);
        l->data[l->dlen++] = ch;
        l->data[l->dlen] = 0;
        refresh_line(l);
        usec_delay(l, LINENOISE_FLASH_DELAY);
        l->data[--l->dlen] = 0;
        refresh_line(l);
    }
}


static void update_sel(lino_t *l)
{
    if (l->selmode) {
        int oend = l->dend;
        l->dend = l->dpos;
        l->need_refresh |= (oend != l->dend);
    }
}

static void clear_sel(lino_t *l)
{
    if (l->selmode) {
        l->selmode = 0;
        l->dsel = l->dend = 0;
        l->need_refresh = 1;
    }
}

static void yank_sel(lino_t *l)
{
    if (l->selmode) {
        int notrev = l->dsel <= l->dend;
        int sel = notrev ? l->dsel : l->dend;
        int end = notrev ? l->dend : l->dsel;

        if (l->selinclusive && l->data[end] && l->data[end] != '\r')
            end++;

        if (end - sel > 0) {
            lino_os.free_fn(l->clip);
            l->clip = coerce(wchar_t *, lino_os.alloc_fn(end - sel + 1));
            wmemcpy(l->clip, l->data + sel, end - sel);
            l->clip[end - sel] = 0;
            l->dpos = sel;
        }
    }
}

static void delete_sel(lino_t *l)
{
    if (l->selmode) {
        int notrev = l->dsel <= l->dend;
        int sel = notrev ? l->dsel : l->dend;
        int end = notrev ? l->dend : l->dsel;
        int len = l->dlen;

        if (l->selinclusive && l->data[end] && l->data[end] != '\r')
            end++;

        if (len - end > 0)
            wmemmove(l->data + sel, l->data + end, len - end);

        len -= (end - sel);
        l->data[len] = 0;
        l->dlen = len;
        l->dpos = sel;

        clear_sel(l);
    }
}


/* Insert the character 'c' at cursor current position.
 *
 * On error writing to the terminal -1 is returned, otherwise 0. */
static int edit_insert(lino_t *l, wchar_t c) {
    if (l->dlen < (int) nelem(l->data) - 1) {
        record_triv_undo(l);
        delete_sel(l);
        if (l->dpos == l->dlen) {
            l->data[l->dpos] = c;
            l->dpos++;
            l->dlen++;
            l->data[l->dlen] = '\0';
            if ((!l->mlmode && l->len == l->dlen && l->plen+l->len < l->cols)) {
                /* Avoid a full update of the line in the
                 * trivial case. */
                wchar_t str[2] = { c };
                if (!lino_os.puts_fn(l->tty_ofs, str))
                    return -1;
            } else {
                l->need_refresh = 1;
            }
        } else {
            wmemmove(l->data + l->dpos+1, l->data + l->dpos, l->dlen-l->dpos);
            l->data[l->dpos] = c;
            l->dlen++;
            l->dpos++;
            l->data[l->dlen] = '\0';
            l->need_refresh = 1;
        }
    }
    return 0;
}

static int edit_insert_str(lino_t *l, const wchar_t *s, int nchar)
{
    if (l->dlen < (int) nelem (l->data) - nchar) {
        record_undo(l);
        delete_sel(l);

        if (l->dpos < l->dlen)
            wmemmove(l->data + l->dpos + nchar, l->data + l->dpos, l->dlen - l->dpos);
        wmemcpy(l->data + l->dpos, s, nchar);
        l->dpos += nchar;
        l->dlen += nchar;
        l->data[l->dlen] = 0;
        l->need_refresh = 1;
        clear_sel(l);
    }
    return 0;
}

/* Move cursor on the left. */
static void edit_move_left(lino_t *l) {
    if (l->dpos > 0) {
        l->dpos--;
        l->need_refresh = 1;
    }
}

/* Move cursor on the right. */
static void edit_move_right(lino_t *l) {
    if (l->dpos != l->dlen) {
        l->dpos++;
        l->need_refresh = 1;
    }
}

static void edit_move_home(lino_t *l)
{
    if (l->dpos != 0) {
        l->dpos = 0;
        l->need_refresh = 1;
    }
}

static void edit_move_sol(lino_t *l) {
    if (!l->mlmode) {
        edit_move_home(l);
    } else {
        int dpos = l->dpos;

        while (dpos > 0 && l->data[dpos-1] != '\r')
            dpos--;

        if (l->dpos != dpos) {
            l->dpos = dpos;
            l->need_refresh = 1;
        } else {
            edit_move_home(l);
        }
    }
}

static void edit_move_end(lino_t *l)
{
    if (l->dpos != l->dlen) {
        l->dpos = l->dlen;
        l->need_refresh = 1;
    }
}

static void edit_move_eol(lino_t *l) {
    if (!l->mlmode) {
        edit_move_end(l);
    } else {
        int dpos = l->dpos;

        dpos += wcscspn(l->data + dpos, L"\r");

        if (l->dpos != dpos) {
            l->dpos = dpos;
            l->need_refresh = 1;
        } else {
            edit_move_end(l);
        }
    }
}

static void edit_move_matching_paren(lino_t *l)
{
    int p = find_nearest_paren(l->data, l->dpos);

    if (p != -1) {
        int fw = scan_fwd(l->data, p);
        int re = scan_rev(l->data, p);

        if (fw != -1) {
            l->dpos = fw;
            l->need_refresh = 1;
        } else if (re != -1) {
            l->dpos = re;
            l->need_refresh = 1;
        } else {
            l->dpos = p;
            l->need_refresh = 1;
        }
    }
}

/* Substitute the currently edited line with the next or previous history
 * entry as specified by 'dir'. */
#define LINENOISE_HISTORY_NEXT 0
#define LINENOISE_HISTORY_PREV 1
static void edit_history_next(lino_t *l, int dir) {
    clear_sel(l);
    undo_subst_hist_idx(l, INT_MAX, l->history_index);

    if (l->history_len > 1) {
        /* Update the current history entry before to
         * overwrite it with the next one. */
        lino_os.free_fn(l->history[l->history_len - 1 - l->history_index]);
        l->history[l->history_len - 1 - l->history_index] = lino_os.wstrdup_fn(l->data);
        /* Show the new entry */
        l->history_index += (dir == LINENOISE_HISTORY_PREV) ? 1 : -1;
        if (l->history_index < 0) {
            l->history_index = 0;
            return;
        } else if (l->history_index >= l->history_len) {
            l->history_index = l->history_len-1;
            return;
        }
        wcsncpy(l->data, l->history[l->history_len - 1 - l->history_index], nelem(l->data));
        l->data[nelem(l->data) - 1] = 0;
        l->dpos = l->dlen = wcslen(l->data);
        l->need_refresh = 1;
    }
}

/* Delete the character at the right of the cursor without altering the cursor
 * position. Basically this is what happens with the "Delete" keyboard key. */
static void edit_delete(lino_t *l) {
    if (l->selmode) {
        record_undo(l);
        delete_sel(l);
        return;
    }

    if (l->dlen > 0 && l->dpos < l->dlen) {
        record_undo(l);
        wmemmove(l->data + l->dpos, l->data + l->dpos + 1, l->dlen - l->dpos - 1);
        l->dlen--;
        l->data[l->dlen] = '\0';
        l->need_refresh = 1;
    }
    remove_noop_undo(l);
}

/* Backspace implementation. */
static void edit_backspace(lino_t *l) {
    if (l->selmode && l->dend > l->dsel) {
        record_undo(l);
        delete_sel(l);
        return;
    }

    record_undo(l);
    delete_sel(l);

    if (l->dpos > 0 && l->dlen > 0) {
        wmemmove(l->data + l->dpos - 1, l->data + l->dpos, l->dlen - l->dpos);
        l->dpos--;
        l->dlen--;
        l->data[l->dlen] = '\0';
        l->need_refresh = 1;
    }

    remove_noop_undo(l);
}

/* Delete all characters to left of cursor. */
static void edit_delete_prev_all(lino_t *l)
{
    clear_sel(l);

    if (!l->mlmode) {
        if (l->dpos > 0) {
            record_undo(l);
            wmemmove(l->data, l->data + l->dpos, l->dlen - l->dpos + 1);
            l->dlen -= l->dpos;
            l->dpos = 0;
            l->need_refresh = 1;
        }
    } else {
        wchar_t *e = l->data + l->dpos, *s = e;
        int delta;

        while (s > l->data && s[-1] != '\r')
            s--;

        delta = e - s;

        if (delta > 0) {
            record_undo(l);
            wmemmove(s, e, l->data + l->dlen - e);
            l->dlen -= delta;
            l->dpos -= delta;
            l->data[l->dlen] = 0;
            l->need_refresh = 1;
        }
    }
}

static void edit_delete_to_eol(lino_t *l)
{
    clear_sel(l);

    if (l->dlen != l->dpos) {
        if (!l->mlmode) {
            record_undo(l);
            l->data[l->dpos] = '\0';
            l->dlen = l->dpos;
            l->need_refresh = 1;
        } else {
            int delsize = wcscspn(l->data + l->dpos, L"\r");
            if (delsize != 0) {
                record_undo(l);
                if (l->dlen - delsize)
                    wmemmove(l->data + l->dpos, l->data + l->dpos + delsize,
                             l->dlen - l->dpos - delsize);
                l->dlen -= delsize;
                l->data[l->dlen] = 0;
                l->need_refresh = 1;
            }
        }
    }
}

/* Delete the previosu word, maintaining the cursor at the start of the
 * current word. */
static void edit_delete_prev_word(lino_t *l) {
    int odpos, diff;
    static const wchar_t *space = L"\r\t ";

    delete_sel(l);

    odpos = l->dpos;
    while (l->dpos > 0 && wcschr(space, l->data[l->dpos - 1]))
        l->dpos--;
    while (l->dpos > 0 && wcschr(space, l->data[l->dpos - 1]) == 0)
        l->dpos--;
    diff = odpos - l->dpos;
    if (diff != 0) {
        record_undo(l);
        wmemmove(l->data + l->dpos, l->data + odpos, l->dlen - odpos + 1);
        l->dlen -= diff;
        l->need_refresh = 1;
    }
}

static void edit_delete_line(lino_t *l)
{
    clear_sel(l);

    if (l->mlmode) {
        wchar_t *e = l->data + l->dpos, *s = e;
        int delta;

        while (s > l->data && s[-1] != '\r')
            s--;
        e += wcscspn(e, L"\r");
        if (*e == '\r')
            e++;

        delta = e - s;

        if (delta > 0) {
            record_undo(l);
            wmemmove(s, e, l->data + l->dlen - e);
            l->dlen -= delta;
            l->dpos = s - l->data;
            l->data[l->dlen] = 0;
            l->need_refresh = 1;
        }
    }
}

static void tr(wchar_t *s, wchar_t find, wchar_t rep)
{
    for (; *s; s++)
        if (*s == find)
            *s = rep;
}

static const char *get_home(void)
{
#ifdef __CYGWIN__
  struct utsname un;

  if (uname(&un) >= 0) {
    if (strncmp(un.sysname, "CYGNAL", 6) == 0)
      return getenv("USERPROFILE");
  }
#endif
  return getenv("HOME");
}


static void edit_in_editor(lino_t *l) {
    const char *templ = ".linotmpXXXXXX";
    mem_t *fo = 0;
    char *ed = getenv("EDITOR");
    char path[128];

    if (ed) {
        const char *ho = get_home();
        int fd;
#if HAVE_MKSTEMPS
        const char *suffix = l->suffix ? l->suffix : "";
#else
        const char *suffix = "";
#endif

        if (ho)
            snprintf(path, sizeof path, "%s/%s%s", ho, templ, suffix);
        else
            snprintf(path, sizeof path, "%s%s", templ, suffix);

#if HAVE_MKSTEMPS
        if ((fd = mkstemps(path, strlen(suffix))) != -1)
            fo = lino_os.fdopen_fn(fd, L"w");
#else
        if ((fd = mkstemp(path)) != -1)
            fo = lino_os.fdopen_fn(fd, L"w");
#endif

        if (!fo && fd != -1)
            close(fd);
    }

    if (fo) {
        char cmd[256];
        snprintf(cmd, sizeof cmd, "%s %s", ed, path);
        tr(l->data, '\r', '\n');

        if (lino_os.puts_fn(fo, l->data) && lino_os.puts_fn(fo, L"\n"))
        {
            mem_t *fi;
            lino_os.close_fn(fo);
            fo = 0;

            if (system(cmd) == 0 && (fi = lino_os.open8_fn(path, L"r")) != 0) {
                size_t len;
                (void) lino_os.gets_fn(fi, l->data, nelem(l->data));
                lino_os.close_fn(fi);
                len = wcslen(l->data);

                record_undo(l);

                l->data[len] = 0;
                if (len > 0 && l->data[len - 1] == '\n')
                    l->data[--len] = 0;
                l->dpos = l->dlen = len;
                tr(l->data, '\n', '\r');
                l->need_refresh = 1;
                remove_noop_undo(l);
            }
        }

        if (fo != 0)
            lino_os.close_fn(fo);

        remove(path);
        clear_sel(l);
    }
}

/* This function is the core of the line editing capability of linenoise.
 * It expects 'fd' to be already in "raw mode" so that every key pressed
 * will be returned ASAP to read().
 *
 * The resulting string is put into 'buf' when the user type enter, or
 * when ctrl+d is typed.
 *
 * The function returns the length of the current buffer. */
static int edit(lino_t *l, const wchar_t *prompt)
{
    int verbatim = 0, extended = 0, paste = 0, extend_num = -1;
    int ret = -1;

    /* Populate the linenoise state that we pass to functions implementing
     * specific editing functionalities. */
    l->prompt = prompt;
    l->plen = wcslen(prompt);
    l->pos = l->len = 0;
    l->dpos = l->dlen = 0;
    l->cols = get_columns(l->tty_ifs, l->tty_ofs);
    l->oldrow = l->maxrows = 0;
    l->history_index = 0;
    clear_sel(l);

    /* Buffer starts empty. */
    l->data[0] = '\0';

    /* The latest history entry is always our current buffer, that
     * initially is just an empty string. */
    lino_hist_add(l, L"");

    if (!lino_os.puts_fn(l->tty_ofs, prompt)) {
        l->error = lino_ioerr;
        return ret;
    }

    if (l->save_hist_idx) {
        int hi = l->history_len - l->save_hist_idx - 1;
        l->history_index = l->save_hist_idx;
        l->save_hist_idx = 0;
        wcscpy(l->data, l->history[hi]);
        l->dpos = l->dlen = wcslen(l->data);
        l->need_refresh = 1;
    }

    while(1) {
        wint_t c;
        wint_t seq[3];

        update_sel(l);

        if (l->need_refresh) {
            l->need_refresh = 0;
            refresh_line(l);
        }

        c = lino_os.getch_fn(l->tty_ifs);

        if (c < 0 && errno == EINTR) {
            handle_resize(l, 0);
            continue;
        }

        if (c < 0) {
            ret = l->len ? l->len : -1;
            goto out;
        }

        if (paste && (c == CTL('X'))) {
            paste = 0;
            continue;
        }

        if (verbatim ||
            (paste && c != ESC && c != BACKSPACE && c != CTL('H')))
        {
            if (edit_insert(l,c)) {
                l->error = lino_ioerr;
                goto out;
            }
            verbatim = 0;
            continue;
        }

        if (extended && c != TAB && c != ESC) {
            switch (c) {
            case CTL('E'):
                extended = 0;
                edit_in_editor(l);
                break;
            case CTL('K'):
                extended = 0;
                edit_delete_line(l);
                break;
            case CTL('V'):
                extended = 0;
                paste = 1;
                break;
            case CTL('W'): case 'w':
                extended = 0;
                if (l->history_len > 1 + l->history_index && extend_num != 0) {
                    wchar_t *prev_line = l->history[l->history_len - 2 - l->history_index];
                    wchar_t *word_end = prev_line + wcslen(prev_line);
                    wchar_t *word_start = word_end;

                    if (extend_num < 0)
                        extend_num = 1;

                    for (; extend_num--; word_end = word_start) {
                        while (word_end > prev_line && iswspace(convert(wint_t, word_end[-1])))
                            word_end--;

                        word_start = word_end;

                        while (word_start > prev_line && !iswspace(convert(wint_t, word_start[-1])))
                            word_start--;

                        if (extend_num == 0)
                            break;
                    }

                    if (edit_insert_str(l, word_start, word_end - word_start)) {
                        l->error = lino_ioerr;
                        goto out;
                    }
                }
                break;
            case CTL('A'): case 'a':
                extended = 0;
                if (extend_num < 0)
                    extend_num = 1;
                if (l->history_len > 1 + l->history_index && l->atom_callback)
                {
                    wchar_t *prev_line = l->history[l->history_len - 2
                                                    - l->history_index];
                    wchar_t *word = l->atom_callback(l, prev_line, extend_num, l->ca_ctx);
                    int res = 0;

                    if (word != 0) {
                        res = edit_insert_str(l, word, wcslen(word));
                        lino_os.free_fn(word);
                    }

                    if (res) {
                        l->error = lino_ioerr;
                        goto out;
                    }
                }
                break;
            case CTL('R'): case 'r':
                extended = 0;
                if (extend_num < 0)
                    extend_num = 1;
                if (l->history_len > extend_num + l->history_index) {
                    wchar_t *prev_line = l->history[l->history_len - 1
                                                    - extend_num
                                                    - l->history_index];
                    int res = edit_insert_str(l, prev_line, wcslen(prev_line));
                    if (res) {
                        l->error = lino_ioerr;
                        goto out;
                    }
                }
                break;
            case CTL('P'): case 'p':
                extended = 0;
                if (l->result) {
                    int res = edit_insert_str(l, l->result, wcslen(l->result));
                    if (res) {
                        l->error = lino_ioerr;
                        goto out;
                    }
                }
                break;
            case CTL('Q'):
                extended = 0;
                {
                    wchar_t *clip = l->clip;
                    l->clip = 0;
                    yank_sel(l);
                    if (clip != 0) {
                        edit_insert_str(l, clip, wcslen(clip));
                        lino_os.free_fn(clip);
                    }
                    clear_sel(l);
                }
                break;
            case ENTER:
                extended = 0;
                if (l->mlmode && l->enter_callback &&
                    !l->enter_callback(l->data, l->ce_ctx))
                {
                    if (edit_insert(l,c)) {
                        l->error = lino_ioerr;
                        goto out;
                    }
                    if (l->dpos == l->dlen)
                        flash(l, '!');
                    break;
                }
                if (l->mlmode)
                    edit_move_end(l);
                if (l->need_refresh)
                    refresh_line(l);
                ret = l->len;
                l->save_hist_idx = l->history_index;
                goto out;
            case '?':
                extended = 0;
                show_help(l);
                break;
            default:
                if (isdigit(convert(unsigned char, c))) {
                    if (extend_num < 0)
                        extend_num = 0;
                    extend_num %= 100;
                    extend_num *= 10;
                    extend_num += (c - '0');
                    break;
                }
                extended = 0;
                generate_beep(l);
                break;
            }
            continue;
        }


        /* Only autocomplete when the callback is set. It returns < 0 when
         * there was an error reading from fd. Otherwise it will return the
         * character that should be handled next. */
        switch (c) {
        case TAB:
            if (l->completion_callback != NULL) {
                record_undo(l);
                clear_sel(l);
                c = complete_line(l, extended);
            }
            extended = 0;
            break;
        case CTL('R'):
            record_undo(l);
            clear_sel(l);
            c = history_search(l);
            break;
        }

        if (c < 0)
            goto out;
        if (c == 0)
            continue;

        switch(c) {
        case ENTER:
            if (paste || (l->mlmode && l->enter_callback &&
                          !l->enter_callback(l->data, l->ce_ctx)))
            {
                if (edit_insert(l,c)) {
                    l->error = lino_ioerr;
                    goto out;
                }
                if (!paste && l->dpos == l->dlen)
                    flash(l, '!');
                break;
            }
            if (l->mlmode)
                edit_move_end(l);
            if (l->need_refresh)
                refresh_line(l);
            ret = l->len;
            goto out;
        case CTL('C'):
            if (l->mlmode) {
                edit_move_end(l);
                if (l->need_refresh)
                    refresh_line(l);
            }
            l->error = lino_intr;
            record_undo(l);
            goto out;
        case BACKSPACE:   /* backspace */
        case CTL('H'):
            edit_backspace(l);
            paren_jump(l);
            break;
        case CTL('D'):   /* remove char at right of cursor, or if the
                            line is empty, act as end-of-file. */
            if (l->dlen > 0) {
                yank_sel(l);
                edit_delete(l);
            } else {
                l->error = lino_eof;
                goto out;
            }
            break;
        case CTL('T'):   /* swaps current character with previous. */
            clear_sel(l);
            if (l->dpos > 0 && l->dpos < l->dlen) {
                int aux = l->data[l->dpos - 1];

                record_undo(l);
                l->data[l->dpos-1] = l->data[l->dpos];
                l->data[l->dpos] = aux;
                if (l->dpos != l->dlen - 1) l->dpos++;
                l->need_refresh = 1;
            }
            break;
        case CTL('B'):
            edit_move_left(l);
            break;
        case CTL('F'):
            edit_move_right(l);
            break;
        case CTL('P'):
            edit_history_next(l, LINENOISE_HISTORY_PREV);
            break;
        case CTL('N'):
            edit_history_next(l, LINENOISE_HISTORY_NEXT);
            break;
        case ESC:
            extended = 0;
            /* Read the next two bytes representing the escape sequence.
             * Use two calls to handle slow terminals returning the two
             * chars at different times. */
            if ((seq[0] = lino_os.getch_fn(l->tty_ifs)) < 0)
                break;
            if ((seq[1] = lino_os.getch_fn(l->tty_ifs)) < 0)
                break;

            /* ESC [ sequences. */
            if (seq[0] == '[') {
                if (seq[1] >= '0' && seq[1] <= '9') {
                    /* Extended escape, read additional byte. */
                    if ((seq[2] = lino_os.getch_fn(l->tty_ifs)) < 0)
                        break;
                    if (seq[2] == '~') {
                        switch(seq[1]) {
                        case '3': /* Delete key. */
                            edit_delete(l);
                            break;
                        }
                    }
                } else {
                    switch(seq[1]) {
                    case 'A': /* Up */
                        edit_history_next(l, LINENOISE_HISTORY_PREV);
                        break;
                    case 'B': /* Down */
                        edit_history_next(l, LINENOISE_HISTORY_NEXT);
                        break;
                    case 'C': /* Right */
                        edit_move_right(l);
                        break;
                    case 'D': /* Left */
                        edit_move_left(l);
                        break;
                    case 'H': /* Home */
                        edit_move_sol(l);
                        break;
                    case 'F': /* End*/
                        edit_move_eol(l);
                        break;
                    }
                }
            }

            /* ESC O sequences. */
            else if (seq[0] == 'O') {
                switch(seq[1]) {
                case 'H': /* Home */
                    edit_move_sol(l);
                    break;
                case 'F': /* End*/
                    edit_move_eol(l);
                    break;
                }
            }
            break;
        case ')': case ']': case '}':
        case '(': case '[': case '{':
            if (edit_insert(l,c)) {
                l->error = lino_ioerr;
                goto out;
            }
            paren_jump(l);
            break;
        default:
            if (c < 32)
                break;
            if (edit_insert(l,c)) {
                l->error = lino_ioerr;
                goto out;
            }
            break;
        case CTL('U'):
            edit_delete_prev_all(l);
            break;
        case CTL('V'): /* insert next char verbatim */
            verbatim = 1;
            break;
        case CTL('X'):
            extended = 1;
            extend_num = -1;
            continue;
        case CTL('S'):
            l->selmode ^= 1;
            if (l->selmode)
                l->dsel = l->dend = l->dpos;
            l->need_refresh = 1;
            break;
        case CTL('^'):
            if (l->selmode)
            {
                int tmp = l->dsel;
                l->dsel = l->dend;
                l->dend = l->dpos = tmp;
                l->need_refresh = 1;
            }
            break;
        case CTL('Y'):
            yank_sel(l);
            clear_sel(l);
            break;
        case CTL('Q'):
            if (l->clip != 0)
                edit_insert_str(l, l->clip, wcslen(l->clip));
            break;
        case CTL('K'):
            edit_delete_to_eol(l);
            break;
        case CTL('A'):
            edit_move_sol(l);
            break;
        case CTL('E'):
            edit_move_eol(l);
            break;
        case CTL(']'):
            edit_move_matching_paren(l);
            break;
        case CTL('L'):
            lino_clear_screen(l);
            l->need_refresh = 1;
            break;
        case CTL('W'):
            edit_delete_prev_word(l);
            break;
        case CTL('J'):
            if (l->mlmode) {
                wcsnprintf(l->buf, nelem(l->buf), L"%ls", l->prompt);
                l->pos = l->len = wcslen(l->buf);
                refresh_multiline(l);
            }

            l->mlmode ^= 1;
            l->need_refresh = 1;
            break;
        case CTL('Z'):
            {
                int dpos = l->dpos;
                if (l->mlmode)
                    edit_move_end(l);
                if (l->need_refresh)
                    refresh_line(l);
                disable_raw_mode(l);
                raise(SIGTSTP);
                enable_raw_mode(l);
                l->maxrows = 0;
                l->dpos = dpos;
                l->need_refresh = 1;
            }
            break;
        case CTL('O'):
            restore_undo(l);
            break;
        }
    }
out:
    if (l->history_len > 0) {
        l->history_len--;
        lino_os.free_fn(l->history[l->history_len]);
        l->history[l->history_len] = 0;
        undo_subst_hist_idx(l, INT_MAX, 0);
        undo_renumber_hist_idx(l, -1);
    }
    return ret;
}

#ifdef SIGWINCH
static void sigwinch_handler(int sig)
{
    lino_t *li;

    if (lino_list_busy)
        return;

    for (li = lino_list.next; li != &lino_list; li = li->next)
        li->need_resize = 1;
}
#endif

/* The main function of the linenoise library
 * handles a non-TTY input file descriptor by opening
 * a standard I/O stream on it and reading lines
 * without any prompting. TTY input is handled using
 * the edit function.  */
wchar_t *linenoise(lino_t *ls, const wchar_t *prompt)
{
    int count;
    int ifd = lino_os.fileno_fn(ls->tty_ifs);

    if ( ls->noninteractive || !isatty(ifd)) {
        /* Not a tty: read from file / pipe. */
        if (lino_os.getl_fn(ls->tty_ifs, ls->data, nelem(ls->data)) == 0) {
            ls->error = (lino_os.eof_fn(ls->tty_ifs) ? lino_eof : lino_ioerr);
            return 0;
        }

        count = wcslen(ls->data);

        if (count && ls->data[count-1] == '\n')
            ls->data[count-1] = '\0';
        return lino_os.wstrdup_fn(ls->data);
    } else {
        wchar_t *ret = 0;
#ifdef SIGWINCH
        static struct sigaction blank;
        struct sigaction sa = blank, oa;
        sa.sa_handler = sigwinch_handler;
        sigaction(SIGWINCH, &sa, &oa);
#endif

        /* Interactive editing. */
        if (enable_raw_mode(ls) == -1)
            return 0;
        count = edit(ls, prompt);
        disable_raw_mode(ls);
        if (count != -1 || ls->error == lino_eof) {
            if (!lino_os.puts_fn(ls->tty_ofs, L"\n")) {
                ls->error = lino_ioerr;
                return 0;
            }
        }
        if (count == -1)
            return 0;
        ret = lino_os.wstrdup_fn(ls->data);

#ifdef SIGWINCH
        sigaction(SIGWINCH, &oa, 0);
#endif
        return ret;
    }
}

static void link_into_list(lino_t *list, lino_t *ls)
{
    lino_list_busy = 1;
    ls->prev = list;
    ls->next = list->next;
    list->next->prev = ls;
    list->next = ls;
    lino_list_busy = 0;
}

static void unlink_from_list(lino_t *ls)
{
    lino_list_busy = 1;
    ls->prev->next = ls->next;
    ls->next->prev = ls->prev;
    ls->next = ls->prev = 0;
    lino_list_busy = 0;
}

lino_t *lino_make(mem_t *ifs, mem_t *ofs)
{
    lino_t *ls = coerce(lino_t *, lino_os.alloc_fn(sizeof *ls));

    if (ls) {
        memset(ls, 0, sizeof *ls);
        ls->history_max_len = LINENOISE_DEFAULT_HISTORY_MAX_LEN;
        ls->tty_ifs = ifs;
        ls->tty_ofs = ofs;

        link_into_list(&lino_list, ls);
    }

    return ls;
}

lino_t *lino_copy(lino_t *le)
{
    lino_t *ls = coerce(lino_t *, lino_os.alloc_fn(sizeof *ls));

    if (ls != 0) {
        *ls = *le;
        ls->history_len = 0;
        ls->history = 0;
        ls->rawmode = 0;
        ls->clip = 0;
        ls->result = 0;
        ls->undo_stack = 0;

        link_into_list(&lino_list, ls);
    }

    return ls;
}


static void free_hist(lino_t *ls);

static void lino_cleanup(lino_t *ls)
{
    disable_raw_mode(ls);
    free_hist(ls);
    free_undo_stack(ls);
    lino_os.free_fn(ls->clip);
    ls->clip = 0;
    lino_os.free_fn(ls->result);
    ls->result = 0;
}

void lino_free(lino_t *ls)
{
    if (ls != 0) {
        unlink_from_list(ls);
        lino_cleanup(ls);
        lino_os.free_fn(ls);
    }
}

void lino_set_tempfile_suffix(lino_t *l, const char *suffix)
{
    l->suffix = suffix;
}

lino_error_t lino_get_error(lino_t *l)
{
    return l->error;
}

lino_error_t lino_set_error(lino_t *l, lino_error_t set)
{
    lino_error_t old = l->error;
    l->error = set;
    return old;
}

/* ================================ History ================================= */

/* Free the history, but does not reset it. Only used when we have to
 * exit() to avoid memory leaks are reported by valgrind & co. */
static void free_hist(lino_t *ls) {
    if (ls->history) {
        int j;

        for (j = 0; j < ls->history_len; j++) {
            lino_os.free_fn(ls->history[j]);
            ls->history[j] = 0;
        }
        lino_os.free_fn(ls->history);
        ls->history = 0;
    }
}

/* At exit we'll try to fix the terminal to the initial conditions. */
static void atexit_handler(void) {
    lino_t *ls;

    for (ls = lino_list.next; ls != &lino_list; ls = ls->next)
        lino_cleanup(ls);
}

/* This is the API call to add a new entry in the linenoise history.
 * It uses a fixed array of char pointers that are shifted (memmoved)
 * when the history max length is reached in order to remove the older
 * entry and make room for the new one, so it is not exactly suitable for huge
 * histories, but will work well for a few hundred of entries.
 *
 * Using a circular buffer is smarter, but a bit more complex to handle. */
int lino_hist_add(lino_t *ls, const wchar_t *line) {
    wchar_t *linecopy;

    if (ls->history_max_len == 0) return 0;

    /* Initialization on first call. */
    if (ls->history == NULL) {
        size_t size = ls->history_max_len * sizeof *ls->history;
        ls->history = coerce(wchar_t **, lino_os.alloc_fn(size));
        if (ls->history == NULL) return 0;
        memset(ls->history, 0, size);
    }

    /* Don't add duplicated lines, unless we are resubmitting historic lines. */
    if (ls->history_len && !ls->save_hist_idx &&
        !wcscmp(ls->history[ls->history_len-1], line))
        return 0;

    /* Add an heap allocated copy of the line in the history.
     * If we reached the max length, remove the older line. */
    linecopy = lino_os.wstrdup_fn(line);
    if (!linecopy) return 0;
    if (ls->history_len == ls->history_max_len) {
        lino_os.free_fn(ls->history[0]);
        memmove(ls->history,ls->history+1,(ls->history_max_len-1)*sizeof *ls->history);
        ls->history_len--;
    }
    ls->history[ls->history_len] = linecopy;
    ls->history_len++;
    undo_renumber_hist_idx(ls, 1);
    return 1;
}

/* Set the maximum length for the history. This function can be called even
 * if there is already some history, the function will make sure to retain
 * just the latest 'len' elements if the new history length value is smaller
 * than the amount of items already inside the history. */
int lino_hist_set_max_len(lino_t *ls, int len) {
    wchar_t **nsv;

    if (len < 1) return 0;
    if (ls->history) {
        int tocopy = ls->history_len;

        nsv = coerce(wchar_t **, lino_os.alloc_fn(len * sizeof *nsv));
        if (nsv == NULL) return 0;

        /* If we can't copy everything, free the elements we'll not use. */
        if (len < tocopy) {
            int j;

            for (j = 0; j < tocopy-len; j++)
                lino_os.free_fn(ls->history[j]);
            tocopy = len;
        }
        memset(nsv, 0, sizeof *nsv * len);
        memcpy(nsv, ls->history+(ls->history_len-tocopy), sizeof *ls->history * tocopy);
        lino_os.free_fn(ls->history);
        ls->history = nsv;
        ls->history_len = tocopy;
    }
    ls->history_max_len = len;
    return 1;
}

/* Save the history in the specified file. On success 0 is returned
 * otherwise -1 is returned. */
int lino_hist_save(lino_t *ls, const wchar_t *filename) {
    mem_t *fp = lino_os.open_fn(filename, L"w");
    int j;

    if (fp == NULL) {
        ls->error = lino_error;
        return -1;
    }

    for (j = 0; j < ls->history_len; j++) {
        lino_os.puts_fn(fp, ls->history[j]);
        lino_os.puts_fn(fp, L"\n");
    }

    lino_os.close_fn(fp);
    return 0;
}

/* Load the history from the specified file. If the file does not exist
 * zero is returned and no operation is performed.
 *
 * If the file exists and the operation succeeded 0 is returned, otherwise
 * on error -1 is returned. */
int lino_hist_load(lino_t *ls, const wchar_t *filename) {
    mem_t *fp = lino_os.open_fn(filename, L"r");
    wchar_t buf[LINENOISE_MAX_LINE];

    if (fp == NULL) {
        ls->error = lino_error;
        return -1;
    }

    while (lino_os.getl_fn(fp, buf, LINENOISE_MAX_LINE) != NULL) {
        wchar_t *p = wcschr(buf, '\n');
        if (p)
            *p = '\0';
        lino_hist_add(ls, buf);
    }

    lino_os.close_fn(fp);
    return 0;
}

void lino_set_result(lino_t *ls, wchar_t *res)
{
    lino_os.free_fn(ls->result);
    ls->result = res;
    while ((res = wcschr(res, '\n')) != 0)
        *res = '\r';
}

void lino_init(lino_os_t *os)
{
    lino_os = *os;
}
