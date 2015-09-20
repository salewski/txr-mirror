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
 * Copyright (c) 2010-2014, Salvatore Sanfilippo <antirez at gmail dot com>
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
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <signal.h>
#include <time.h>
#include "config.h"
#if HAVE_POLL
#include <poll.h>
#endif
#include "linenoise.h"

#define LINENOISE_DEFAULT_HISTORY_MAX_LEN 100
#define LINENOISE_MAX_LINE 1024
#define LINENOISE_MAX_DISP (LINENOISE_MAX_LINE * 8)
#define LINENOISE_PAREN_DELAY 400000

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
    struct termios orig_termios;        /* In order to restore at exit.*/
    int rawmode;        /* For atexit() function to check if restore is needed*/
    int mlmode;         /* Multi line mode. Default is single line. */
    int history_max_len;
    int history_len;
    char **history;
    char *clip;         /* Selection */
    int ifd;            /* Terminal stdin file descriptor. */
    int ofd;            /* Terminal stdout file descriptor. */

    /* Volatile state pertaining to just one linenoise call */
    char buf[LINENOISE_MAX_DISP];       /* Displayed line bufer. */
    char data[LINENOISE_MAX_LINE];      /* True data corresponding to display */
    const char *prompt; /* Prompt to display. */
    const char *suffix; /* Suffix when creating temp file. */
    size_t plen;        /* Prompt length. */
    size_t pos;         /* Current cursor position. */
    size_t sel;         /* Selection start in terms of display. */
    size_t end;         /* Selection end in terms of display. */
    size_t len;         /* Current edited line display length. */
    size_t dlen;        /* True underlying length. */
    size_t dpos;        /* True underlying position. */
    size_t dsel;        /* Start of selection */
    size_t dend;        /* End of selection */
    size_t cols;        /* Number of columns in terminal. */
    size_t oldrow;      /* Row of previous cursor position (multiline mode) */
    size_t maxrows;     /* Maximum num of rows used so far (multiline mode) */
    int history_index;  /* The history index we are currently editing. */
    int need_resize;    /* Need resize flag. */
    int need_refresh;   /* Need refresh. */
    int selmode;        /* Visual selection being made. */
    lino_error_t error; /* Most recent error. */
};

#define CTL(LETTER) ((LETTER) - '@')

enum key_action {
    TAB = 9,
    ENTER = 13,
    ESC = 27,
    BACKSPACE =  127
};

#define SPACE "\t "

typedef unsigned char mem_t;
mem_t *chk_malloc(size_t n);
mem_t *chk_realloc(mem_t *old, size_t size);
char *chk_strdup_utf8(const char *str);

static lino_t lino_list = { &lino_list, &lino_list };
volatile sig_atomic_t lino_list_busy;
static int atexit_registered = 0; /* Register atexit just 1 time. */

/* ======================= Low level terminal handling ====================== */

/* Set if to use or not the multi line mode. */
void lino_set_multiline(lino_t *ls, int ml) {
    ls->mlmode = ml;
}

int lino_get_multiline(lino_t *ls) {
    return ls->mlmode;
}

void lino_set_atom_cb(lino_t *l, lino_atom_cb_t *cb, void *ctx)
{
    l->atom_callback = cb;
    l->ca_ctx = ctx;
}

static void atexit_handler(void);

/* Raw mode: 1960 magic shit. */
static int enable_raw_mode(lino_t *ls) {
    struct termios raw;

    if (!isatty(ls->ifd)) goto fatal;

    if (!atexit_registered) {
        atexit(atexit_handler);
        atexit_registered = 1;
    }

    if (tcgetattr(ls->ifd,&ls->orig_termios) == -1) goto fatal;

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
    if (tcsetattr(ls->ifd,TCSANOW,&raw) < 0) goto fatal;
    ls->rawmode = 1;
    return 0;

fatal:
    ls->error = lino_notty;
    return -1;
}

static void disable_raw_mode(lino_t *ls) {
    /* Don't even check the return value as it's too late. */
    if (ls->rawmode && tcsetattr(ls->ifd,TCSANOW,&ls->orig_termios) != -1)
        ls->rawmode = 0;
}

/* Use the ESC [6n escape sequence to query the horizontal cursor position
 * and return it. On error -1 is returned, on success the position of the
 * cursor. */
static int get_cursor_position(int ifd, int ofd) {
    char buf[32];
    int cols, rows;
    unsigned int i = 0;

    /* Report cursor location */
    if (write(ofd, "\x1b[6n", 4) != 4) return -1;

    /* Read the response: ESC [ rows ; cols R */
    while (i < sizeof(buf)-1) {
        if (read(ifd,buf+i,1) != 1) break;
        if (buf[i] == 'R') break;
        i++;
    }
    buf[i] = '\0';

    /* Parse it. */
    if (buf[0] != ESC || buf[1] != '[') return -1;
    if (sscanf(buf+2,"%d;%d",&rows,&cols) != 2) return -1;
    return cols;
}

/* Try to get the number of columns in the current terminal, or assume 80
 * if it fails. */
static int get_columns(int ifd, int ofd) {
#if HAVE_WINSIZE
    struct winsize ws;

    if (ioctl(1, TIOCGWINSZ, &ws) == 0 && ws.ws_col != 0)
        return ws.ws_col;
#endif

    {
        /* ioctl() failed or we don't have struct winsize.
           Try to query the terminal itself. */
        int start, cols;

        /* Get the initial position so we can restore it later. */
        start = get_cursor_position(ifd,ofd);
        if (start == -1) goto failed;

        /* Go to right margin and get position. */
        if (write(ofd,"\x1b[999C",6) != 6) goto failed;
        cols = get_cursor_position(ifd,ofd);
        if (cols == -1) goto failed;

        /* Restore position. */
        if (cols > start) {
            char seq[32];
            snprintf(seq,32,"\x1b[%dD",cols-start);
            if (write(ofd,seq,strlen(seq)) == -1) {
                /* Can't recover... */
            }
        }
        return cols;
    }

failed:
    return 80;
}

/* Clear the screen. Used to handle ctrl+l */
int lino_clear_screen(lino_t *ls) {
    return (write(ls->ofd,"\x1b[H\x1b[2J",7) > 0);
}

static void refresh_line(lino_t *l);

static void handle_resize(lino_t *ls, lino_t *lc)
{
    if (ls->need_resize) {
        ls->need_resize = 0;
        if (lc != 0)
            lc->need_resize = 0;
        ls->cols = get_columns(ls->ifd, ls->ofd);
        if (lc)
            lc->cols = ls->cols;
        refresh_line(ls);
    }
}

/* Beep, used for completion when there is nothing to complete or when all
 * the choices were already shown. */
static int generate_beep(lino_t *ls) {
    return write(ls->ofd, "\x7", 1) > 0;
}

/* ============================== Completion ================================ */

/* Free a list of completion option populated by lino_add_completion(). */
static void free_completions(lino_completions_t *lc) {
    size_t i;
    for (i = 0; i < lc->len; i++)
        free(lc->cvec[i]);
    if (lc->cvec != NULL)
        free(lc->cvec);
}

static void sync_data_to_buf(lino_t *l);

static int compare_completions(const void *larg, const void *rarg)
{
    const char **lelem = (const char **) larg;
    const char **relem = (const char **) rarg;
    const char *lstr = *lelem, *rstr = *relem;
    size_t llen = strlen(lstr);
    size_t rlen = strlen(rstr);

    if (llen < rlen)
        return -1;
    if (llen > rlen)
        return 1;

    return strcmp(lstr, rstr);
}

/* This is an helper function for edit() and is called when the
 * user types the <tab> key in order to complete the string currently in the
 * input.
 *
 * The state of the editing is encapsulated into the pointed lino_state
 * structure as described in the structure definition. */
static int complete_line(lino_t *ls) {
    lino_completions_t lc = { 0, NULL };
    int nread;
    char c = 0;
    char save = ls->data[ls->dpos];
    lino_t *lt = lino_copy(ls);

    if (lt == 0)
        return -1;

    ls->data[ls->dpos] = 0;
    ls->completion_callback(ls->data, &lc, ls->cb_ctx);
    ls->data[ls->dpos] = save;

    qsort(lc.cvec, lc.len, sizeof *lc.cvec, compare_completions);

    if (lc.len == 0) {
        generate_beep(ls);
    } else {
        size_t stop = 0, i = 0;

        while(!stop) {
            /* Show completion or original buffer */
            if (i < lc.len) {
                size_t n = snprintf(lt->data, sizeof lt->data,
                                    "%s%s", lc.cvec[i], ls->data + ls->dpos);
                lt->dlen = n;
                lt->dpos = strlen(lc.cvec[i]);
                sync_data_to_buf(lt);
                refresh_line(lt);
            } else {
                refresh_line(ls);
            }

            nread = read(ls->ifd,&c,1);

            if (nread <= 0) {
                if (errno == EINTR) {
                    handle_resize(ls, lt);
                    continue;
                }
                free_completions(&lc);
                free(lt);
                ls->error = (nread < 0 ? lino_ioerr : lino_eof);
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
                        strcpy(ls->data, lt->data);
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
void lino_add_completion(lino_completions_t *lc, const char *str) {
    size_t len = strlen(str);
    char *copy, **cvec;

    copy = (char *) chk_malloc(len+1);
    if (copy == NULL) return;
    memcpy(copy,str,len+1);
    cvec = (char **) chk_realloc((mem_t *) lc->cvec,
                                 (lc->len+1) * sizeof *cvec);
    if (cvec == NULL) {
        free(copy);
        return;
    }
    lc->cvec = cvec;
    lc->cvec[lc->len++] = copy;
}

static int next_hist_match(lino_t *l, char *pat, int cur, size_t *offs)
{
    int i;

    if (cur >= l->history_len)
        cur = l->history_len - 1;

    for (i = cur; i >= 0; i--) {
        char *hline = l->history[i];
        char *pmatch = strstr(hline, pat);
        if (pmatch != 0) {
            *offs = pmatch - hline;
            return i;
        }
    }
    return -1;
}

static int history_search(lino_t *l)
{
    char hpat[128] = "";
    int hi = l->history_len - l->history_index - 2 + (l->history_index == 0);
    int hp = hi, hl = 0, stop = 0;
    size_t dp = l->dpos;
    const char *fmt = "[%s]%s";
    size_t ex = strlen(fmt) - 2*strlen("%s");
    lino_t *lc = lino_copy(l), *ld = lino_copy(l);
    int c = -1;

    if (lc == 0 || ld == 0)
        goto out;

    lc->prompt = "search:";

    while (!stop) {
        size_t nw = snprintf(lc->data, sizeof lc->data, fmt, hpat, l->data);
        int vb = 0;
        lc->dlen = nw;
        lc->dpos = dp + hl + ex;

        if (lc->dpos > lc->dlen)
            lc->dpos = lc->dlen;

        refresh_line(lc);

        for (;;) {
            unsigned char byte;
            int nread = read(lc->ifd, &byte, 1);

            if (nread <= 0) {
                if (errno == EINTR) {
                    handle_resize(lc, l);
                    continue;
                }
                c = nread;
                stop = 1;
            } else {
                c = byte;

                if (vb)
                    goto verbatim;

                switch (c) {
                default:
                    if (c < 32)
                        continue;
                verbatim:
                    if (hl >= sizeof hpat)
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
                                 strcmp(l->history[hi], l->history[ni]) == 0);
                        if (ni < 0)
                            break;

                        hi = ni;
                        strcpy(l->data, l->history[hi]);
                        l->dpos = l->dlen = strlen(l->data);
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
                    strcpy(l->data, ld->data);
                    l->dpos = ld->dpos;
                    l->dlen = ld->dlen;
                    stop = 1;
                    c = 0;
                    break;
                case CTL('F'): case CTL('B'):
                case CTL('A'): case CTL('E'):
                case CTL('N'): case CTL('P'):
                case CTL('D'): case CTL('U'): case CTL('W'): case CTL('K'):
                case CTL('T'):
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
                    lino_clear_screen(l);
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
    lino_free(lc);
    lino_free(ld);
    refresh_line(l);
    return c;
}

/* =========================== Line editing ================================= */

/* We define a very simple "append buffer" structure, that is an heap
 * allocated string where we can append to. This is useful in order to
 * write all the escape sequences in a buffer and flush them to the standard
 * output in a single call, to avoid flickering effects. */
struct abuf {
    char *b;
    int len;
};

static void ab_init(struct abuf *ab) {
    ab->b = NULL;
    ab->len = 0;
}

static void ab_append(struct abuf *ab, const char *s, int len) {
    char *ns = (char *) chk_realloc((mem_t *) ab->b,ab->len+len);

    if (ns == NULL) return;
    memcpy(ns+ab->len,s,len);
    ab->b = ns;
    ab->len += len;
}

static void ab_free(struct abuf *ab) {
    free(ab->b);
}

/* Convert raw data to display data, and recalculate
   display length and position. */
static void sync_data_to_buf(lino_t *l)
{
    char *dptr = l->data, *bptr = l->buf;

    if (l->mlmode)
        bptr += snprintf(l->buf, sizeof l->buf, "%s",
                         l->prompt);

    while (bptr - l->buf < sizeof l->buf - 1) {
        size_t dpos = dptr - l->data;
        size_t pos = bptr - l->buf;

        if (l->dpos == dpos)
            l->pos = pos;
        if (l->dsel == dpos)
            l->sel = pos;
        if (l->dend == dpos)
            l->end = pos;

        if (*dptr)  {
            char ch = *dptr++;

            if (ch == TAB) {
                int pos = bptr - l->buf;
                do {
                    *bptr++ = ' ';
                    pos++;
                } while (pos % 8 != 0);
            } else if (l->mlmode && ch == '\r') {
                *bptr++ = '\r';
                *bptr++ = '\n';
            } else if (ch < ' ') {
                *bptr++ = '^';
                *bptr++ = '@' + ch;
            } else if (ch == 127) {
                *bptr++ = '^';
                *bptr++ = '?';
            } else {
                *bptr++ = ch;
            }

            continue;
        }
        break;
    }

    l->len = bptr - l->buf;
    *bptr++ = 0;
}

/* Single line low level line refresh.
 *
 * Rewrite the currently edited line accordingly to the buffer content,
 * cursor position, and number of columns of the terminal. */
static void refresh_singleline(lino_t *l) {
    char seq[64];
    size_t plen = strlen(l->prompt);
    int fd = l->ofd;
    char *buf = l->buf;
    size_t len = l->len;
    size_t pos = l->pos;
    struct abuf ab;
    size_t sel = l->sel;
    size_t end = l->end;

    if (sel > end) {
        size_t tmp = end;
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
    snprintf(seq,64,"\r");
    ab_append(&ab,seq,strlen(seq));
    /* Write the prompt and the current buffer content */
    ab_append(&ab,l->prompt,strlen(l->prompt));
    if (!l->selmode) {
        ab_append(&ab, buf, len);
    } else {
        if (sel > 0)
            ab_append(&ab, buf, sel);
        if (end - sel > 0) {
            ab_append(&ab, "\x1b[7m", 4);
            ab_append(&ab, buf + sel, end - sel);
            ab_append(&ab, "\x1b[m", 3);
        }
        if (len - end > 0)
            ab_append(&ab, buf + end, len - end);
    }

    /* Erase to right */
    snprintf(seq,64,"\x1b[0K");
    ab_append(&ab,seq,strlen(seq));
    /* Move cursor to original position. */
    snprintf(seq,64,"\r\x1b[%dC", (int)(pos+plen));
    ab_append(&ab,seq,strlen(seq));
    if (write(fd,ab.b,ab.len) == -1) {} /* Can't recover from write error. */
    ab_free(&ab);
}

struct row_values {
    int rows[2];
};

static struct row_values screen_rows(const char *str, size_t pos, int cols)
{
    const char *start = str;
    int ch = *str;
    struct row_values out = { { 1, 1 } };
    int col = 1;

    if (!ch)
        return out;

    for (; ; ch = *++str, col++) {
        if (str - start == pos)
            out.rows[1] = out.rows[0];
        if (ch == 0)
            break;
        if (ch == '\r')
            continue;
        if (ch == '\n' || col == cols) {
            col = 0;
            out.rows[0]++;
        }
    }

    return out;
}

static int col_offset_in_str(const char *str, size_t pos)
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
    char seq[64];
    struct row_values r = screen_rows(l->buf, l->pos, l->cols);
    int rows = r.rows[0]; /* rows used by current buf. */
    int nrow = r.rows[1]; /* cursor row after refresh. */
    int col; /* colum position, zero-based. */
    int oldmaxrows = l->maxrows;
    int fd = l->ofd, j;
    struct abuf ab;

    /* Update maxrows if needed. */
    if (rows > (int)l->maxrows) l->maxrows = rows;

    /* First step: clear all the lines used before. To do so start by
     * going to the last row. */
    ab_init(&ab);
    if (oldmaxrows - l->oldrow > 0) {
        snprintf(seq,64,"\x1b[%dB", oldmaxrows - l->oldrow);
        ab_append(&ab,seq,strlen(seq));
    }

    /* Now for every row clear it, go up. */
    for (j = 0; j < oldmaxrows - 1; j++) {
        snprintf(seq,64,"\r\x1b[0K\x1b[1A");
        ab_append(&ab,seq,strlen(seq));
    }

    /* Clean the top line. */
    snprintf(seq,64,"\r\x1b[0K");
    ab_append(&ab,seq,strlen(seq));

    /* Write the current buffer content which includes the prompt */
    if (!l->selmode) {
        ab_append(&ab,l->buf,l->len);
    } else {
        size_t sel = l->sel;
        size_t end = l->end;
        size_t len = l->len;

        if (sel > end) {
            size_t tmp = end;
            end = sel;
            sel = tmp;
        }

        if (sel > 0)
            ab_append(&ab, l->buf, sel);
        if (end - sel > 0) {
            ab_append(&ab, "\x1b[7m", 4);
            ab_append(&ab, l->buf + sel, end - sel);
            ab_append(&ab, "\x1b[m", 3);
        }
        if (len - end > 0)
            ab_append(&ab, l->buf + end, len - end);
    }

    /* If we are at the very end of the screen with our prompt, we need to
     * emit a newline and move the cursor to the first column. */

    col = col_offset_in_str(l->buf, l->pos);

    if (l->pos && l->pos == l->len && col % l->cols == l->cols - 1)
    {
        ab_append(&ab,"\n",1);
        snprintf(seq,64,"\r");
        ab_append(&ab,seq,strlen(seq));
        rows++;
        if (rows > (int)l->maxrows) l->maxrows = rows;
    }

    /* Go up till we reach the expected positon. */
    if (rows - nrow > 0) {
        snprintf(seq,64,"\x1b[%dA", rows - nrow);
        ab_append(&ab,seq,strlen(seq));
    }

    /* Set column. */
    if (col)
        snprintf(seq,64,"\r\x1b[%dC", col);
    else
        snprintf(seq,64,"\r");
    ab_append(&ab,seq,strlen(seq));

    /* Remember current cursor row for next time */
    l->oldrow = nrow;

    if (write(fd,ab.b,ab.len) == -1) {} /* Can't recover from write error. */
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

static size_t scan_match_rev(const char *s, size_t i, int mch)
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

static size_t scan_rev(const char *s, size_t i)
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

static void usec_delay(lino_t *l, long usec)
{
#if HAVE_POLL
    struct pollfd pfd;
    pfd.fd = l->ifd;
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
    size_t pos = scan_rev(l->data, l->dpos - 1);

    if (pos != -1) {
        size_t dp = l->dpos;
        l->dpos = pos;
        refresh_line(l);
        usec_delay(l, LINENOISE_PAREN_DELAY);
        l->dpos = dp;
        l->need_refresh = 1;
    }
}

static void update_sel(lino_t *l)
{
    if (l->selmode) {
        size_t oend = l->dend;
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
        size_t sel = notrev ? l->dsel : l->dend;
        size_t end = notrev ? l->dend : l->dsel;

        if (end - sel > 0) {
            free(l->clip);
            l->clip = (char *) chk_malloc(end - sel + 1);
            memcpy(l->clip, l->data + sel, end - sel);
            l->clip[end - sel] = 0;
            l->dpos = sel;
        }
    }
}

static void delete_sel(lino_t *l)
{
    if (l->selmode) {
        int notrev = l->dsel <= l->dend;
        size_t sel = notrev ? l->dsel : l->dend;
        size_t end = notrev ? l->dend : l->dsel;
        size_t len = l->dlen;

        if (len - end > 0)
            memmove(l->data + sel, l->data + end, len - end);

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
static int edit_insert(lino_t *l, char c) {
    if (l->dlen < sizeof l->data - 1) {
        delete_sel(l);
        if (l->dpos == l->dlen) {
            l->data[l->dpos] = c;
            l->dpos++;
            l->dlen++;
            l->data[l->dlen] = '\0';
            if ((!l->mlmode && l->len == l->dlen && l->plen+l->len < l->cols) /* || mlmode */) {
                /* Avoid a full update of the line in the
                 * trivial case. */
                if (write(l->ofd,&c,1) == -1) return -1;
            } else {
                l->need_refresh = 1;
            }
        } else {
            memmove(l->data + l->dpos+1, l->data + l->dpos, l->dlen-l->dpos);
            l->data[l->dpos] = c;
            l->dlen++;
            l->dpos++;
            l->data[l->dlen] = '\0';
            l->need_refresh = 1;
        }
    }
    return 0;
}

static int edit_insert_str(lino_t *l, const char *s, size_t nchar)
{
    if (l->dlen < sizeof l->data - nchar) {
        delete_sel(l);

        if (l->dpos < l->dlen)
            memmove(l->data + l->dpos + nchar, l->data + l->dpos, l->dlen - l->dpos);
        memcpy(l->data + l->dpos, s, nchar);
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

/* Move cursor to the start of the line. */
static void edit_move_home(lino_t *l) {
    if (l->dpos != 0) {
        l->dpos = 0;
        l->need_refresh = 1;
    }
}

/* Move cursor to the end of the line. */
static void edit_move_end(lino_t *l) {
    if (l->dpos != l->dlen) {
        l->dpos = l->dlen;
        l->need_refresh = 1;
    }
}

/* Substitute the currently edited line with the next or previous history
 * entry as specified by 'dir'. */
#define LINENOISE_HISTORY_NEXT 0
#define LINENOISE_HISTORY_PREV 1
static void edit_history_next(lino_t *l, int dir) {
    clear_sel(l);
    if (l->history_len > 1) {
        /* Update the current history entry before to
         * overwrite it with the next one. */
        free(l->history[l->history_len - 1 - l->history_index]);
        l->history[l->history_len - 1 - l->history_index] = chk_strdup_utf8(l->data);
        /* Show the new entry */
        l->history_index += (dir == LINENOISE_HISTORY_PREV) ? 1 : -1;
        if (l->history_index < 0) {
            l->history_index = 0;
            return;
        } else if (l->history_index >= l->history_len) {
            l->history_index = l->history_len-1;
            return;
        }
        strncpy(l->data,l->history[l->history_len - 1 - l->history_index], sizeof l->data);
        l->data[sizeof l->data - 1] = 0;
        l->dpos = l->dlen = strlen(l->data);
        l->need_refresh = 1;
    }
}

/* Delete the character at the right of the cursor without altering the cursor
 * position. Basically this is what happens with the "Delete" keyboard key. */
static void edit_delete(lino_t *l) {
    if (l->selmode) {
        delete_sel(l);
        return;
    }

    if (l->dlen > 0 && l->dpos < l->dlen) {
        memmove(l->data + l->dpos, l->data + l->dpos + 1, l->dlen - l->dpos - 1);
        l->dlen--;
        l->data[l->dlen] = '\0';
        l->need_refresh = 1;
    }
}

/* Backspace implementation. */
static void edit_backspace(lino_t *l) {
    if (l->selmode && l->dend > l->dsel) {
        delete_sel(l);
        return;
    }

    delete_sel(l);

    if (l->dpos > 0 && l->dlen > 0) {
        memmove(l->data + l->dpos - 1, l->data + l->dpos, l->dlen - l->dpos);
        l->dpos--;
        l->dlen--;
        l->data[l->dlen] = '\0';
        l->need_refresh = 1;
    }
}

/* Delete all characters to left of cursor. */
static void edit_delete_prev_all(lino_t *l)
{
    delete_sel(l);

    memmove(l->data, l->data + l->dpos, l->dlen - l->dpos + 1);
    l->dlen -= l->dpos;
    l->dpos = 0;
    l->need_refresh = 1;
}

/* Delete the previosu word, maintaining the cursor at the start of the
 * current word. */
static void edit_delete_prev_word(lino_t *l) {
    size_t odpos, diff;

    delete_sel(l);

    odpos = l->dpos;
    while (l->dpos > 0 && strchr(SPACE, l->data[l->dpos - 1]))
        l->dpos--;
    while (l->dpos > 0 && strchr(SPACE, l->data[l->dpos - 1]) == 0)
        l->dpos--;
    diff = odpos - l->dpos;
    memmove(l->data + l->dpos, l->data + odpos, l->dlen - odpos + 1);
    l->dlen -= diff;
    l->need_refresh = 1;
}

static void tr(char *s, int find, int rep)
{
    for (; *s; s++)
        if (*s == find)
            *s = rep;
}

static void edit_in_editor(lino_t *l) {
    char *template = ".linotmpXXXXXX";
    FILE *fo = 0;
    char *ed = getenv("EDITOR");
    char path[128];

    if (ed) {
        char *ho = getenv("HOME");
        int fd;
#if HAVE_MKSTEMPS
        const char *suffix = l->suffix ? l->suffix : "";
#else
        const char *suffix = "";
#endif

        if (ho)
            snprintf(path, sizeof path, "%s/%s%s", ho, template, suffix);
        else
            snprintf(path, sizeof path, "%s%s", template, suffix);

#if HAVE_MKSTEMPS
        if ((fd = mkstemps(path, strlen(suffix))) != -1)
            fo = fdopen(fd, "w");
#else
        if ((fd = mkstemp(path)) != -1)
            fo = fdopen(fd, "w");
#endif

        if (!fo && fd != -1)
            close(fd);
    }

    if (fo) {
        char cmd[256];
        snprintf(cmd, sizeof cmd, "%s %s", ed, path);
        tr(l->data, '\r', '\n');
        if (fputs(l->data, fo) != EOF && putc('\n', fo) != EOF &&
            fflush(fo) == 0)
        {
            FILE *fi;
            int nread;

            if (system(cmd) == 0 && (fi = fopen(path, "r")) != 0) {
                nread = fread(l->data, 1, sizeof l->data - 1, fi);
                fclose(fi);
                l->data[nread] = 0;
                if (nread > 0 && l->data[nread - 1] == '\n')
                    l->data[--nread] = 0;
                l->dpos = l->dlen = nread;
                tr(l->data, '\n', '\r');
                l->need_refresh = 1;
            }
        }

        fclose(fo);
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
static int edit(lino_t *l, const char *prompt)
{
    int verbatim = 0, extended = 0, paste = 0, extend_num = -1;

    /* Populate the linenoise state that we pass to functions implementing
     * specific editing functionalities. */
    l->prompt = prompt;
    l->plen = strlen(prompt);
    l->pos = l->len = 0;
    l->dpos = l->dlen = 0;
    l->cols = get_columns(l->ifd, l->ofd);
    l->oldrow = l->maxrows = 0;
    l->history_index = 0;
    clear_sel(l);

    /* Buffer starts empty. */
    l->data[0] = '\0';

    /* The latest history entry is always our current buffer, that
     * initially is just an empty string. */
    lino_hist_add(l, "");

    if (write(l->ofd,prompt,l->plen) == -1) {
        l->error = lino_ioerr;
        return -1;
    }
    while(1) {
        unsigned char byte;
        int c;
        int nread;
        char seq[3];

        update_sel(l);

        if (l->need_refresh) {
            l->need_refresh = 0;
            refresh_line(l);
        }

        nread = read(l->ifd,&byte,1);

        if (nread < 0 && errno == EINTR) {
            handle_resize(l, 0);
            continue;
        }

        if (nread <= 0)
            return l->len ? (int) l->len : -1;

        c = byte;

        if (paste && (c == CTL('X'))) {
            paste = 0;
            continue;
        }

        if (verbatim ||
            (paste && c != ESC && c != BACKSPACE && c != CTL('H')))
        {
            if (edit_insert(l,c)) {
                l->error = lino_ioerr;
                return -1;
            }
            verbatim = 0;
            continue;
        }

        if (extended) {
            switch (c) {
            case CTL('E'):
                extended = 0;
                edit_in_editor(l);
                break;
            case CTL('V'):
                extended = 0;
                paste = 1;
                break;
            case CTL('W'): case 'w':
                extended = 0;
                if (l->history_len > 1 && extend_num != 0) {
                    char *prev_line = l->history[l->history_len - 2];
                    char *word_end = prev_line + strlen(prev_line);
                    char *word_start = word_end;

                    if (extend_num < 0)
                        extend_num = 1;

                    for (; extend_num--; word_end = word_start) {
                        while (word_end > prev_line && isspace((unsigned char) word_end[-1]))
                            word_end--;

                        word_start = word_end;

                        while (word_start > prev_line && !isspace((unsigned char) word_start[-1]))
                            word_start--;

                        if (extend_num == 0)
                            break;
                    }

                    if (edit_insert_str(l, word_start, word_end - word_start)) {
                        l->error = lino_ioerr;
                        return -1;
                    }
                }
                break;
            case CTL('A'): case 'a':
                extended = 0;
                if (extend_num < 0)
                    extend_num = 1;
                if (l->history_len > 1 && l->atom_callback)
                {
                    char *prev_line = l->history[l->history_len - 2];
                    char *word = l->atom_callback(l, prev_line,
                                                  extend_num, l->ca_ctx);
                    int res = 0;

                    if (word != 0) {
                        res = edit_insert_str(l, word, strlen(word));
                        free(word);
                    }

                    if (res) {
                        l->error = lino_ioerr;
                        return -1;
                    }
                }
                break;
            case CTL('Q'):
                extended = 0;
                {
                    char *clip = l->clip;
                    l->clip = 0;
                    yank_sel(l);
                    if (clip != 0) {
                        edit_insert_str(l, clip, strlen(clip));
                        free(clip);
                    }
                    clear_sel(l);
                }
                break;
            default:
                if (isdigit((unsigned char) c)) {
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
                clear_sel(l);
                c = complete_line(l);
            }
            break;
        case CTL('R'):
            clear_sel(l);
            c = history_search(l);
            break;
        }

        if (c < 0)
            return l->len;
        if (c == 0)
            continue;

        switch(c) {
        case ENTER:
            if (paste) {
                if (edit_insert(l,c)) {
                    l->error = lino_ioerr;
                    return -1;
                }
                break;
            }
            if (l->history_len > 0) {
                l->history_len--;
                free(l->history[l->history_len]);
                l->history[l->history_len] = 0;
            }
            if (l->mlmode) edit_move_end(l);
            if (l->need_refresh)
                refresh_line(l);
            return (int)l->len;
        case CTL('C'):
            l->error = lino_intr;
            return -1;
        case BACKSPACE:   /* backspace */
        case CTL('H'):
            edit_backspace(l);
            break;
        case CTL('D'):   /* remove char at right of cursor, or if the
                            line is empty, act as end-of-file. */
            if (l->dlen > 0) {
                yank_sel(l);
                edit_delete(l);
            } else {
                if (l->history_len > 0) {
                    l->history_len--;
                    free(l->history[l->history_len]);
                    l->history[l->history_len] = 0;
                }
                l->error = lino_eof;
                return -1;
            }
            break;
        case CTL('T'):   /* swaps current character with previous. */
            if (l->dpos > 0 && l->dpos < l->dlen) {
                int aux = l->data[l->dpos - 1];
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
            /* Read the next two bytes representing the escape sequence.
             * Use two calls to handle slow terminals returning the two
             * chars at different times. */
            if (read(l->ifd,seq,1) == -1) break;
            if (read(l->ifd,seq+1,1) == -1) break;

            /* ESC [ sequences. */
            if (seq[0] == '[') {
                if (seq[1] >= '0' && seq[1] <= '9') {
                    /* Extended escape, read additional byte. */
                    if (read(l->ifd,seq+2,1) == -1) break;
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
                        edit_move_home(l);
                        break;
                    case 'F': /* End*/
                        edit_move_end(l);
                        break;
                    }
                }
            }

            /* ESC O sequences. */
            else if (seq[0] == 'O') {
                switch(seq[1]) {
                case 'H': /* Home */
                    edit_move_home(l);
                    break;
                case 'F': /* End*/
                    edit_move_end(l);
                    break;
                }
            }
            break;
        case ')': case ']': case '}':
            if (edit_insert(l,c)) {
                l->error = lino_ioerr;
                return -1;
            }
            paren_jump(l);
            break;
        default:
            if (c < 32)
                break;
            if (edit_insert(l,c)) {
                l->error = lino_ioerr;
                return -1;
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
        case CTL('Y'):
            yank_sel(l);
            clear_sel(l);
            break;
        case CTL('Q'):
            if (l->clip != 0)
                edit_insert_str(l, l->clip, strlen(l->clip));
            break;
        case CTL('K'): /* delete from current to end of line. */
            l->data[l->dpos] = '\0';
            l->dlen = l->dpos;
            l->need_refresh = 1;
            break;
        case CTL('A'):
            edit_move_home(l);
            break;
        case CTL('E'):
            edit_move_end(l);
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
                snprintf(l->buf, sizeof l->buf, "%s", l->prompt);
                l->pos = l->len = strlen(l->buf);
                refresh_multiline(l);
            }

            l->mlmode ^= 1;
            l->need_refresh = 1;
            break;
        case CTL('Z'):
            disable_raw_mode(l);
            raise(SIGTSTP);
            enable_raw_mode(l);
            l->need_refresh = 1;
            break;
        }
    }
    return l->len;
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
char *linenoise(lino_t *ls, const char *prompt)
{
    int count;

    if (!isatty(ls->ifd)) {
        int fd = dup(ls->ifd);
        FILE *fi = (fd > 0) ? fdopen(fd, "r") : 0;

        if (!fi) {
            ls->error = lino_error;
            if (fd > 0)
                close(fd);
            return 0;
        }

        /* Not a tty: read from file / pipe. */
        if (fgets(ls->data, sizeof ls->data, fi) == NULL) {
            ls->error = (ferror(fi) ? lino_ioerr : lino_eof);
            fclose(fi);
            return 0;
        }

        fclose(fi);
        count = strlen(ls->data);

        if (count && ls->data[count-1] == '\n')
            ls->data[count-1] = '\0';
        return chk_strdup_utf8(ls->data);
    } else {
        char *ret = 0;
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
            char nl = '\n';
            if (write(ls->ofd, &nl, 1) < 0)
                return 0;
        }
        if (count == -1)
            return 0;
        ret = chk_strdup_utf8(ls->data);

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

lino_t *lino_make(int ifd, int ofd)
{
    lino_t *ls = (lino_t *) chk_malloc(sizeof *ls);

    if (ls) {
        memset(ls, 0, sizeof *ls);
        ls->history_max_len = LINENOISE_DEFAULT_HISTORY_MAX_LEN;
        ls->ifd = ifd;
        ls->ofd = ofd;

        link_into_list(&lino_list, ls);
    }

    return ls;
}

lino_t *lino_copy(lino_t *le)
{
    lino_t *ls = (lino_t *) chk_malloc(sizeof *ls);

    if (ls != 0) {
        *ls = *le;
        ls->history_len = 0;
        ls->history = 0;
        ls->rawmode = 0;
        ls->clip = 0;

        link_into_list(&lino_list, ls);
    }

    return ls;
}


static void free_hist(lino_t *ls);

static void lino_cleanup(lino_t *ls)
{
    disable_raw_mode(ls);
    free_hist(ls);
    free(ls->clip);
    ls->clip = 0;
}

void lino_free(lino_t *ls)
{
    if (ls != 0) {
        unlink_from_list(ls);
        lino_cleanup(ls);
        free(ls);
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
            free(ls->history[j]);
            ls->history[j] = 0;
        }
        free(ls->history);
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
int lino_hist_add(lino_t *ls, const char *line) {
    char *linecopy;

    if (ls->history_max_len == 0) return 0;

    /* Initialization on first call. */
    if (ls->history == NULL) {
        size_t size = ls->history_max_len * sizeof *ls->history;
        ls->history = (char **) chk_malloc(size);
        if (ls->history == NULL) return 0;
        memset(ls->history, 0, size);
    }

    /* Don't add duplicated lines. */
    if (ls->history_len && !strcmp(ls->history[ls->history_len-1], line)) return 0;

    /* Add an heap allocated copy of the line in the history.
     * If we reached the max length, remove the older line. */
    linecopy = chk_strdup_utf8(line);
    if (!linecopy) return 0;
    if (ls->history_len == ls->history_max_len) {
        free(ls->history[0]);
        memmove(ls->history,ls->history+1,(ls->history_max_len-1)*sizeof *ls->history);
        ls->history_len--;
    }
    ls->history[ls->history_len] = linecopy;
    ls->history_len++;
    return 1;
}

/* Set the maximum length for the history. This function can be called even
 * if there is already some history, the function will make sure to retain
 * just the latest 'len' elements if the new history length value is smaller
 * than the amount of items already inside the history. */
int lino_hist_set_max_len(lino_t *ls, int len) {
    char **nsv;

    if (len < 1) return 0;
    if (ls->history) {
        int tocopy = ls->history_len;

        nsv = (char **) chk_malloc(len * sizeof *nsv);
        if (nsv == NULL) return 0;

        /* If we can't copy everything, free the elements we'll not use. */
        if (len < tocopy) {
            int j;

            for (j = 0; j < tocopy-len; j++) free(ls->history[j]);
            tocopy = len;
        }
        memset(nsv,0,sizeof(char*)*len);
        memcpy(nsv,ls->history+(ls->history_len-tocopy), sizeof *ls->history * tocopy);
        free(ls->history);
        ls->history = nsv;
        ls->history_len = tocopy;
    }
    ls->history_max_len = len;
    return 1;
}

/* Save the history in the specified file. On success 0 is returned
 * otherwise -1 is returned. */
int lino_hist_save(lino_t *ls, const char *filename) {
    FILE *fp = fopen(filename,"w");
    int j;

    if (fp == NULL) {
        ls->error = lino_error;
        return -1;
    }

    for (j = 0; j < ls->history_len; j++)
        fprintf(fp,"%s\n",ls->history[j]);
    fclose(fp);
    return 0;
}

/* Load the history from the specified file. If the file does not exist
 * zero is returned and no operation is performed.
 *
 * If the file exists and the operation succeeded 0 is returned, otherwise
 * on error -1 is returned. */
int lino_hist_load(lino_t *ls, const char *filename) {
    FILE *fp = fopen(filename,"r");
    char buf[LINENOISE_MAX_LINE];

    if (fp == NULL) {
        ls->error = lino_error;
        return -1;
    }

    while (fgets(buf,LINENOISE_MAX_LINE,fp) != NULL) {
        char *p = strchr(buf,'\n');
        if (p) *p = '\0';
        lino_hist_add(ls, buf);
    }
    fclose(fp);
    return 0;
}
