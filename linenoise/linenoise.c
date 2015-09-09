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
#include "linenoise.h"

#define LINENOISE_DEFAULT_HISTORY_MAX_LEN 100
#define LINENOISE_MAX_LINE 1024
#define LINENOISE_MAX_DISP (LINENOISE_MAX_LINE * 8)

/* The lino_state structure represents the state during line editing.
 * We pass this state to functions implementing specific editing
 * functionalities. */
struct lino_state {
    lino_t *next, *prev;        /* Links for global list: must be first */

    /* Lifetime enduring state */
    lino_compl_cb_t *completion_callback;
    void *cb_ctx;               /* User context for callback */
    struct termios orig_termios;        /* In order to restore at exit.*/
    int rawmode;        /* For atexit() function to check if restore is needed*/
    int mlmode;         /* Multi line mode. Default is single line. */
    int history_max_len;
    int history_len;
    char **history;
    int ifd;            /* Terminal stdin file descriptor. */
    int ofd;            /* Terminal stdout file descriptor. */

    /* Volatile state pertaining to just one linenoise call */
    char buf[LINENOISE_MAX_DISP];       /* Displayed line bufer. */
    char data[LINENOISE_MAX_LINE];      /* True data corresponding to display */
    const char *prompt; /* Prompt to display. */
    size_t plen;        /* Prompt length. */
    size_t pos;         /* Current cursor position. */
    size_t oldpos;      /* Previous refresh cursor position. */
    size_t len;         /* Current edited line display length. */
    size_t dlen;        /* True underlying length. */
    size_t dpos;        /* True underlying position. */
    size_t cols;        /* Number of columns in terminal. */
    size_t maxrows;     /* Maximum num of rows used so far (multiline mode) */
    int history_index;  /* The history index we are currently editing. */
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
static int atexit_registered = 0; /* Register atexit just 1 time. */

/* Debugging macro. */
#if 0
FILE *lndebug_fp = NULL;
#define lndebug(...) \
    do { \
        if (lndebug_fp == NULL) { \
            lndebug_fp = fopen("/tmp/lndebug.txt","a"); \
            fprintf(lndebug_fp, \
            "[%d %d %d] p: %d, rows: %d, rpos: %d, max: %d, oldmax: %d\n", \
            (int)l->len,(int)l->pos,(int)l->oldpos,plen,rows,rpos, \
            (int)l->maxrows,old_rows); \
        } \
        fprintf(lndebug_fp, ", " __VA_ARGS__); \
        fflush(lndebug_fp); \
    } while (0)
#else
#define lndebug(fmt, ...)
#endif

/* ======================= Low level terminal handling ====================== */

/* Set if to use or not the multi line mode. */
void lino_set_multiline(lino_t *ls, int ml) {
    ls->mlmode = ml;
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
    /* output modes - disable post processing */
    raw.c_oflag &= ~(OPOST);
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
    struct winsize ws;

    if (ioctl(1, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
        /* ioctl() failed. Try to query the terminal itself. */
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
    } else {
        return ws.ws_col;
    }

failed:
    return 80;
}

/* Clear the screen. Used to handle ctrl+l */
int lino_clear_screen(lino_t *ls) {
    return (write(ls->ofd,"\x1b[H\x1b[2J",7) > 0);
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
static void refresh_line(lino_t *l);

/* This is an helper function for edit() and is called when the
 * user types the <tab> key in order to complete the string currently in the
 * input.
 *
 * The state of the editing is encapsulated into the pointed lino_state
 * structure as described in the structure definition. */
static int complete_line(lino_t *ls) {
    lino_completions_t lc = { 0, NULL };
    int nread, nwritten;
    char c = 0;

    ls->completion_callback(ls->data, &lc, ls->cb_ctx);
    if (lc.len == 0) {
        generate_beep(ls);
    } else {
        size_t stop = 0, i = 0;

        while(!stop) {
            /* Show completion or original buffer */
            if (i < lc.len) {
                lino_t saved = *ls;

                ls->dpos = ls->dlen = strlen(lc.cvec[i]);
                strncpy(ls->data, lc.cvec[i], sizeof ls->data);
                ls->data[sizeof ls->data - 1] = 0;
                sync_data_to_buf(ls);
                refresh_line(ls);
                *ls = saved;
            } else {
                refresh_line(ls);
            }

            nread = read(ls->ifd,&c,1);
            if (nread <= 0) {
                free_completions(&lc);
                ls->error = (nread < 0 ? lino_ioerr : lino_eof);
                return -1;
            }

            switch(c) {
                case TAB:
                    i = (i+1) % (lc.len+1);
                    if (i == lc.len) generate_beep(ls);
                    break;
                case ESC:
                    /* Re-show original buffer */
                    if (i < lc.len) refresh_line(ls);
                    stop = 1;
                    break;
                default:
                    /* Update buffer and return */
                    if (i < lc.len) {
                        nwritten = snprintf(ls->data, sizeof ls->data, "%s", lc.cvec[i]);
                        ls->dpos = ls->dlen = nwritten;
                        sync_data_to_buf(ls);
                        refresh_line(ls);
                    }
                    stop = 1;
                    break;
            }
        }
    }

    free_completions(&lc);
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

    for (;;) {
        if (dptr - l->data == l->dpos)
            l->pos = bptr - l->buf;

        if (*dptr)  {
            char ch = *dptr++;

            if (ch == TAB) {
                int pos = bptr - l->buf;
                do {
                    *bptr++ = ' ';
                    pos++;
                } while (pos % 8 != 0);
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

    while((plen+pos) >= l->cols) {
        buf++;
        len--;
        pos--;
    }
    while (plen+len > l->cols) {
        len--;
    }

    ab_init(&ab);
    /* Cursor to left edge */
    snprintf(seq,64,"\r");
    ab_append(&ab,seq,strlen(seq));
    /* Write the prompt and the current buffer content */
    ab_append(&ab,l->prompt,strlen(l->prompt));
    ab_append(&ab,buf,len);
    /* Erase to right */
    snprintf(seq,64,"\x1b[0K");
    ab_append(&ab,seq,strlen(seq));
    /* Move cursor to original position. */
    snprintf(seq,64,"\r\x1b[%dC", (int)(pos+plen));
    ab_append(&ab,seq,strlen(seq));
    if (write(fd,ab.b,ab.len) == -1) {} /* Can't recover from write error. */
    ab_free(&ab);
}

/* Multi line low level line refresh.
 *
 * Rewrite the currently edited line accordingly to the buffer content,
 * cursor position, and number of columns of the terminal. */
static void refresh_multiline(lino_t *l) {
    char seq[64];
    int plen = strlen(l->prompt);
    int rows = (plen+l->len+l->cols-1)/l->cols; /* rows used by current buf. */
    int rpos = (plen+l->oldpos+l->cols)/l->cols; /* cursor relative row. */
    int rpos2; /* rpos after refresh. */
    int col; /* colum position, zero-based. */
    int old_rows = l->maxrows;
    int fd = l->ofd, j;
    struct abuf ab;

    /* Update maxrows if needed. */
    if (rows > (int)l->maxrows) l->maxrows = rows;

    /* First step: clear all the lines used before. To do so start by
     * going to the last row. */
    ab_init(&ab);
    if (old_rows-rpos > 0) {
        lndebug("go down %d", old_rows-rpos);
        snprintf(seq,64,"\x1b[%dB", old_rows-rpos);
        ab_append(&ab,seq,strlen(seq));
    }

    /* Now for every row clear it, go up. */
    for (j = 0; j < old_rows-1; j++) {
        lndebug("clear+up");
        snprintf(seq,64,"\r\x1b[0K\x1b[1A");
        ab_append(&ab,seq,strlen(seq));
    }

    /* Clean the top line. */
    lndebug("clear");
    snprintf(seq,64,"\r\x1b[0K");
    ab_append(&ab,seq,strlen(seq));

    /* Write the prompt and the current buffer content */
    ab_append(&ab,l->prompt,strlen(l->prompt));
    ab_append(&ab,l->buf,l->len);

    /* If we are at the very end of the screen with our prompt, we need to
     * emit a newline and move the prompt to the first column. */
    if (l->pos &&
        l->pos == l->len &&
        (l->pos+plen) % l->cols == 0)
    {
        lndebug("<newline>");
        ab_append(&ab,"\n",1);
        snprintf(seq,64,"\r");
        ab_append(&ab,seq,strlen(seq));
        rows++;
        if (rows > (int)l->maxrows) l->maxrows = rows;
    }

    /* Move cursor to right position. */
    rpos2 = (plen+l->pos+l->cols)/l->cols; /* current cursor relative row. */
    lndebug("rpos2 %d", rpos2);

    /* Go up till we reach the expected positon. */
    if (rows-rpos2 > 0) {
        lndebug("go-up %d", rows-rpos2);
        snprintf(seq,64,"\x1b[%dA", rows-rpos2);
        ab_append(&ab,seq,strlen(seq));
    }

    /* Set column. */
    col = (plen+(int)l->pos) % (int)l->cols;
    lndebug("set col %d", 1+col);
    if (col)
        snprintf(seq,64,"\r\x1b[%dC", col);
    else
        snprintf(seq,64,"\r");
    ab_append(&ab,seq,strlen(seq));

    lndebug("\n");
    l->oldpos = l->pos;

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

/* Insert the character 'c' at cursor current position.
 *
 * On error writing to the terminal -1 is returned, otherwise 0. */
static int edit_insert(lino_t *l, char c) {
    if (l->dlen < sizeof l->data - 1) {
        if (l->len == l->dpos) {
            l->data[l->dpos] = c;
            l->dpos++;
            l->dlen++;
            l->data[l->dlen] = '\0';
            sync_data_to_buf(l);
            if ((!l->mlmode && l->len == l->dlen && l->plen+l->len < l->cols) /* || mlmode */) {
                /* Avoid a full update of the line in the
                 * trivial case. */
                if (write(l->ofd,&c,1) == -1) return -1;
            } else {
                refresh_line(l);
            }
        } else {
            memmove(l->data + l->dpos+1, l->data + l->dpos, l->dlen-l->dpos);
            l->data[l->dpos] = c;
            l->dlen++;
            l->dpos++;
            l->data[l->dlen] = '\0';
            refresh_line(l);
        }
    }
    return 0;
}

/* Move cursor on the left. */
static void edit_move_left(lino_t *l) {
    if (l->dpos > 0) {
        l->dpos--;
        refresh_line(l);
    }
}

/* Move cursor on the right. */
static void edit_move_right(lino_t *l) {
    if (l->dpos != l->dlen) {
        l->dpos++;
        refresh_line(l);
    }
}

/* Move cursor to the start of the line. */
static void edit_move_home(lino_t *l) {
    if (l->dpos != 0) {
        l->dpos = 0;
        refresh_line(l);
    }
}

/* Move cursor to the end of the line. */
static void edit_move_end(lino_t *l) {
    if (l->dpos != l->dlen) {
        l->dpos = l->dlen;
        refresh_line(l);
    }
}

/* Substitute the currently edited line with the next or previous history
 * entry as specified by 'dir'. */
#define LINENOISE_HISTORY_NEXT 0
#define LINENOISE_HISTORY_PREV 1
static void edit_history_next(lino_t *l, int dir) {
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
        refresh_line(l);
    }
}

/* Delete the character at the right of the cursor without altering the cursor
 * position. Basically this is what happens with the "Delete" keyboard key. */
static void edit_delete(lino_t *l) {
    if (l->dlen > 0 && l->dpos < l->dlen) {
        memmove(l->data + l->dpos, l->data + l->dpos + 1, l->dlen - l->dpos - 1);
        l->dlen--;
        l->data[l->dlen] = '\0';
        refresh_line(l);
    }
}

/* Backspace implementation. */
static void edit_backspace(lino_t *l) {
    if (l->dpos > 0 && l->dlen > 0) {
        memmove(l->data + l->dpos - 1, l->data + l->dpos, l->dlen - l->dpos);
        l->dpos--;
        l->dlen--;
        l->data[l->dlen] = '\0';
        refresh_line(l);
    }
}

/* Delete all characters to left of cursor. */
static void edit_delete_prev_all(lino_t *l)
{
    memmove(l->data, l->data + l->dpos, l->dlen - l->dpos + 1);
    l->dlen -= l->dpos;
    l->dpos = 0;
    refresh_line(l);
}

/* Delete the previosu word, maintaining the cursor at the start of the
 * current word. */
static void edit_delete_prev_word(lino_t *l) {
    size_t odpos = l->dpos;
    size_t diff;

    while (l->dpos > 0 && strchr(SPACE, l->data[l->dpos - 1]))
        l->dpos--;
    while (l->dpos > 0 && strchr(SPACE, l->data[l->dpos - 1]) == 0)
        l->dpos--;
    diff = odpos - l->dpos;
    memmove(l->data + l->dpos, l->data + odpos, l->dlen - odpos + 1);
    l->dlen -= diff;
    refresh_line(l);
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
    int verbatim = 0;

    /* Populate the linenoise state that we pass to functions implementing
     * specific editing functionalities. */
    l->prompt = prompt;
    l->plen = strlen(prompt);
    l->oldpos = l->pos = l->len = 0;
    l->dpos = l->dlen = 0;
    l->cols = get_columns(l->ifd, l->ofd);
    l->maxrows = 0;
    l->history_index = 0;

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
        char c;
        int nread;
        char seq[3];

        nread = read(l->ifd,&c,1);
        if (nread <= 0)
            return l->len ? (int) l->len : -1;

        if (verbatim) {
            if (edit_insert(l,c)) {
                l->error = lino_ioerr;
                return -1;
            }
            verbatim = 0;
            continue;
        }

        /* Only autocomplete when the callback is set. It returns < 0 when
         * there was an error reading from fd. Otherwise it will return the
         * character that should be handled next. */
        if (c == 9 && l->completion_callback != NULL) {
            c = complete_line(l);
            /* Return on errors */
            if (c < 0) return l->len;
            /* Read next character when 0 */
            if (c == 0) continue;
        }

        switch(c) {
        case ENTER:
            if (l->history_len > 0) {
                l->history_len--;
                free(l->history[l->history_len]);
                l->history[l->history_len] = 0;
            }
            if (l->mlmode) edit_move_end(l);
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
            if (l->len > 0) {
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
                refresh_line(l);
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
        default:
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
        case CTL('K'): /* delete from current to end of line. */
            l->data[l->dpos] = '\0';
            l->dlen = l->dpos;
            refresh_line(l);
            break;
        case CTL('A'):
            edit_move_home(l);
            break;
        case CTL('E'):
            edit_move_end(l);
            break;
        case CTL('L'):
            lino_clear_screen(l);
            refresh_line(l);
            break;
        case CTL('W'):
            edit_delete_prev_word(l);
            break;
        case CTL('Z'):
            disable_raw_mode(l);
            raise(SIGTSTP);
            enable_raw_mode(l);
            refresh_line(l);
            break;
        }
    }
    return l->len;
}

/* This special mode is used by linenoise in order to print scan codes
 * on screen for debugging / development purposes. It is implemented
 * by the linenoise_example program using the --keycodes option. */
void lino_print_keycodes(lino_t *l) {
    char quit[4];

    printf("Linenoise key codes debugging mode.\n"
            "Press keys to see scan codes. Type 'quit' at any time to exit.\n");
    if (enable_raw_mode(l) == -1) return;
    memset(quit,' ',4);
    while(1) {
        char c;
        int nread;

        nread = read(l->ifd,&c,1);
        if (nread <= 0) continue;
        memmove(quit,quit+1,sizeof(quit)-1); /* shift string to left. */
        quit[sizeof(quit)-1] = c; /* Insert current char on the right. */
        if (memcmp(quit,"quit",sizeof(quit)) == 0) break;

        printf("'%c' %02x (%d) (type quit to exit)\n",
            isprint(c) ? c : '?', (int)c, (int)c);
        printf("\r"); /* Go left edge manually, we are in raw mode. */
        fflush(stdout);
    }
    disable_raw_mode(l);
}

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
        return chk_strdup_utf8(ls->data);
    }
}

lino_t *lino_make(int ifd, int ofd)
{
    lino_t *ls = (lino_t *) chk_malloc(sizeof *ls);

    if (ls) {
        memset(ls, 0, sizeof *ls);
        ls->history_max_len = LINENOISE_DEFAULT_HISTORY_MAX_LEN;
        ls->ifd = ifd;
        ls->ofd = ofd;

        ls->prev = &lino_list;
        ls->next = lino_list.next;
        lino_list.next->prev = ls;
        lino_list.next = ls;
    }

    return ls;
}

static void free_hist(lino_t *ls);

static void lino_cleanup(lino_t *ls)
{
    disable_raw_mode(ls);
    free_hist(ls);
}

void lino_free(lino_t *ls)
{
    ls->prev->next = ls->next;
    ls->next->prev = ls->prev;
    ls->next = ls->prev = 0;
    lino_cleanup(ls);
    free(ls);
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
        char *p;

        p = strchr(buf,'\r');
        if (!p) p = strchr(buf,'\n');
        if (p) *p = '\0';
        lino_hist_add(ls, buf);
    }
    fclose(fp);
    return 0;
}
