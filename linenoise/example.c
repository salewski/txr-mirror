#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "linenoise.h"

typedef unsigned char mem_t;

mem_t *chk_malloc(size_t n)
{
    return malloc(n);
}

mem_t *chk_realloc(mem_t *old, size_t size)
{
    return realloc(old, size);
}

void completion(const char *buf, lino_completions_t *lc) {
    if (buf[0] == 'h') {
        lino_add_completion(lc, "hello");
        lino_add_completion(lc, "hello there");
    }
}

int main(int argc, char **argv) {
    char *line;
    char *prgname = argv[0];

    /* Parse options, with --multiline we enable multi line editing. */
    while(argc > 1) {
        argc--;
        argv++;
        if (!strcmp(*argv,"--multiline")) {
            lino_set_multiline(1);
            printf("Multi-line mode enabled.\n");
        } else if (!strcmp(*argv,"--keycodes")) {
            lino_print_keycodes();
            exit(0);
        } else {
            fprintf(stderr, "Usage: %s [--multiline] [--keycodes]\n", prgname);
            exit(1);
        }
    }

    /* Set the completion callback. This will be called every time the
     * user uses the <tab> key. */
    lino_set_completion_cb(completion);

    /* Load history from file. The history file is just a plain text file
     * where entries are separated by newlines. */
    lino_hist_load("history.txt"); /* Load the history at startup */

    /* Now this is the main loop of the typical linenoise-based application.
     * The call to linenoise() will block as long as the user types something
     * and presses enter.
     *
     * The typed string is returned as a malloc() allocated string by
     * linenoise, so the user needs to free() it. */
    while((line = linenoise("hello> ")) != NULL) {
        /* Do something with the string. */
        if ((1 || line[0] != '\0') && line[0] != '/') {
            printf("echo: '%s'\n", line);
            lino_hist_add(line); /* Add to the history. */
            lino_hist_save("history.txt"); /* Save the history on disk. */
        } else if (!strncmp(line,"/historylen",11)) {
            /* The "/historylen" command will change the history len. */
            int len = atoi(line+11);
            lino_hist_set_max_len(len);
        } else if (line[0] == '/') {
            printf("Unreconized command: %s\n", line);
        }
        free(line);
    }
    return 0;
}
