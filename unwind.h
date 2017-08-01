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

typedef union uw_frame uw_frame_t;
typedef enum uw_frtype uw_frtype_t;

enum uw_frtype { UW_BLOCK, UW_ENV };

struct uw_common {
  uw_frame_t *up;
  uw_frtype_t type;
};

struct uw_block {
  uw_frame_t *up;
  uw_frtype_t type;
  obj_t *tag;
  obj_t *result;
  jmp_buf jb;
};

struct uw_dynamic_env {
  uw_frame_t *up;
  uw_frtype_t type;
  obj_t *func_bindings;
};

union uw_frame {
  struct uw_common uw;
  struct uw_block bl;
  struct uw_dynamic_env ev;
};

void uw_init(void);
void uw_push_block(uw_frame_t *, obj_t *tag);
void uw_push_env(uw_frame_t *);
obj_t *uw_get_func(obj_t *sym);
obj_t *uw_set_func(obj_t *sym, obj_t *value);
obj_t *uw_block_return(obj_t *tag, obj_t *result);
void uw_pop_frame(uw_frame_t *);


#define uw_block_begin(TAG, RESULTVAR)  \
  obj_t *RESULTVAR = nil;               \
  {                                     \
    uw_frame_t uw_blk;                  \
    uw_push_block(&uw_blk, TAG);        \
    if (setjmp(uw_blk.bl.jb)) {         \
      RESULTVAR = uw_blk.bl.result;     \
    } else  {

#define uw_block_end                    \
    }                                   \
    uw_pop_frame(&uw_blk);              \
  }

#define uw_env_begin                    \
  {                                     \
    uw_frame_t uw_env;                  \
    uw_push_env(&uw_env)

#define uw_env_end                      \
    uw_pop_frame(&uw_env);              \
  }
