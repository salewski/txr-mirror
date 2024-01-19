/* Copyright 2018-2024
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

typedef enum vm_op {
  NOOP = 0,
  FRAME = 1,
  SFRAME = 2,
  DFRAME = 3,
  END = 4,
  PROF = 5,
  CALL = 6,
  APPLY = 7,
  GCALL = 8,
  GAPPLY = 9,
  MOVRS = 10,
  MOVSR = 11,
  MOVRR = 12,
  JMP = 13,
  IF = 14,
  IFQ = 15,
  IFQL = 16,
  SWTCH = 17,
  UWPROT = 18,
  BLOCK = 19,
  RETSR = 20,
  RETRS = 21,
  RETRR = 22,
  ABSCSR = 23,
  CATCH = 24,
  HANDLE = 25,
  GETV = 26,
  OLDGETF = 27,
  GETL1 = 28,
  GETVB = 29,
  GETFB = 30,
  GETL1B = 31,
  SETV = 32,
  SETL1 = 33,
  BINDV = 34,
  CLOSE = 35,
  GETLX = 36,
  SETLX = 37,
  GETF = 38,
} vm_op_t;

#define VM_LEV_BITS 10
#define VM_LEV_MASK 0x3FF
#define VM_SM_LEV_BITS 6
#define VM_SM_LEV_MASK 0x3F
#define VM_MAX_LEV 63
#define VM_MAX_V_LEV 61
#define VM_LEV_SIZE 1024
