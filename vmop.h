/* Copyright 2018-2020
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

typedef enum vm_op {
  NOOP = 0,
  FRAME = 1,
  SFRAME = 2,
  DFRAME = 3,
  END = 4,
  FIN = 5,
  PROF = 6,
  CALL = 7,
  APPLY = 8,
  GCALL = 9,
  GAPPLY = 10,
  MOVRS = 11,
  MOVSR = 12,
  MOVRR = 13,
  MOVRSI = 14,
  MOVSMI = 15,
  MOVRBI = 16,
  JMP = 17,
  IF = 18,
  IFQ = 19,
  IFQL = 20,
  SWTCH = 21,
  UWPROT = 22,
  BLOCK = 23,
  RETSR = 24,
  RETRS = 25,
  RETRR = 26,
  ABSCSR = 27,
  CATCH = 28,
  HANDLE = 29,
  GETV = 30,
  OLDGETF = 31,
  GETL1 = 32,
  GETVB = 33,
  GETFB = 34,
  GETL1B = 35,
  SETV = 36,
  SETL1 = 37,
  BINDV = 38,
  CLOSE = 39,
  GETLX = 40,
  SETLX = 41,
  GETF = 42,
} vm_op_t;
