/* Copyright 2018
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
  CALL = 6,
  APPLY = 7,
  GCALL = 8,
  GAPPLY = 9,
  MOVRS = 10,
  MOVSR = 11,
  MOVRR = 12,
  MOVRSI = 13,
  MOVSMI = 14,
  MOVRBI = 15,
  JMP = 16,
  IF = 17,
  IFQ = 18,
  IFQL = 19,
  UWPROT = 20,
  BLOCK = 21,
  RETSR = 22,
  RETRS = 23,
  RETRR = 24,
  ABSCSR = 25,
  CATCH = 26,
  HANDLE = 27,
  GETV = 28,
  GETF = 29,
  GETL1 = 30,
  GETVB = 31,
  GETFB = 32,
  GETL1B = 33,
  SETV = 34,
  SETL1 = 35,
  BINDV = 36,
  CLOSE = 37,
} vm_op_t;
