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
  DFRAME = 2,
  END = 3,
  FIN = 4,
  CALL = 5,
  APPLY = 6,
  MOVRS = 7,
  MOVSR = 8,
  MOVRR = 9,
  MOVRSI = 10,
  MOVSMI = 11,
  MOVRBI = 12,
  JMP = 13,
  IF = 14,
  UWPROT = 15,
  BLOCK = 16,
  RETSR = 17,
  RETRS = 18,
  RETRR = 19,
  CATCH = 20,
  HANDLE = 21,
  GETV = 22,
  GETF = 23,
  GETL1 = 24,
  GETVB = 25,
  GETFB = 26,
  GETL1B = 27,
  SETV = 28,
  SETL1 = 29,
  BINDV = 30,
  CLOSE = 31,
} vm_op_t;
