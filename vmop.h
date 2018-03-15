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
  MOVRS = 8,
  MOVSR = 9,
  MOVRR = 10,
  MOVRSI = 11,
  MOVSMI = 12,
  MOVRBI = 13,
  JMP = 14,
  IF = 15,
  IFQ = 16,
  IFQL = 17,
  UWPROT = 18,
  BLOCK = 19,
  RETSR = 20,
  RETRS = 21,
  RETRR = 22,
  CATCH = 23,
  HANDLE = 24,
  GETV = 25,
  GETF = 26,
  GETL1 = 27,
  GETVB = 28,
  GETFB = 29,
  GETL1B = 30,
  SETV = 31,
  SETL1 = 32,
  BINDV = 33,
  CLOSE = 34,
} vm_op_t;
