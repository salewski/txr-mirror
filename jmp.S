/* Copyright 2009-2016
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * Redistribution of this software in source and binary forms, with or without
 * modification, is permitted provided that the following two conditions are met.
 *
 * Use of this software in any manner constitutes agreement with the disclaimer
 * which follows the two conditions.
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DAMAGES, HOWEVER CAUSED,
 * AND UNDER ANY THEORY OF LIABILITY, ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#if __MINGW32__ || __CYGWIN32__
#define DEFUN(NAME) \
.global _ ## NAME ; \
_ ## NAME: ;
#elif __APPLE__
#define DEFUN(NAME) \
.globl _ ## NAME ; \
_ ## NAME: ;
#elif __arm__ && !__thumb__
#define DEFUN(NAME) \
.text ; \
.align 4 ; \
.global NAME ; \
.type NAME, %function ; \
NAME: ;
#elif __arm__ && __thumb__
#define DEFUN(NAME) \
.text ; \
.align 4 ; \
.global NAME ; \
.thumb ; \
.thumb_func ; \
.type NAME, %function ; \
NAME: ;
#else
#define DEFUN(NAME) \
.global NAME ; \
.type NAME, @function ; \
NAME: ;
#endif

#if __i386__

#define JEIP  0
#define JESP  4
#define JEBP  8
#define JEBX 12
#define JESI 16
#define JEDI 20

#define RETA  0
#define ARG1  4
#define ARG2  8

DEFUN(jmp_save)
  movl RETA(%esp), %ecx
  movl ARG1(%esp), %eax
  movl %ecx, JEIP(%eax)
  leal ARG1(%esp), %ecx
  movl %ecx, JESP(%eax)
  movl %ebp, JEBP(%eax)
  movl %ebx, JEBX(%eax)
  movl %esi, JESI(%eax)
  movl %edi, JEDI(%eax)
  xorl %eax, %eax
  ret

DEFUN(jmp_restore)
  movl ARG1(%esp), %edx
  movl ARG2(%esp), %eax
  mov JEDI(%edx),%edi
  mov JESI(%edx),%esi
  mov JEBX(%edx),%ebx
  mov JEBP(%edx),%ebp
  mov JESP(%edx),%ecx
  mov %ecx,%esp
  mov JEIP(%edx),%ecx
  jmp *%ecx

#elif __x86_64__

#define JRIP  0
#define JRSP  8
#define JRBP 16
#define JRBX 24
#define JR12 32
#define JR13 40
#define JR14 48
#define JR15 56

#define RETA  0
#define ARG2  8

DEFUN(jmp_save)
  mov RETA(%rsp), %rdx
  mov %rdx, JRIP(%rdi)
  lea ARG2(%rsp), %rdx
  mov %rdx, JRSP(%rdi)
  mov %rbp, JRBP(%rdi)
  mov %rbx, JRBX(%rdi)
  mov %r12, JR12(%rdi)
  mov %r13, JR13(%rdi)
  mov %r14, JR14(%rdi)
  mov %r15, JR15(%rdi)
  xor %rax, %rax
  ret

DEFUN(jmp_restore)
  mov JR15(%rdi), %r15
  mov JR14(%rdi), %r14
  mov JR13(%rdi), %r13
  mov JR12(%rdi), %r12
  mov JRBX(%rdi), %rbx
  mov JRBP(%rdi), %rbp
  mov JRSP(%rdi), %rdx
  mov %rdx, %rsp
  mov JRIP(%rdi), %rdx
  mov %rsi, %rax
  jmp *%rdx

#elif __arm__ && !__thumb__

DEFUN(jmp_save)
  stmia r0, {r4, r5, r6, r7, r8, r9, r10, fp, sp, lr}
  mov   r0, #0
  bx    lr

DEFUN(jmp_restore)
  ldmia r0, {r4, r5, r6, r7, r8, r9, r10, fp, sp, lr}
  mov   r0, r1
  bx    lr

#elif __arm__ && __thumb__

DEFUN(jmp_save)
  mov   r2, r0
  mov   r3, lr
  stmia r0!, {r3, r4, r5, r6, r7}
  mov   r3, r8
  mov   r4, r9
  mov   r5, r10
  mov   r6, fp
  mov   r7, sp
  stmia r0!, {r3, r4, r5, r6, r7}
  ldmia r2!, {r3, r4, r5, r6, r7}
  mov   r0, #0
  bx    lr

DEFUN(jmp_restore)
  mov   r2, r0
  add   r0, #5*4
  ldmia r0!, {r3, r4, r5, r6, r7}
  mov   r8, r3
  mov   r9, r4
  mov   r10, r5
  mov   fp, r6
  mov   sp, r7
  ldmia r2!, {r3, r4, r5, r6, r7}
  mov   r0, r1
  bx    r3

#elif __PPC64__

DEFUN(jmp_save)
  mflr  %r11
  mfcr  %r12
  std   %r1, 0(%r3)
  std   %r2, 8(%r3)
  std   %r11, 16(%r3)
  std   %r12, 24(%r3)
  std   %r13, 32(%r3)
  std   %r14, 40(%r3)
  std   %r15, 48(%r3)
  std   %r16, 56(%r3)
  std   %r17, 64(%r3)
  std   %r18, 72(%r3)
  std   %r19, 80(%r3)
  std   %r20, 88(%r3)
  std   %r21, 96(%r3)
  std   %r22, 104(%r3)
  std   %r23, 112(%r3)
  std   %r24, 120(%r3)
  std   %r25, 128(%r3)
  std   %r26, 136(%r3)
  std   %r27, 144(%r3)
  std   %r28, 152(%r3)
  std   %r29, 160(%r3)
  std   %r30, 168(%r3)
  std   %r31, 176(%r3)
  li    %r3, 0
  blr

DEFUN(jmp_restore)
  ld    %r31, 176(%r3)
  ld    %r30, 168(%r3)
  ld    %r29, 160(%r3)
  ld    %r28, 152(%r3)
  ld    %r27, 144(%r3)
  ld    %r26, 136(%r3)
  ld    %r25, 128(%r3)
  ld    %r24, 120(%r3)
  ld    %r23, 112(%r3)
  ld    %r22, 104(%r3)
  ld    %r21, 96(%r3)
  ld    %r20, 88(%r3)
  ld    %r17, 64(%r3)
  ld    %r19, 80(%r3)
  ld    %r18, 72(%r3)
  ld    %r16, 56(%r3)
  ld    %r15, 48(%r3)
  ld    %r14, 40(%r3)
  ld    %r13, 32(%r3)
  ld    %r12, 24(%r3)
  ld    %r11, 16(%r3)
  ld    %r2, 8(%r3)
  ld    %r1, 0(%r3)
  mtcr  %r12
  mtlr  %r11
  mr    %r3, %r4
  blr

#else
#error port me!
#endif
