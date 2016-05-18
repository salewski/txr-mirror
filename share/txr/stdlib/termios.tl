;; Copyright 2016
;; Kaz Kylheku <kaz@kylheku.com>
;; Vancouver, Canada
;; All rights reserved.
;;
;; Redistribution of this software in source and binary forms, with or without
;; modification, is permitted provided that the following two conditions are met.
;;
;; Use of this software in any manner constitutes agreement with the disclaimer
;; which follows the two conditions.
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in
;;    the documentation and/or other materials provided with the
;;    distribution.
;;
;; THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
;; WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DAMAGES, HOWEVER CAUSED,
;; AND UNDER ANY THEORY OF LIABILITY, ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defmeth termios set-iflags (tio . values)
  (set tio.iflag (logior tio.iflag . values)))

(defmeth termios set-oflags (tio . values)
  (set tio.oflag (logior tio.oflag . values)))

(defmeth termios set-cflags (tio . values)
  (set tio.cflag (logior tio.cflag . values)))

(defmeth termios set-lflags (tio . values)
  (set tio.lflag (logior tio.lflag . values)))

(defmeth termios clear-iflags (tio . values)
  (set tio.iflag (logand tio.iflag (lognot (logior . values)))))

(defmeth termios clear-oflags (tio . values)
  (set tio.oflag (logand tio.oflag (lognot (logior . values)))))

(defmeth termios clear-cflags (tio . values)
  (set tio.cflag (logand tio.cflag (lognot (logior . values)))))

(defmeth termios clear-lflags (tio . values)
  (set tio.lflag (logand tio.lflag (lognot (logior . values)))))

(defmeth termios go-raw (tio)
  tio.(clear-iflags ignbrk brkint parmrk istrip inlcr igncr icrnl ixon)
  tio.(clear-oflags opost)
  tio.(clear-cflags csize parenb)
  tio.(clear-lflags echo echonl icanon isig)
  (if (boundp 'iexten)
    tio.(clear-lflags iexten))
  tio.(set-cflags cs8)
  (set tio.[cc vmin] 1)
  (set tio.[cc vtime] 0))

(defmeth termios go-cbreak (tio)
  tio.(clear-iflags icrnl)
  tio.(clear-lflags icanon)
  tio.(set-lfags isig)
  (set tio.[cc vmin] 1)
  (set tio.[cc vtime] 0))

(defmeth termios string-encode (tio)
  (let ((*print-base* 16))
    tio.(sys:encode-speeds)
    (downcase-str `@{tio.iflag}:@{tio.oflag}:@{tio.cflag}:@{tio.lflag}:\
                  @{(list-vec tio.cc) ":"}`)))

(defmeth termios string-decode (tio string)
  (let ((vals (mapcar (op int-str @1 16) (split-str string ":"))))
    (lset tio.iflag tio.oflag tio.cflag tio.lflag vals)
    (set tio.cc (vec-list (cddddr vals)))
    tio.(sys:decode-speeds)))
