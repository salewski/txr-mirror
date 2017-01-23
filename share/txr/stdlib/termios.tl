;; Copyright 2016-2017
;; Kaz Kylheku <kaz@kylheku.com>
;; Vancouver, Canada
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
