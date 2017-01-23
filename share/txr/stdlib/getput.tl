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

(defun file-get (name)
  (with-stream (s (open-file name))
    (read s)))

(defun file-put (name obj)
  (with-stream (s (open-file name "w"))
    (prinl obj s)))

(defun file-append (name obj)
  (with-stream (s (open-file name "a"))
    (prinl obj s)))

(defun file-get-string (name)
  (with-stream (s (open-file name))
    (get-string s)))

(defun file-put-string (name string)
  (with-stream (s (open-file name "w"))
    (put-string string s)))

(defun file-append-string (name string)
  (with-stream (s (open-file name "a"))
    (put-string string s)))

(defun file-get-lines (name)
  (get-lines (open-file name)))

(defun file-put-lines (name lines)
  (with-stream (s (open-file name "w"))
    (put-lines lines s)))

(defun file-append-lines (name lines)
  (with-stream (s (open-file name "a"))
    (put-lines lines s)))

(defun command-get (cmd)
  (with-stream (s (open-command cmd))
    (read s)))

(defun command-put (cmd obj)
  (with-stream (s (open-command cmd "w"))
    (prinl obj s)))

(defun command-get-string (cmd)
  (with-stream (s (open-command cmd))
    (get-string s)))

(defun command-put-string (cmd string)
  (with-stream (s (open-command cmd "w"))
    (put-string string s)))

(defun command-get-lines (cmd)
  (get-lines (open-command cmd)))

(defun command-put-lines (cmd lines)
  (with-stream (s (open-command cmd "w"))
    (put-lines lines s)))
