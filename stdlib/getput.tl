;; Copyright 2016-2022
;; Kaz Kylheku <kaz@kylheku.com>
;; Vancouver, Canada
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice,
;;    this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

(defun sys:get-buf-common (s bytes seek)
  (let ((b (make-buf 0 0 (min bytes 4096)))
        (o 0))
    (when (plusp seek)
      (unless (ignerr (seek-stream s seek :from-current))
        (let ((b (make-buf (min seek 4096)))
              (c 0))
          (while (< c seek)
            (let ((p (fill-buf b 0 s)))
              (if (zerop p)
                (return))
              (inc c p))))))
    (while (or (null bytes) (< (len b) bytes))
      (let ((p (fill-buf-adjust b o s)))
        (when (= p o)
          (return))
        (set o p)
        (when (eql p (buf-alloc-size b))
          (buf-set-length b (min (+ p p) bytes)))))
    b))

(defun get-jsons (: (s *stdin*))
  (when (stringp s)
    (set s (make-string-byte-input-stream s)))
  (build
    (catch*
      (while t
        (add (get-json s)))
      (syntax-error (type . args)
        (if (parse-errors s)
          (throw type . args))))))

(defun put-jsons (list : (s *stdout*) flat-p)
  (each ((obj list))
    (put-jsonl obj s flat-p))
  t)

(defun file-get (name : mopt)
  (with-stream (s (open-file name `r@mopt`))
    (read s)))

(defun file-put (name obj : mopt)
  (with-stream (s (open-file name `w@mopt`))
    (prinl obj s)))

(defun file-append (name obj : mopt)
  (with-stream (s (open-file name `a@mopt`))
    (prinl obj s)))

(defun file-get-string (name : mopt)
  (with-stream (s (open-file name `r@mopt`))
    (get-string s)))

(defun file-put-string (name string : mopt)
  (with-stream (s (open-file name `w@mopt`))
    (put-string string s)))

(defun file-append-string (name string : mopt)
  (with-stream (s (open-file name `a@mopt`))
    (put-string string s)))

(defun file-get-lines (name : mopt)
  (get-lines (open-file name `r@mopt`)))

(defun file-put-lines (name lines : mopt)
  (with-stream (s (open-file name `w@mopt`))
    (put-lines lines s)))

(defun file-append-lines (name lines : mopt)
  (with-stream (s (open-file name `a@mopt`))
    (put-lines lines s)))

(defun file-get-buf (name : bytes (seek 0) mopt)
  (with-stream (s (open-file name `rb@(if bytes "u")@mopt`))
    (sys:get-buf-common s bytes seek)))

(defun file-put-buf (name buf : (seek 0) mopt)
  (with-stream (s (open-file name `wb@mopt`))
    (unless (zerop seek)
      (seek-stream s seek :from-current))
    (put-buf buf 0 s)))

(defun file-place-buf (name buf : (seek 0) mopt)
  (with-stream (s (open-file name `mb@mopt`))
    (unless (zerop seek)
      (seek-stream s seek :from-current))
    (put-buf buf 0 s)))

(defun file-append-buf (name buf : mopt)
  (with-stream (s (open-file name `ab@mopt`))
    (put-buf buf 0 s)))

(defun file-get-json (name : mopt)
  (with-stream (s (open-file name `r@mopt`))
    (get-json s)))

(defun file-put-json (name obj : flat-p mopt)
  (with-stream (s (open-file name `w@mopt`))
    (put-jsonl obj s flat-p)))

(defun file-append-json (name obj : flat-p mopt)
  (with-stream (s (open-file name `a@mopt`))
    (put-jsonl obj s flat-p)))

(defun file-get-jsons (name : mopt)
  (with-stream (s (open-file name `r@mopt`))
    (get-jsons s)))

(defun file-put-jsons (name seq : flat-p mopt)
  (with-stream (s (open-file name `w@mopt`))
    (put-jsons seq s flat-p)))

(defun file-append-jsons (name seq : flat-p mopt)
  (with-stream (s (open-file name `a@mopt`))
    (put-jsons s seq flat-p)))

(defun command-get (cmd : mopt)
  (with-stream (s (open-command cmd `r@mopt`))
    (read s)))

(defun command-put (cmd obj : mopt)
  (with-stream (s (open-command cmd `w@mopt`))
    (prinl obj s)))

(defun command-get-string (cmd : mopt)
  (with-stream (s (open-command cmd `r@mopt`))
    (get-string s)))

(defun command-put-string (cmd string : mopt)
  (with-stream (s (open-command cmd `w@mopt`))
    (put-string string s)))

(defun command-get-lines (cmd : mopt)
  (get-lines (open-command cmd `r@mopt`)))

(defun command-put-lines (cmd lines : mopt)
  (with-stream (s (open-command cmd `w@mopt`))
    (put-lines lines s)))

(defun command-get-buf (cmd : bytes (skip 0))
  (with-stream (s (open-command cmd (if bytes "rbu" "rb")))
    (sys:get-buf-common s bytes skip)))

(defun command-put-buf (cmd buf : mopt)
  (with-stream (s (open-command cmd `wb@mopt`))
    (put-buf buf 0 s)))

(defun command-get-json (cmd : mopt)
  (with-stream (s (open-command cmd `r@mopt`))
    (get-json s)))

(defun command-put-json (cmd obj : flat-p mopt)
  (with-stream (s (open-command cmd `w@mopt`))
    (put-jsonl obj s flat-p)))

(defun command-get-jsons (cmd : mopt)
  (with-stream (s (open-command cmd `r@mopt`))
    (get-jsons s)))

(defun command-put-jsons (cmd seq : flat-p mopt)
  (with-stream (s (open-command cmd `w@mopt`))
    (put-jsons seq s flat-p)))
