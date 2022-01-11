;; Copyright 2017-2022
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
(defstruct stream-wrap nil
  stream
  (:method put-string (me str)
    (put-string str me.stream))
  (:method put-char (me chr)
    (put-char chr me.stream))
  (:method put-byte (me byte)
    (put-byte byte me.stream))
  (:method get-line (me)
    (get-line me.stream))
  (:method get-char (me)
    (get-char me.stream))
  (:method get-byte (me)
    (get-byte me.stream))
  (:method unget-char (me chr)
    (unget-char chr me.stream))
  (:method unget-byte (me byte)
    (unget-byte byte me.stream))
  (:method put-buf (me buf pos)
    (put-buf buf pos me.stream))
  (:method fill-buf (me buf pos)
    (fill-buf buf pos me.stream))
  (:method close (me)
    (close-stream me.stream))
  (:method flush (me)
    (flush-stream me.stream))
  (:method seek (me offs whence)
    (seek-stream me.stream offs whence))
  (:method truncate (me len)
    (truncate-stream me.stream len))
  (:method get-prop (me sym)
    (stream-get-prop me.stream sym))
  (:method set-prop (me sym nval)
    (stream-set-prop me.stream sym nval))
  (:method get-error (me)
    (get-error me.stream))
  (:method get-error-str (me)
    (get-error-str me.stream))
  (:method clear-error (me)
    (clear-error me.stream))
  (:method get-fd (me)
    (fileno me.stream)))
