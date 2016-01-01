;; Copyright 2015-2016
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

(defmacro with-out-string-stream ((stream) . body)
  ^(let ((,stream (make-string-output-stream)))
     ,*body
     (get-string-from-stream ,stream)))

(defmacro with-out-strlist-stream ((stream) . body)
  ^(let ((,stream (make-strlist-output-stream)))
     ,*body
     (get-list-from-stream ,stream)))

(defmacro with-in-string-stream ((stream string) . body)
  ^(let ((,stream (make-string-input-stream ,string)))
     ,*body))

(defmacro with-in-string-byte-stream ((stream string) . body)
  ^(let ((,stream (make-string-byte-input-stream ,string)))
     ,*body))

(defmacro with-stream ((var stream) . body)
  ^(let ((,var ,stream))
     (unwind-protect
       (progn ,*body)
       (close-stream ,var))))
