;; Copyright 2016-2019
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

(defvar *trace-output* *stdout*)

(defvar sys:*trace-hash* (hash :equal-based))
(defvar sys:*trace-level* -1)

(defvarl sys:tr* (fun *))
(defvarl sys:trfm (fun format))

(defun sys:trace-enter (name args)
  [sys:trfm *trace-output* "~*a(~s ~s\n" [sys:tr* sys:*trace-level* 2] "" name args])

(defun sys:trace-leave (name val)
  [sys:trfm *trace-output* "~*a  ~s)\n" [sys:tr* sys:*trace-level* 2] "" val])

(defun sys:trace-canonicalize-name (name)
  (if (and (consp name)
           (eq (car name) 'meth))
    (let* ((req-type-sym (cadr name))
           (slot-sym (caddr name))
           (req-type (find-struct-type req-type-sym))
           (s-s-p (if req-type
                    (static-slot-p req-type slot-sym)))
           (actual-type-sym (if s-s-p
                              (static-slot-home req-type-sym slot-sym))))
      (if (and s-s-p (neq req-type-sym actual-type-sym))
        ^(meth ,actual-type-sym ,slot-sym)
        name))
    name))

(defun sys:trace (names)
  (cond
    ((null names) (hash-keys sys:*trace-hash*))
    (t
      (each ((orig-n names)
             (n [mapcar sys:trace-canonicalize-name names]))
        (unless [sys:*trace-hash* n]
          (when (neq n orig-n)
            (usr:catch
              (throwf 'warning "~s: ~s is actually ~s: tracing that instead"
                      'trace orig-n n)
              (continue ())))
          (let* ((prev (or (symbol-function n)
                           (throwf 'eval-error
                                   "~s: ~s does not name a function" 'trace n)))
                 (lex-n n)
                 (hook (lambda (. args)
                         (let ((abandoned t)
                               (sys:*trace-level* (succ sys:*trace-level*)))
                           (unwind-protect
                             (progn
                               (sys:trace-enter lex-n args)
                               (let ((val (apply prev args)))
                                 (sys:trace-leave lex-n val)
                                 (set abandoned nil)
                                 val))
                             (if abandoned
                               (sys:trace-leave lex-n :abandoned)))))))
            (set (symbol-function n) hook
                 [sys:*trace-hash* n] prev)))))))

(defun sys:untrace (names)
  (flet ((disable (name-orig name)
           (let ((prev (del [sys:*trace-hash* name])))
             (when prev
               (when (neq name-orig name)
                 (usr:catch
                   (throwf 'warning "~s: ~s is actually ~s: untracing that instead"
                           'trace name-orig name)
                   (continue ())))
               (set (symbol-function name) prev)))))
    (if names
      (each ((n-orig names)
             (n [mapcar sys:trace-canonicalize-name names]))
        (disable n-orig n))
      (dohash (n v sys:*trace-hash*)
        (disable n n)))))

(defun sys:trace-redefine-check (orig-name)
  (let ((name (sys:trace-canonicalize-name orig-name)))
    (when [sys:*trace-hash* name]
      (usr:catch
        (cond
          ((neq name orig-name)
             (throwf 'warning "~!~s won't be traced, though it overrides\n\
                               ~s which is currently traced"
                      name orig-name))
          (t (throwf 'warning "previously traced ~s is redefined and no\ \
                               longer traced"
                      name)
             (sys:untrace (list name))))
        (continue ())))))

(defmacro usr:trace (. names)
  ^(sys:trace ',names))

(defmacro usr:untrace (. names)
  ^(sys:untrace ',names))
