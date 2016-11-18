;; Copyright 2016
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

(defmacro tagbody (:env env . forms)
  (when forms
    (let* ((tb-id (gensym "tb-id-"))
           (next-var (gensym "next-"))
           (bblocks [partition forms (op where [orf symbolp integerp chrp])])
           (start-lbl (and (car bblocks) [[orf symbolp integerp chrp] (caar bblocks)]))
           (entry-lbl (if start-lbl (caar bblocks) (gensym "entry-"))))
      (unless start-lbl
        (push entry-lbl (car bblocks)))
      (let* ((lbls [mapcar car bblocks])
             (forms [mapcar cdr bblocks])
             ;; This trickery transform the individually labeled form
             ;; blocks into branches, such that each branch falls through
             ;; to the next one thanks to substructure sharing.
             (threaded-1 (mapcar (op member-if true) (conses forms)))
             (threaded-2 [apply nconc forms]) ;; important side effect
             (codes [mapcar car threaded-1]))
        (unless (eql (length (uniq lbls)) (length lbls))
          (throwf 'eval-error "~s: duplicate labels occur" 'tagbody))
        (let* ((basic-code ^(let ((,tb-id (gensym "tb-dyn-id-")))
                              (for ((,next-var 0))
                                   (,next-var)
                                   ((set ,next-var
                                         (block* ,tb-id
                                           (sys:switch ,next-var #(,*codes))
                                           nil))))))
               ;; pass one: expand inner forms, including tagbody forms.
               ;; if any inner tagbody forms leave (go ...) forms unexpanded,
               ;; protect those (go ...)forms from falling victim to the
               ;; global macro, by wrapping this with a harmless local go macro.
               (pass-one (sys:expand ^(macrolet ((go (:form form label) form))
                                        ,basic-code)) env))
          ;; pass two: now expand the remaining go forms at this level, against
          ;; this tagbody. If any go forms remain, they must refer to nonexistent
          ;; labels. By calling sys:expand one more time, we flush these out
          ;; using the global go macro --- unless we are nested inside the
          ;; pass-one expansion of outer tagbody, which protects them!
          ;; Thus, the outermost tagbody flushes out the undefined labels.
          (sys:expand ^(macrolet ((go (:form form label)
                                      (let ((index (posql label ',lbls)))
                                        (cond
                                          ((null index) form)
                                          (t ^(return* ,',tb-id ,index))))))
                         ,pass-one) env))))))

(defmacro go (label)
  (if [[orf symbolp integerp chrp] label]
    (throwf 'eval-error "~s: no ~s label visible" 'go label)
    (throwf 'eval-error "~s: ~s isn't a symbol, integer or character" 'go label)))
