;; test local setjmp
(let ((jb (jmp-buf)))
  (setjmp jb result
    (progn (put-line "setjmp")
           (longjmp jb 42))
    (put-line `result @result`)))

(defun png-fake-output ()
  (put-string "libpng longjmp\nlibpng error 42\n")
  (exit))

(unless (ignerr (dlopen "libpng.so"))
  (png-fake-output))

;; needed by png-set-longjmp-fn API
(defvarl libc (dlopen nil))
(defvarl longjmp-addr (dlsym libc "longjmp"))

(typedef png-structp (cptr png))

(with-dyn-lib "libpng.so"
  (deffi png-get-header-ver "png_get_header_ver" str (png-structp))
  (deffi png-create-read-struct "png_create_read_struct" png-structp (str cptr cptr cptr))
  (deffi png-set-longjmp-fn "png_set_longjmp_fn" (carray uchar) (png-structp (cptr dlsym) size-t))
  (deffi png-longjmp "png_longjmp" void (png-structp int)))

(defvar png-ver (png-get-header-ver cptr-null))

;; In the png.h header, png_setjmp is a macro only; you cannot
;; #undef it to get to a function. So we write the macro in
;; the same way as a Lisp macro, in terms of png-set-longjmp-fn,
;; whereby we pass the longjmp function, and sizeof (jmp_buf).
(defmacro png-setjmp (png-ptr)
  (let ((jmpbuf-size (load-time (len (jmp-buf)))))
    ^(png-set-longjmp-fn ,png-ptr longjmp-addr ,jmpbuf-size)))

;;; Test

;; get png handle
(defvar png (png-create-read-struct png-ver cptr-null cptr-null cptr-null))

;; get jmp_buf from png handle, setjmp it, longjmp to it.
(setjmp (png-setjmp png) err
  (progn (put-line "libpng longjmp")
         (png-longjmp png 42))
  (put-line `libpng error @err`))
