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

(defstruct sockaddr nil)

(defstruct sockaddr-in sockaddr
  (addr 0) (port 0))

(defstruct sockaddr-in6 sockaddr
  (addr 0) (port 0) (flow-info 0) (scope-id 0))

(defstruct sockaddr-un sockaddr
  path)

(defstruct addrinfo nil
  (flags 0)
  (family 0)
  (socktype 0)
  (protocol 0)
  (canonname 0))

(defvarl shut-rd 0)
(defvarl shut-wr 1)
(defvarl shut-rdwr 2)

(defun str-inaddr (addr : port)
  (let ((d (logand addr #xFF))
        (c (logand (ash addr -8) #xFF))
        (b (logand (ash addr -16) #xFF))
        (a         (ash addr -24))
        (p (if port `:@port` "")))
    (if (or (> a 255) (minusp a))
      (throwf 'eval-error "str-inaddr: ~a out of range for IPv4 address" addr)
      `@a.@b.@c.@d@p`)))

(defun sys:in6addr-condensed-text (numeric-pieces)
  (let* ((notyet t)
         (texts (window-mappend
                  1 nil
                  (lambda (pre chunk post)
                    (cond
                      ((and notyet (zerop (car chunk)) (cdr chunk))
                       (zap notyet)
                       (if (and post pre) '("") '(":")))
                      (t (mapcar (op format nil "~x") chunk))))
                  [partition-by zerop numeric-pieces])))
    `@{texts ":"}`))

(defun str-in6addr (addr : port)
  (let ((str (if (and (<= (width addr) 48)
                      (= (ash addr -32) #xFFFF))
               `::ffff:@(str-inaddr (logtrunc addr 32))`
               (let* ((pieces (let ((count 8))
                                (nexpand-left (lambda (val)
                                                (if (minusp (dec count))
                                                  (unless (zerop val)
                                                    (throwf 'eval-error
                                                            "str-in6addr: \
                                                            \ ~a out of range \
                                                            \ for IPv6 address"
                                                            addr))
                                                  (cons (logand val #xFFFF)
                                                        (ash val -16))))
                                              addr))))
                 (sys:in6addr-condensed-text pieces)))))
    (if port
      `[@str]:@port`
      str)))

(defun sys:str-inaddr-net-impl (addr wextra : weff)
  (let ((mask addr))
    (set mask (logior mask (ash mask 1)))
    (set mask (logior mask (ash mask 2)))
    (set mask (logior mask (ash mask 4)))
    (set mask (logior mask (ash mask 8)))
    (set mask (logior mask (ash mask 16)))
    (let ((w (- 32 (width (lognot mask 32))))
          (d (logand addr #xFF))
          (c (logand (ash addr -8) #xFF))
          (b (logand (ash addr -16) #xFF))
          (a         (ash addr -24))
          (we (or weff (+ w wextra))))
      (cond
        ((or (> a 255) (minusp a))
         (throwf 'eval-error "str-inaddr-net: ~a out of range for IPv4 address"
                 addr))
        ((> w 24) `@a.@b.@c.@d/@we`)
        ((> w 16) `@a.@b.@c/@we`)
        ((> w 8) `@a.@b/@we`)
        (t `@a/@we`)))))

(defun str-inaddr-net (addr : width)
  (sys:str-inaddr-net-impl addr 0 width))

(defun str-in6addr-net (addr : width)
  (if (and (<= (width addr) 48)
           (= (ash addr -32) #xFFFF))
    `::ffff:@(sys:str-inaddr-net-impl (logtrunc addr 32) 96 width)`
    (let ((mask addr))
      (set mask (logior mask (ash mask 1)))
      (set mask (logior mask (ash mask 2)))
      (set mask (logior mask (ash mask 4)))
      (set mask (logior mask (ash mask 8)))
      (set mask (logior mask (ash mask 16)))
      (set mask (logior mask (ash mask 32)))
      (set mask (logior mask (ash mask 64)))
      (let* ((w (- 128 (width (lognot mask 128))))
             (pieces (let ((count 8))
                       (nexpand-left (lambda (val)
                                       (if (minusp (dec count))
                                         (unless (zerop val)
                                           (throwf 'eval-error
                                                   "str-in6addr-net: \
                                                   \ ~a out of range \
                                                   \ for IPv6 address"
                                                   addr))
                                           (cons (logand val #xFFFF)
                                                 (ash val -16))))
                                       addr)))
             (cand-prefix [pieces 0..(trunc (+ w 15) 16)])
             (prefix (if (search cand-prefix '(0 0)) pieces cand-prefix)))
        `@(sys:in6addr-condensed-text prefix)/@(or width w)`))))

(defplace (sock-peer sock) body
  (getter setter
    ^(macrolet ((,getter () ^(sock-peer ',',sock))
                (,setter (val) ^(sock-set-peer ,',sock ,val)))
       ,body)))
