;; Copyright 2016-2021
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

(defstruct sockaddr nil
  (:static family nil))

(defstruct sockaddr-in sockaddr
  (addr 0) (port 0)
  (prefix 32)
  (:static family af-inet))

(defstruct sockaddr-in6 sockaddr
  (addr 0) (port 0) (flow-info 0) (scope-id 0)
  (prefix 128)
  (:static family af-inet6))

(defstruct sockaddr-un sockaddr
  path
  (:static family af-unix))

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
      (throwf 'eval-error "~s: ~a out of range for IPv4 address"
              'str-inaddr addr)
      `@a.@b.@c.@d@p`)))


(defun sys:in6addr-condensed-text (numeric-pieces)
  (let* ((str (cat-str [mapcar (op fmt "~x") numeric-pieces] ":"))
         (zr (rra #/0(:0)+/ str))
         (lp [pos-max zr : [callf - to from]])
         (lr [zr lp]))
    (when lp
      (del [str lr]))
    (cond
      ((equal "" str) "::")
      ((starts-with ":" str) `:@str`)
      ((ends-with ":" str) `@str:`)
      (t str))))

(defun str-in6addr (addr : port)
  (let ((str (if (and (<= (width addr) 48)
                      (= (ash addr -32) #xFFFF))
               `::ffff:@(str-inaddr (logtrunc addr 32))`
               (let* ((pieces (let ((count 8))
                                (nexpand-left (lambda (val)
                                                (if (minusp (dec count))
                                                  (unless (zerop val)
                                                    (throwf 'eval-error
                                                            "~s: \
                                                            \ ~a out of range \
                                                            \ for IPv6 address"
                                                            'str-in6addr
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
    (let* ((w (- 32 (width (lognot mask 32))))
           (d (logand addr #xFF))
           (c (logand (ash addr -8) #xFF))
           (b (logand (ash addr -16) #xFF))
           (a         (ash addr -24))
           (we (or weff (+ w wextra))))
      (cond
        ((or (> a 255) (minusp a))
         (throwf 'eval-error "~s: ~a out of range for IPv4 address"
                 'str-inaddr-net addr))
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
                                                   "~s: \
                                                   \ ~a out of range \
                                                   \ for IPv6 address"
                                                   'str-in6addr-net
                                                   addr))
                                           (cons (logand val #xFFFF)
                                                 (ash val -16))))
                                       addr)))
             (cand-prefix [pieces 0..(trunc (+ w 15) 16)])
             (prefix (if (search cand-prefix '(0 0)) pieces cand-prefix)))
        `@(sys:in6addr-condensed-text prefix)/@(or width w)`))))

(defun inaddr-str (str)
  (labels ((invalid ()
             (error "~s: invalid address ~s" 'inaddr-str str))
           (mkaddr (octets port)
             (unless [all octets (op <= 0 @1 255)]
               (invalid))
             (unless (<= 0 port 65535)
               (invalid))
             (new sockaddr-in
                  addr (+ (ash (pop octets) 24)
                          (ash (pop octets) 16)
                          (ash (pop octets) 8)
                          (car octets))
                  port port))
           (mkaddr-pf (octets prefix port)
             (unless [all octets (op <= 0 @1 255)]
               (invalid))
             (unless (<= 0 prefix 32)
               (invalid))
             (unless (<= 0 port 65535)
               (invalid))
             (let* ((addr (+ (ash (or (pop octets) 0) 24)
                             (ash (or (pop octets) 0) 16)
                             (ash (or (pop octets) 0) 8)
                             (or (car octets) 0))))
               (new sockaddr-in
                    addr (logand addr (ash -1 (- 32 prefix)))
                    port port
                    prefix prefix))))
    (cond
      ((r^$ #/\d+\.\d+\.\d+\.\d+:\d+/ str)
       (tree-bind (addr port) (split* str (rpos #\: str))
         (mkaddr [mapcar toint (spl #\. addr)] (toint port))))
      ((r^$ #/\d+\.\d+\.\d+\.\d+(:\d+)?/ str)
       (mkaddr [mapcar toint (spl #\. str)] 0))
      ((r^$ #/\d+(\.\d+(\.\d+(\.\d+)?)?)?\/\d+/ str)
       (tree-bind (addr prefix) (spl #\/ str)
         (mkaddr-pf [mapcar toint (spl #\. addr)] (toint prefix) 0)))
      ((r^$ #/\d+(\.\d+(\.\d+(\.\d+)?)?)?\/\d+:\d+/ str)
       (tree-bind (addr prefix port) (split-str-set str ":/")
         (mkaddr-pf [mapcar toint (spl #\. addr)] (toint prefix) (toint port))))
      (t (invalid)))))

(defun in6addr-str (str)
  (labels ((invalid ()
             (error "~s: invalid address ~s" 'in6addr-str str))
           (mkaddr-full (pieces)
             (unless [all pieces (op <= 0 @1 #xffff)]
               (invalid))
             (unless (eql 8 (length pieces))
               (invalid))
             (new sockaddr-in6
                  addr (reduce-left (op + @2 (ash @1 16)) pieces)))
           (mkaddr-brev (pieces-x pieces-y)
             (let ((len-x (len pieces-x))
                   (len-y (len pieces-y)))
               (unless (<= (+ len-x len-y) 7)
                 (invalid))
               (let* ((val-x (reduce-left (op + @2 (ash @1 16)) pieces-x 0))
                      (val-y (reduce-left (op + @2 (ash @1 16)) pieces-y 0))
                      (addr (cond
                              ((null pieces-x) val-y)
                              ((null pieces-y) (ash val-x (* 16 (- 8 len-x))))
                              (t (+ val-y
                                    (ash val-x (* 16 (- 8 len-x))))))))
                 (new sockaddr-in6
                      addr addr))))
           (str-to-pieces (str)
              (unless (empty str)
                [mapcar (lop toint 16) (spl #\: str)]))
           (octets-to-pieces (octets)
             (unless [all octets (op <= 0 @1 255)]
               (invalid))
             (list (+ (ash (pop octets) 8)
                      (pop octets))
                   (+ (ash (pop octets) 8)
                      (pop octets)))))
    (cond
      ((r^$ #/\[.*\]:\d+/ str)
       (tree-bind (addr-str port-str) (split* str (rpos #\: str))
         (let ((addr (in6addr-str [addr-str 1..-1]))
               (port (toint port-str)))
           (unless (<= 0 port 65535)
             (invalid))
           (set addr.port port)
           addr)))
      ((r^$ #/[^\/]+\/\d+/ str)
       (tree-bind (addr-str prefix-str) (split* str (rpos #\/ str))
         (let ((addr (in6addr-str addr-str))
               (prefix (toint prefix-str)))
           (unless (<= 0 prefix 128)
             (invalid))
           (upd addr.addr (logand (ash -1 (- 128 prefix))))
           (set addr.prefix prefix)
           addr)))
      ((r^$ #/[\da-fA-F]*(:[\da-fA-F]*)*/ str)
       (upd str (regsub #/::/ "@"))
       (let* ((str-splat (regsub #/::/ "@" str))
              (maj-pieces (spl #\@ str-splat)))
         (caseql (len maj-pieces)
           (1 (mkaddr-full (str-to-pieces (car maj-pieces))))
           (2 (mkaddr-brev (str-to-pieces (car maj-pieces))
                           (str-to-pieces (cadr maj-pieces))))
           (t (invalid)))))
      ((r^$ #/::0*[fF][fF][fF][fF]:\d+\.\d+\.\d+\.\d+/ str)
       (let* ((bigsplit (split* str (rpos #\: str)))
              (4part (cadr bigsplit))
              (octets [mapcar toint (spl #\. 4part)])
              (pieces (cons #xffff (octets-to-pieces octets))))
         (mkaddr-brev nil pieces)))
      (t (invalid)))))

(defplace (sock-peer sock) body
  (getter setter
    ^(macrolet ((,getter () ^(sock-peer ',',sock))
                (,setter (val) ^(sock-set-peer ,',sock ,val)))
       ,body)))
