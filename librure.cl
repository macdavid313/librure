;;;; librure.cl
(in-package #:cl-user)

(defpackage #:librure
  (:use #:cl
        #:excl
        #:ff))

(in-package #:librure)

(def-foreign-type size_t :unsigned-nat)

(defconstant +RURE_FLAG_CASEI+      #.(ash 1 0))
(defconstant +RURE_FLAG_MULTI+      #.(ash 1 1))
(defconstant +RURE_FLAG_DOTNL+      #.(ash 1 2))
(defconstant +RURE_FLAG_SWAP_GREED+ #.(ash 1 2))
(defconstant +RURE_FLAG_SPACE+      #.(ash 1 2))
(defconstant +RURE_FLAG_UNICODE+    #.(ash 1 2))
(defconstant +RURE_DEFAULT_FLAGS+   +RURE_FLAG_UNICODE+)

(def-foreign-type rure_match
  (:struct
   (start size_t)
   (end  size_t)))

;;; rure
(def-foreign-call rure_compile_must ((pattern (* :char) simple-string))
  :returning :foreign-address
  :strings-convert nil
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_compile ((pattern (* :unsigned-char) (simple-array (unsigned-byte 8) (*)))
                                (length size_t)
                                (flags :unsigned-int fixnum)
                                (options :aligned)
                                (error :aligned))
  :returning :foreign-address
  :strings-convert t
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_free ((re :aligned))
  :returning :void
  :arg-checking nil
  :call-direct t)

;;; rure APIs
(def-foreign-call rure_is_match ((re :aligned)
                                 (haystack (* :unsigned-char) (simple-array (unsigned-byte 8) (*)))
                                 (length size_t)
                                 (start size_t))
  :returning (:char boolean)
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_find ((re :aligned)
                             (haystack (* :unsigned-char) (simple-array (unsigned-byte 8) (*)))
                             (length size_t)
                             (start size_t)
                             (match (* rure_match)))
  :returning (:char boolean)
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_find_captures ((re :aligned)
                                      (haystack (* :unsigned-char) (simple-array (unsigned-byte 8) (*)))
                                      (length size_t)
                                      (start size_t)
                                      (captures :aligned))
  :returning (:char boolean)
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_shortest_match ((re :aligned)
                                       (haystack (* :unsigned-char) (simple-array (unsigned-byte 8) (*)))
                                       (length size_t)
                                       (start size_t)
                                       (end (* size_t)))
  :returning (:char boolean)
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_capture_name_index ((re :aligned) (name (* :char)))
  :returning :int
  :strings-convert nil
  :arg-checking nil
  :call-direct t)

;;; rure_iter_capture_names
(def-foreign-call rure_iter_capture_names_new ((re :aligned))
  :returning :foreign-address
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_iter_capture_names_free ((it :aligned))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_iter_capture_names_next ((it :aligned) (name (* (* :char))))
  :returning (:char boolean)
  :strings-convert nil
  :arg-checking nil
  :call-direct t)

;;; rure_iter
(def-foreign-call rure_iter_new ((re :aligned))
  :returning :foreign-address
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_iter_free ((it :aligned))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_iter_next ((it :aligned)
                                  (haystack (* :unsigned-char))
                                  (length size_t)
                                  (match (* rure_match)))
  :returning (:char boolean)
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_iter_next_captures ((it :aligned)
                                           (haystack (* :unsigned-char))
                                           (length size_t)
                                           (captures :aligned))
  :returning (:char boolean)
  :arg-checking nil
  :call-direct t)

;;; rure_captures
(def-foreign-call rure_captures_new ((re :aligned))
  :returning :foreign-address
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_captures_free ((captures :aligned))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_captures_at ((captures :aligned) (i size_t) (match (* rure_match)))
  :returning (:char boolean)
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_captures_len ((captures :aligned))
  :returning size_t
  :arg-checking nil
  :call-direct t)

;;; rure_options
(def-foreign-call rure_options_new (:void)
  :returning :foreign-address
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_options_free ((options :aligned))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_options_size_limit ((options :aligned) (limit size_t))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_options_dfa_size_limit ((options :aligned) (limit size_t))
  :returning :void
  :arg-checking nil
  :call-direct t)

;;; rure_set
(def-foreign-call rure_compile_set ((patterns (* (* :unsigned-char)))
                                    (patterns_lengths (* size_t))
                                    (patterns_count size_t)
                                    (flags :unsigned-int)
                                    (options :aligned)
                                    (error :aligned))
  :returning :foreign-address
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_set_free ((re :aligned))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_set_is_match ((re :aligned)
                                     (haystack (* :unsigned-char))
                                     (length size_t)
                                     (start size_t))
  :returning (:char boolean)
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_set_matches ((re :aligned)
                                    (haystack (* :unsigned-char))
                                    (length size_t)
                                    (start size_t)
                                    (matches (* :char)))
  :returning (:char boolean)
  :strings-convert nil
  :arg-checking nil
  :call-direct t)

;;; rure_error
(def-foreign-call rure_error_new (:void)
  :returning :foreign-address
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_error_free ((err :aligned))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_error_message ((err :aligned))
  :returning ((* :char))
  :arg-checking nil
  :call-direct t)

;;; misc
(def-foreign-call rure_escape_must ((pattern (* :char)))
  :returning ((* :char))
  :strings-convert nil
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_cstring_free ((s (* :char)))
  :returning :void
  :strings-convert nil
  :arg-checking nil
  :call-direct t)

(eval-when (:load-toplevel :execute)
  (do-symbols (sym)
    (when (match-re "^\\+?rure" (string sym) :case-fold t)
      (export sym))))
