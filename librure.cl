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
                                (options :foreign-address)
                                (error :foreign-address))
  :returning :foreign-address
  :strings-convert t
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_free ((re :foreign-address))
  :returning :void
  :arg-checking nil
  :call-direct t)

;;; rure APIs
(def-foreign-call rure_is_match ((re :foreign-address)
                                 (haystack (* :unsigned-char) (simple-array (unsigned-byte 8) (*)))
                                 (length size_t)
                                 (start size_t))
  :returning (:char boolean)
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_find ((re :foreign-address)
                             (haystack (* :unsigned-char) (simple-array (unsigned-byte 8) (*)))
                             (length size_t)
                             (start size_t)
                             (match (* rure_match)))
  :returning (:char boolean)
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_find_captures ((re :foreign-address)
                                      (haystack (* :unsigned-char) (simple-array (unsigned-byte 8) (*)))
                                      (length size_t)
                                      (start size_t)
                                      (captures :foreign-address))
  :returning (:char boolean)
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_shortest_match ((re :foreign-address)
                                       (haystack (* :unsigned-char) (simple-array (unsigned-byte 8) (*)))
                                       (length size_t)
                                       (start size_t)
                                       (end (* size_t)))
  :returning (:char boolean)
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_capture_name_index ((re :foreign-address) (name (* :char)))
  :returning :int
  :strings-convert nil
  :arg-checking nil
  :call-direct t)

;;; rure_iter_capture_names
(def-foreign-call rure_iter_capture_names_new ((re :foreign-address))
  :returning :foreign-address
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_iter_capture_names_free ((it :foreign-address))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_iter_capture_names_next ((it :foreign-address) (name (* (* :char))))
  :returning (:char boolean)
  :strings-convert nil
  :arg-checking nil
  :call-direct t)

;;; rure_iter
(def-foreign-call rure_iter_new ((re :foreign-address))
  :returning :foreign-address
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_iter_free ((it :foreign-address))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_iter_next ((it :foreign-address)
                                  (haystack (* :unsigned-char))
                                  (length size_t)
                                  (match (* rure_match)))
  :returning (:char boolean)
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_iter_next_captures ((it :foreign-address)
                                           (haystack (* :unsigned-char))
                                           (length size_t)
                                           (captures :foreign-address))
  :returning (:char boolean)
  :arg-checking nil
  :call-direct t)

;;; rure_captures
(def-foreign-call rure_captures_new ((re :foreign-address))
  :returning :foreign-address
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_captures_free ((captures :foreign-address))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_captures_at ((captures :foreign-address) (i size_t) (match (* rure_match)))
  :returning (:char boolean)
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_captures_len ((captures :foreign-address))
  :returning size_t
  :arg-checking nil
  :call-direct t)

;;; rure_options
(def-foreign-call rure_options_new (:void)
  :returning :foreign-address
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_options_free ((options :foreign-address))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_options_size_limit ((options :foreign-address) (limit size_t))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_options_dfa_size_limit ((options :foreign-address) (limit size_t))
  :returning :void
  :arg-checking nil
  :call-direct t)

;;; rure_set
(def-foreign-call rure_compile_set ((patterns (* (* :unsigned-char)))
                                    (patterns_lengths (* size_t))
                                    (patterns_count size_t)
                                    (flags :unsigned-int)
                                    (options :foreign-address)
                                    (error :foreign-address))
  :returning :foreign-address
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_set_free ((re :foreign-address))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_set_is_match ((re :foreign-address)
                                     (haystack (* :unsigned-char))
                                     (length size_t)
                                     (start size_t))
  :returning (:char boolean)
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_set_matches ((re :foreign-address)
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

(def-foreign-call rure_error_free ((err :foreign-address))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call rure_error_message ((err :foreign-address))
  :returning :foreign-address
  :strings-convert nil
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

;; (eval-when (:load-toplevel :execute)
;;   (do-symbols (sym)
;;     (when (match-re "^\\+?rure" (string sym) :case-fold t)
;;       (export sym))))

;;; High-level APIs
(defstruct (rure (:constructor %make-rure))
  (ptr 0 :type (integer 0 *) :read-only t)
  flags
  return)

(defun finalize-rure (rure)
  (declare (type rure rure))
  (rure_free (rure-ptr rure)))

(defmacro with-rure-error ((var) &body body)
  `(let ((,var (rure_error_new)))
     (unwind-protect (progn ,@body)
       (rure_error_free ,var))))

(defun lisp-string-to-haystack (str &optional (start 0)
                                      (end (length str))
                                      (external-format :default))
  (string-to-octets str :start start
                        :end end
                        :null-terminate nil
                        :external-format external-format))

(defun make-rure (pattern case-fold ignore-whitespace multiple-lines single-line return)
  (flet ((flag-to-value (flag)
           (case flag
             (:default +RURE_DEFAULT_FLAGS+)
             (:case-fold +RURE_FLAG_CASEI+)
             (:single-line +RURE_FLAG_DOTNL+)
             (:multiple-lines +RURE_FLAG_MULTI+)
             (:ignore-whitespace +RURE_FLAG_SPACE+))))
    (let ((haystack (lisp-string-to-haystack pattern))
          (flags (list :default))
          (ptr 0))
      (when case-fold
        (push :case-fold flags))
      (when single-line
        (push :single-line flags))
      (when multiple-lines
        (push :multiple-lines flags))
      (when ignore-whitespace
        (push :ignore-whitespace flags))
      (with-rure-error (err)
        (setq ptr (rure_compile haystack          ; pattern usb8 array
                                (length haystack) ; length
                                (reduce 'logand flags :key #'flag-to-value) ; flags
                                0    ; options (we are using the default options)
                                err  ; error
                                ))
        (when (zerop ptr)
          (error (native-to-string (rure_error_message err))))
        (let ((rure (%make-rure :ptr ptr :flags flags :return return)))
          (excl:schedule-finalization rure 'finalize-rure)
          rure)))))

(defun compile-rure (string &key case-fold
                            ignore-whitespace
                            multiple-lines
                            single-line
                            (return :unknown))
  (make-rure string
             case-fold
             ignore-whitespace
             multiple-lines
             single-line
             return))

(defun rure-find (rure haystack return)
  (declare (type rure rure)
           (type (simple-array (unsigned-byte 8) (*)) haystack))
  (with-stack-fobjects ((match 'rure_match))
    (when (rure_find (rure-ptr rure) haystack (length haystack) 0 match)
      (let ((start (fslot-value-typed 'rure_match :foreign match 'start))
            (end (fslot-value-typed 'rure_match :foreign match 'end)))
        (ecase return
          (:string (values t (octets-to-string (subseq haystack start end))))
          (:index (values t (cons start end))))))))

(defun match-re (regexp input &key (return :string)
                                case-fold
                                single-line
                                multiple-lines
                                ignore-whitespace
                                (start 0)
                                (end (length input))
                                back-end
                                start-unbounded
                                end-unbounded)
  (declare (ignore back-end start-unbounded end-unbounded)
           (type (or string rure) regexp))
  (let ((rure (if* (rure-p regexp)
                 then regexp
                 else (make-rure regexp
                                 case-fold
                                 ignore-whitespace
                                 multiple-lines
                                 single-line
                                 return)))
        (haystack (lisp-string-to-haystack input start end)))
    (rure-find rure haystack return)))
