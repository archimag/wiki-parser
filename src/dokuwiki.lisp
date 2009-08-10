;;; dokuwiki.lisp

(defpackage :wiki-parser.dokuwiki
  (:use :cl :iter)
  (:nicknames :dokuwiki)
  (:import-from :wiki-parser :define-mode :remake-lexer))

(in-package :wiki-parser.dokuwiki)

(defparameter *lexer* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *symbols-category* (make-hash-table)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; dokuwiki

(define-mode toplevel (0)
  (:allowed :container :baseonly :paragraphs :formatting :substition :protected :disabled))

(define-mode footnote (150 :formatting)
  (:allowed :container :formatting :substition :protected :disabled)
  (:not-allowed footnote)
  (:entry "\\(\\((?=.*\\)\\))")
  (:exit "\\)\\)"))

(define-mode header (50 :baseonly)
  (:special "[ \\t]*={2,}[^\\n]+={2,}[ \\t]*(?=\\n)"))

(define-mode notoc (30 :substition)
  (:single "~~NOTOC~~"))

(define-mode nocache (40 :substition)
  (:single "~~NOCACHE~~"))

(define-mode linebreak (140 :substition)
  (:single "\\\\\\\\(?=\\s)"))

(define-mode eol (370 :paragraphs)
  (:single "\\n"))

(define-mode hr (160 :container)
  (:single "\\n[ \\t]*-{4,}[ \\t]*(?=\\n)"))

(define-mode strong (70 :formatting)
  (:allowed :formatting)
  (:not-allowed strong)
  (:entry "\\*\\*(?=.*\\*\\*)")
  (:exit "\\*\\*"))

;; (define-mode emphasis 80
;;  (:allowed :formatting)
;;  (:not-allowed emphasis)
;;   (:entry "//(?=[^\x00]*[^:]//)")
;;   (:exit "//"))

(define-mode underline (90 :formatting)
  (:allowed :formatting)
  (:not-allowed underline)
  (:entry "__(?=.*__)")
  (:exit "__"))

(define-mode monospace (100 :formatting)
  (:allowed :formatting)
  (:not-allowed monospace)
  (:entry "''(?=.*'')")
  (:exit "''"))

(define-mode subscript (110 :formatting)
  (:allowed :formatting)
  (:not-allowed subscript)
  (:entry "<sub>(?=.*</sub>)")
  (:exit "</sub>"))

(define-mode superscript (120 :formatting)
  (:allowed :formatting)
  (:not-allowed superscript)
  (:entry "<sup>(?=.*</sup>)")
  (:exit "</sup>"))
  
(define-mode listblock (10 :container)
  (:allowed :formatting)
  (:entry "\\n {2,}[\\-\\*]"
          "\\n\\t{1,}[\\-\\*]")
  (:exit "\\n")
  (:continue "\\n {2,}[\\-\\*]"
             "\\n\\t{1,}[\\-\\*]"))

(define-mode table (60 :container)
  (:allowed :formatting :substition :disabled :protected)
  (:entry "\\n\\^"
          "\\n\\|")
  (:exit "\\n")
  (:continue "\\n\\^" "\\n\\|"
             " {2,}" "[\\t ]+"
             "\\^" "\\|"))

(define-mode unformatted (170 :disabled)
  (:entry "<nowiki>(?=.*</nowiki>)")
  (:exit "</nowiki>"))

(define-mode unformattedalt (171 :disabled)
  (:entry "%%(?=.*%%)")
  (:exit "%%"))

(define-mode html (190 :protected)
  (:entry "<html>(?=.*</html>)")
  (:exit "</html>"))

(define-mode preformatted (20 :protected)
  (:entry "\\n  (?![\\*\\-])"
          "\\n\\t(?![\\*\\-])")
  (:exit "\\n")
  (:continue "\\n  "
             "\\n\\t"))

(define-mode code (200 :protected)
  (:entry "<code>(?=.*</code>)")
  (:exit "</code>"))

(define-mode file (210 :protected)
  (:entry "<file>(?=.*</file>)")
  (:exit "</file>"))

(define-mode quoted (220 :container)
  (:allowed :formatting :substition :disabled :protected footnote preformatted unformatted)
  (:entry "\\n>{1,}")
  (:exit "\\n")
  (:continue "\\n>{1,}"))

;;; remake lexer


(remake-lexer 'toplevel)



;;; parse

(defun header-level (header)
  (let ((str (string-trim #(#\Space #\Tab) (second header))))
    (min (position #\= str :test-not #'char-equal)
         (- (length str)
            (position #\= str :test-not #'char-equal :from-end t)
            1))))

(defun find-header (wikidoc min-level &key (start 0) end)
  (position-if #'(lambda (item)
                   (and (consp item)
                        (eql (car item) 'header)
                        (>= (header-level item)
                            min-level)))
               wikidoc
               :start start
               :end end))

(defun make-chapter-tree (raw-wiki &key (start 0) end)
  (let ((marks nil)
        (level nil))
    (iter (for item in raw-wiki)
          (for pos from 0)
          (while (or (not end)
                     (< pos end)))

          (if (>= pos start)
              (when (and (consp item)
                         (eql (car item) 'header)
                         (>= (header-level item)
                             (or level 0)))
                (push pos marks)
                (unless level
                  (setf level
                        (header-level item))))))
    (concatenate 'list
                 (subseq raw-wiki start (car marks))
                 (iter (for m on (nreverse marks))
                       (print m)
                       (collect (cons 'chapter
                                      (cons (nth (car m) raw-wiki)
                                            (make-chapter-tree raw-wiki
                                                               :start (1+ (car m))
                                                               :end (car (cdr m))))))))))
                             
              
;;(defun post-parse (tree)

(defmethod wiki-parser:parse ((markup (eql :dokuwiki)) obj)
  (make-chapter-tree (call-next-method)))
