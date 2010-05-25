;;;; dokuwiki.lisp
;;;;
;;;; This file is part of the wiki-parser library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defpackage #:wiki-parser.dokuwiki
  (:use #:cl #:iter)
  (:nicknames #:dokuwiki)
  (:import-from #:wiki-parser #:define-mode #:remake-lexer)
  (:export #:chapter
           #:paragraph))

(in-package #:wiki-parser.dokuwiki)

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
  (:single "\\\\\\\\"))
  ;;(:single "\\\\\\\\(?=\\s)"))

(define-mode eol (370 :paragraphs)
  (:single "\\n"))

(define-mode hr (160 :container)
  (:single "\\n[ \\t]*-{4,}[ \\t]*(?=\\n)"))

(define-mode strong (70 :formatting)
  (:allowed :formatting :substition :disabled)
  (:not-allowed strong)
  (:entry "\\*\\*(?=.*\\*\\*)")
  (:exit "\\*\\*"))

(define-mode emphasis (80 :formatting)
 (:allowed :formatting :substition :disabled)
 (:not-allowed emphasis)
 (:entry "//(?=.*//)")
 (:exit "//"))

(define-mode underline (90 :formatting)
  (:allowed :formatting :substition :disabled)
  (:not-allowed underline)
  (:entry "__(?=.*__)")
  (:exit "__"))

(define-mode monospace (100 :formatting)
  (:allowed :formatting :substition :disabled)
  (:not-allowed monospace)
  (:entry "''(?=.*'')")
  (:exit "''"))

(define-mode subscript (110 :formatting)
  (:allowed :formatting :substition :disabled)
  (:not-allowed subscript)
  (:entry "<sub>(?=.*</sub>)")
  (:exit "</sub>"))

(define-mode superscript (120 :formatting)
  (:allowed :formatting :substition :disabled)
  (:not-allowed superscript)
  (:entry "<sup>(?=.*</sup>)")
  (:exit "</sup>"))
  
(define-mode unordered-listblock (10 :container)
  (:allowed :formatting :substition :disabled :protected)
  (:entry "\\n {2,}\\*"
          "\\n\\t{1,}\\*")
  (:exit "\\n")
  (:continue "\\n {2,}\\*"
             "\\n\\t{1,}\\*"))

(define-mode ordered-listblock (10 :container)
  (:allowed :formatting :substition :disabled :protected)
  (:entry "\\n {2,}\\-"
          "\\n\\t{1,}\\-")
  (:exit "\\n")
  (:continue "\\n {2,}\\-"
             "\\n\\t{1,}\\-"))

(define-mode table (60 :container)
  (:allowed :table)
  (:special "\\n[\\^\\|][^\\n]+[\\^\\|][^\\n]*(?=\\n)"))

(define-mode table-header-cell (63 :table)
  (:allowed :formatting :substition :disabled :protected)
  (:entry "\\n?\\^(?=[^\\n]*[\\^|\\|])")
  (:exit)
  (:exit-border "\\^"
                "\\|"))

(define-mode table-cell (65 :table)
  (:allowed :formatting :substition :disabled :protected)
  (:entry "\\n?\\|(?=[^\\n]*[\\^\\|])")
  (:exit-border "\\^"
                "\\|"))

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
  (:entry "<code[^\\n<>]*>(?=.*</code>)")
  (:exit "</code>"))

(define-mode file (210 :protected)
  (:entry "<file>(?=.*</file>)")
  (:exit "</file>"))

(define-mode quoted (220 :container)
  (:allowed :formatting :substition :disabled :protected footnote preformatted unformatted)
  (:entry "\\n>{1,}")
  (:exit "\\n")
  (:continue "\\n>{1,}"))

(define-mode internal-link (300 :substition)
  (:entry "\\[\\[(?=.*\\]\\])")
  (:exit "\\]\\]"))

(define-mode media (320 :substition)
  (:entry "{{(?=.*}})")
  (:exit "}}"))

(define-mode external-link (330 :substition)
  (:special "(?:ht|f)tp(?:s?)://[0-9a-zA-Z](?:[-.\\w]*[0-9a-zA-Z])*(?::(?:0-9)*)*(?:/?)(?:[a-zA-Z0-9\\-\\.\\?\\,/\\+&%\\$#\\*]*)?"))

(define-mode em-dash (340 :substition)
  (:single "---"))

(define-mode en-dash (350 :substition)
  (:single "--"))


;;; remake lexer

(remake-lexer 'toplevel)


;;; make-chapter-tree

(defun header-level (header)
  (let ((str (string-trim #(#\Space #\Tab) (second header))))
    (min (position #\= str :test-not #'char-equal)
         (- (length str)
            (position #\= str :test-not #'char-equal :from-end t)
            1))))

(defun header-strim (header)
    (let* ((str (string-trim #(#\Space #\Tab) (second header)))
           (n (min (position #\= str :test-not #'char-equal)
                   (- (length str)
                      (position #\= str :test-not #'char-equal :from-end t)
                      1))))
      (list (car header)
            (string-trim #(#\Space #\Tab) 
                         (subseq str n (- (length str) n))))))

(defun make-chapter-tree (wikidoc &optional (end nil))
  (let ((marks)
        (level))
    (iter (for tail on wikidoc)
          (while (not (eql tail end)))
          (let ((item (car tail)))
            (when (and (consp item)
                       (eq (car item) 'header)
                       (>= (header-level item)
                           (or level 0)))
              (push tail marks)
              (unless level
                (setf level
                      (header-level item))))))
    (setf marks (nreverse marks))
    (concatenate 'list
                 (ldiff wikidoc (or (car marks) end))
                 (iter (for m on marks)
                       (collect (list* 'chapter
                                       (header-strim (caar m))
                                       (make-chapter-tree (cdr (first m))
                                                          (or (second m) end))))))))
              
              
;;;; union-same-items

(defparameter *union-same-items* '(preformatted unordered-listblock ordered-listblock quoted table))

(defun union-same-items (wikidoc)
  (let ((result nil))
    (iter (for item in wikidoc)
          (cond
            ((atom item) (push item result))
            ((and (consp (car result))
                  (find (car item)
                        *union-same-items*)
                  (eql (car item)
                       (caar result))) (nconc (car result)
                  (if (third item)
                      (list (union-same-items (cdr item)))
                      (union-same-items (cdr item))))
             )
            ((find (car item)
                   *union-same-items*) (push (cons (car item)
                                                   (if (third item)
                                                       (list (union-same-items (cdr item)))
                                                       (cdr (union-same-items item))))
                   result))
            (t (push (union-same-items item)
                     result))))
    (nreverse result)))

;;;; make-paragraphs

(defun make-paragraphs (wikidoc)
  (let ((result nil))
    (flet ((paragraph-part-p (item)
             (or (stringp item)
                 (and (symbolp item)
                      (eql (gethash item *symbols-category*)
                           :substition))
                 (and (consp item)
                      (find (gethash (car item)
                                     *symbols-category*)
                            '(:formatting :substition)))))
           (append-to-last-paragraph (item)
             (nconc (car result)
                    (list item))))
      (iter (for item in wikidoc)
            (cond
              ((paragraph-part-p item) (if (and (consp (car result))
                                                (eql (caar result) 'paragraph))
                                           (append-to-last-paragraph item)
                                           (push (list 'paragraph item)
                                                 result)))
              ((and (eql item 'eol)
                    (consp (car result))
                    (eql (caar result) 'paragraph)
                    (not (eql (car (last (car result))) 'eol))) (append-to-last-paragraph item))
              (t (push item result))))
      (nreverse result))))
                
;;;; wiki-parser:parse

(defmethod wiki-parser:parse ((markup (eql :dokuwiki)) (obj string))
  (make-chapter-tree
   (make-paragraphs
    (union-same-items
     (call-next-method)))))
