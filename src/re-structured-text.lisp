;;;; re-structured-text.lisp
;;;;
;;;; This file is part of the wiki-parser library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(wiki-parser:define-parser #:wiki-parser.re-structure-text
  (:use #:cl #:iter))

(in-package #:wiki-parser.re-structure-text)

(define-toplevel-mode
  (:allowed :formatting))

(defmethod wiki-parser:parse ((markup (eql :re-structured-text)) (obj string))
  (wiki-parser:parse #.*package*
                     obj))

;;;; aux

(defun remove-backquote-around (str)
  (let ((last (1- (length str))))
    (if (char= #\`
               (char str 0)
               (char str last))
        (subseq str 1 last)
        str)))
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; inline markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; strong

(define-mode strong (80 :formatting)
  (:allowed :formatting )
  (:not-allowed strong)
  (:entry "\\*\\*(?=.*\\*\\*)")
  (:exit "\\*\\*"))

;;;; emphasis

(define-mode emphasis (80 :formatting)
 (:allowed :formatting)
 (:not-allowed emphasis)
 (:entry "\\*(?=.*\\*)")
 (:exit "\\*"))

;;;; domain-depend

(define-mode domain-depend (90 :formatting)
 (:entry "`(?=.*`)")
 (:exit "`"))

;;;; inline-literal

(define-mode inline-literal (80 :formatting)
  (:entry "``(?=.*``)")
  (:exit "``"))

;;;; reference
                
(define-mode reference (70 :formatting)
  (:special "`.*`_"
            "[^\\s]+_")
  (:post-handler (item)
    (let* ((str (second item)))
      (list 'reference
            (remove-backquote-around 
             (subseq str 0 (1- (length str))))))))

;;;; anonymous reference

(define-mode anonymous-reference (60 :formatting)
  (:special "`.*`__"
            "[^\\s]+__")
  (:post-handler (item)
    (let* ((str (second item)))
      (list 'anonymous-reference
            (remove-backquote-around
             (subseq str 0 (- (length str) 2)))))))

;;;; cross-reference

(define-mode cross-reference (80 :formatting)  
  (:special "_`.*`"
            "_[^\\s]+")
  (:post-handler (item)
    (list 'cross-reference
          (remove-backquote-around 
           (subseq (second item) 1)))))


;;;; substitution-reference

(define-mode substitution-reference (80 :formatting)
  (:entry "\\|(?=.*\\|_)")
  (:exit "\\|_"))

;;;; footnote-reference

(define-mode footnote-reference (60 :formatting)
  (:entry "\\[(?=\\d+\\]_)")
  (:exit "\\]_"))

;;;; citation-reference

(define-mode citation-reference (65 :formatting)
  (:entry "\\[(?=.*\\]_)")
  (:exit "\\]_"))

(define-mode standalone-hyperlink (330 :formatting)
  (:special "(?:ht|f)tp(?:s?)://[0-9a-zA-Z](?:[-.\\w]*[0-9a-zA-Z])*(?::(?:0-9)*)*(?:/?)(?:[a-zA-Z0-9\\-\\.\\?\\,/\\+&%\\$#\\*]*)?"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; init parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(init-parser)