;;;; re-structured-text.lisp
;;;;
;;;; This file is part of the wiki-parser library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(wiki-parser:define-parser #:wiki-parser.re-structured-text
  (:use #:cl #:iter)
  (:export #:paragraph))

(in-package #:wiki-parser.re-structured-text)

(define-toplevel-mode
  (:allowed :formatting :sections :paragraphs))

;;;; aux

(defun trim-backquotes (str)
  (let ((last (1- (length str))))
    (if (char= #\`
               (char str 0)
               (char str last))
        (subseq str 1 last)
        str)))

(defun trim-whitespaces (str)
  (string-trim #(#\Space #\Tab #\Return #\Newline)
               str))

(defun re-blank-line (pos)
  (declare (type integer pos))
  (ignore-errors
    (unless (char= #\Newline
                   (char ppcre::*string* pos))
      (return-from re-blank-line nil))
    (incf pos)
    (iter (with ch = (char ppcre::*string* pos))
          (while (not (char= #\Newline ch)))
          (unless (member ch '(#\Space #\Tab))
            (return-from re-blank-line nil))
          (incf pos))
    (1+ pos)))
        
;; (defun check-new-line (pos)
;;   (if (or (= pos 0)
;;           (char= #\Newline
;;                  (char ppcre::*string* (1- pos))))
;;       pos
;;       nil))

  
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
 (:entry "`(?=[^`]*`)"
         ":[\\w-_\\.]+:`(?=[^`]*`)")
 (:entry-attribute-parser (str)
    (when (char= #\: (char str 0))
      (intern (string-upcase
               (subseq str
                       1
                       (position #\: str :start 1 :test #'char=)))
              :keyword)))
 (:exit "`"))

;;;; inline-literal

(define-mode inline-literal (80 :formatting)
  (:entry "``(?=.*``)")
  (:exit "``"))

;;;; reference
                
(define-mode reference (70 :formatting)
  (:special "`.*`_"
            "\\w+[^\\s]+_")
  (:post-handler (item)
    (let* ((str (second item)))
      (list 'reference
            (trim-backquotes 
             (subseq str 0 (1- (length str))))))))

;;;; anonymous reference

(define-mode anonymous-reference (60 :formatting)
  (:special "`.*`__"
            "[^\\s]+__")
  (:post-handler (item)
    (let* ((str (second item)))
      (list 'anonymous-reference
            (trim-backquotes
             (subseq str 0 (- (length str) 2)))))))

;;;; cross-reference

(define-mode cross-reference (80 :formatting)  
  (:special "_`.*`"
            "_[^\\s]+")
  (:post-handler (item)
    (list 'cross-reference
          (trim-backquotes 
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

;;;; standalone-hyperlink

(define-mode standalone-hyperlink (330 :formatting)
  (:special "(?:ht|f)tp(?:s?)://[0-9a-zA-Z](?:[-.\\w]*[0-9a-zA-Z])*(?::(?:0-9)*)*(?:/?)(?:[a-zA-Z0-9\\-\\.\\?\\,/\\+&%\\$#\\*]*)?"))

;;;; unordered-list

(defparameter +bullet-symbols+
  '(#\* #\+ #\- #\• #\‣ #\⁃))

;; (defun re-bullet-item (pos)
;;   (let ((pos2 (position-if #'(lambda (ch)
;;                                (not (member ch '(#\Tab #\Space))))
;;                            ppcre::*string*
;;                            :start pos)))
;;     (unless (member (char ppcre::*string*
;;                           pos2)
;;                     +bullet-symbols+)
;;       (return-from re-bullet-item nil))
;;     (incf pos2)
    
      


;; (define-mode bullet-list (10 :paragraphs)
;;   (:special (:sequence
;;              (:filter re-blank-line)
;;              (:filter re-bullet-item))
;;              (:greedy-repetition
;;               0 nil
;;               (:sequence
;;                (:greedy-repetition
;;                 1 nil
;;                 (:filter re-blank-line))
;;                (:filter re-bullet-item)))))

;;;;; Section

(defparameter +section-adornment+
  '(#\! #\" #\# #\$ #\% #\&
    #\' #\( #\) #\* #\+ #\, 
    #\- #\. #\/ #\: #\; #\< 
    #\= #\> #\? #\@ #\[ #\\ 
    #\] #\^ #\_ #\` #\{ #\| 
    #\} #\~))

(defun regex-section-filter (pos)
  (ignore-errors
    (flet ((check-condition (flag)
             (unless flag (return-from regex-section-filter nil))))
      (let ((adornment nil)
            (title nil)
            (overline-p nil))
        (check-condition (char= #\Newline
                                (char ppcre::*string* pos)))
        (incf pos)
        (check-condition (null (char= #\Newline
                                      (char ppcre::*string* pos))))
        (when (member (char ppcre::*string* pos)
                      +section-adornment+)
          (let ((pos2 (position (char ppcre::*string* pos)
                                ppcre::*string*
                                :start (1+ pos)
                                :test-not #'char=)))
            (check-condition (char= #\Newline
                                    (char ppcre::*string* pos2)))
            (setf adornment (subseq ppcre::*string* pos pos2)
                  overline-p t
                  pos (+ pos2 1))))
        (unless overline-p
          (check-condition (let ((sp (ppcre:scan "\\s" title)))
                             (or (null sp)
                                 (> sp 0)))))
        (let ((pos2 (position #\Newline
                              ppcre::*string*
                              :start pos
                              :test #'char=)))
          (setf title
                (subseq ppcre::*string* pos pos2))
          (setf pos
                (+ pos2 1)))
        (let ((pos2 (position #\Newline
                              ppcre::*string*
                              :start pos
                              :test #'char=)))
          (cond
            (adornment (check-condition (string= adornment
                                                 (subseq ppcre::*string* pos pos2))))
            (t (check-condition (> pos2 pos))
               (setf adornment
                     (subseq ppcre::*string* pos pos2))
               (check-condition (member (char adornment 0)
                                        +section-adornment+))
               (check-condition (null (find (char adornment 0)
                                            adornment
                                            :test-not #'char=)))))
          (setf pos pos2))
        (check-condition (if overline-p
                              (<= (length title)
                                  (length adornment))
                              (= (length title)
                                 (length adornment))))
        pos))))

(define-mode section (30 :sections)
  (:special (:filter regex-section-filter))
  (:post-handler (item)
    (let* ((lines (cdr (ppcre:split "\\n" (second item))))
           (decoration (car (last lines)))
           (overline-p (= (length lines) 3))
           (title (if overline-p
                      (second lines)
                      (first lines))))
      (list 'section
            (trim-whitespaces title)
            (char decoration 0)
            overline-p))))
      
;;;;; Paragraphs

(define-mode eol (370 :paragraphs)
  (:single "\\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; parse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(init-parser)

(defun make-paragraphs (wikidoc)
  (let ((result nil))
    (flet ((paragraph-part-p (item)
             (or (stringp item)
                 (and (consp item)
                      (find (gethash (car item)
                                     *symbols-category*)
                            '(:formatting)))))
           (append-to-last-paragraph (item)
             (nconc (car result)
                    (list item))))
      (iter (for item in wikidoc)
            (cond
              ((eql item 'eol) nil)
              ((paragraph-part-p item) 
               (if (and (consp (car result))
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

(defmethod wiki-parser:parse ((markup (eql :re-structured-text)) (obj string))
  (make-paragraphs
   (wiki-parser:parse #.*package* obj)))
