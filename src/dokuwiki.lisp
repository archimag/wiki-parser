;;; dokuwiki.lisp

(defpackage :wiki-parser.dokuwiki
  (:use :cl :iter)
  (:nicknames :dokuwiki)
  (:import-from :wiki-parser :define-mode :remake-lexer)
  (:export #:chapter
           #:paragraph))

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

(define-mode emphasis (80 :formatting)
 (:allowed :formatting)
 (:not-allowed emphasis)
 ;;(:entry "//(?=[^ ]*[^:]//)") ;; hack from sources dokuwiki, 
 (:entry "//(?=[^.]*//)") ;;; 
 (:exit "//"))

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
  
(define-mode unordered-listblock (10 :container)
  (:allowed :formatting :substition :disabled :protected)
  (:entry "\\n {2,}\\*"
          "\\n\\t{1,}\\*")
  (:exit "\\n")
  (:continue "\\n {2,}\\*"
             "\\n\\t{1,}\\*"))

(define-mode ordered-listblock (10 :container)
  (:allowed :formatting)
  (:entry "\\n {2,}\\-"
          "\\n\\t{1,}\\-")
  (:exit "\\n")
  (:continue "\\n {2,}\\-"
             "\\n\\t{1,}\\-"))

(define-mode table (60 :container)
  ;;(:allowed :formatting :substition :disabled :protected)
  (:allowed nil)
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

;;          "<code \\w*>(?=.*</code>)"
(define-mode code (200 :protected)
  (:entry "<code[^\\n]*>(?=.*</code>)")
  (:exit "</code>"))

(define-mode file (210 :protected)
  (:entry "<file>(?=.*</file>)")
  (:exit "</file>"))

(define-mode quoted (220 :container)
  (:allowed :formatting :substition :disabled :protected footnote preformatted unformatted)
  (:entry "\\n>{1,}")
  (:exit "\\n")
  (:continue "\\n>{1,}"))

(define-mode external-link (330 :substition)
  (:entry "\\[\\[(?=.*\\]\\])")
  (:special "(?:ht|f)tp(?:s?)://[0-9a-zA-Z](?:[-.\\w]*[0-9a-zA-Z])*(?::(?:0-9)*)*(?:/?)(?:[a-zA-Z0-9\\-\\.\\?\\,\\'/\\\\\\+&%\\$#_]*)?")
  (:exit "\\]\\]"))


;;; remake lexer

(remake-lexer 'toplevel)

;; union-same-items

(defparameter *union-same-items* '(preformatted unordered-listblock ordered-listblock quoted table))

(defun find-same-items (wikidoc &optional (start 0) (end (length wikidoc)))
  (let* ((pos1 (position-if #'(lambda (item)
                                (and (consp item)
                                     (find (car item) *union-same-items*)))
                            wikidoc
                            :start start
                            :end end))
         (pos2 (if pos1
                   (or (position-if #'(lambda (item)
                                        (not (and (consp item)
                                                  (eql (car item) (car (aref wikidoc pos1))))))
                                    wikidoc
                                    :start (1+ pos1)
                                    :end end)
                       end))))
    (if pos1
        (cons pos1 pos2))))
  
(defun union-same-item (wikidoc &optional (start 0) (end (length wikidoc)))
  (let ((curpos start)
        (result nil))
    (iter (for same = (find-same-items wikidoc curpos end))
          (while same)
          (when (> (car same) curpos)
            (iter (for pos from curpos below (car same))
                  (push (aref wikidoc pos)
                        result)))
          (push (cons (car (aref wikidoc (car same)))
                      (iter (for pos from (car same) below (cdr same))
                            (collect (cdr (aref wikidoc pos)))))
                result)
          (setf curpos
                (cdr same)))
    (when (< curpos end)
      (iter (for pos from curpos below end)
            (push (aref wikidoc pos)
                  result)))
    (if result
        (nreverse result)
        (coerce (subseq wikidoc start end) 'list))))
  

;;; make-paragraphs

(defun find-paragraph (wikidoc &optional (start 0) (end (length wikidoc)))
  (let* ((pos1 (position-if #'stringp
                            wikidoc
                            :start start
                            :end end))
         (pos2 (if pos1
                   (position 'eol
                             wikidoc
                             :start pos1
                             :end end))))
    (if pos1
        (progn
          (iter (while (and pos2
                            (< pos2 (1- end))
                            (not (eql (aref wikidoc (1+ pos2)) 'eol))))
                (setf pos2
                      (position 'eol
                                wikidoc
                                :start (1+ pos2)
                                :end end)))
          (cons pos1
                (or pos2 end))))))

          

(defun make-paragraphs (wikidoc &key (start 0) (end (length wikidoc)))
  (let ((curpos start)
        (result nil))
    (iter (for p = (find-paragraph wikidoc curpos end))
          (while p)
          (when (> (car p) curpos)
            (iter (for item in (union-same-item wikidoc curpos (car p)))
                  (push item
                        result)))
          (push (cons 'paragraph
                      (union-same-item wikidoc (car p) (cdr p)))
                result)
          (setf curpos
                (cdr p)))
    (iter (for item in (union-same-item wikidoc curpos end))
          (push item
                result))
    (if result
        (nreverse result)
        (subseq wikidoc start end))))
  

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
  (if raw-wiki
      (let ((marks)
        (level))
        (unless end
          (setf end (length raw-wiki)))
    (iter (for item in-vector raw-wiki)
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
    (setf marks (nreverse marks))
    (concatenate 'list
                 ;;(subseq raw-wiki start (or (car marks) end))
                 (make-paragraphs raw-wiki :start start :end (or (car marks) end))
                 (iter (for m on marks)
                       (collect (list* 'chapter
                                       ;;(header-strim (nth (car m) raw-wiki))
                                       (header-strim (aref raw-wiki  (car m)))
                                       (make-chapter-tree raw-wiki
                                                          :start (1+ (first m))
                                                          :end (second m)))))))))
                             
              
;;(defun post-parse (tree)

(defmethod wiki-parser:parse ((markup (eql :dokuwiki)) obj)
;;  (call-next-method))
  (make-chapter-tree (coerce (call-next-method) 'vector)))
;;  (let ((result (call-next-method)))
;;    (make-chapter-tree (coerce result 'vector))))


