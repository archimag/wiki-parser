;;; wiki-parser.asd

(defpackage :wiki-parser-system
  (:use :cl :asdf))

(in-package :wiki-parser-system)

(defsystem :wiki-parser
  :depends-on (#:cl-ppcre #:iterate #:alexandria)
  :components ((:module :src
                        :components ((:file "packages")
                                     (:file "parser" :depends-on ("packages"))
                                     (:file "dokuwiki" :depends-on ("parser"))))))
