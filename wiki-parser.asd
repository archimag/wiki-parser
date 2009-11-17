;;;; wiki-parser.asd
;;;;
;;;; This file is part of the wiki-parser library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defpackage #:wiki-parser-system
  (:use #:cl #:asdf))

(in-package #:wiki-parser-system)

(defsystem wiki-parser
  :depends-on (#:cl-ppcre #:iterate #:alexandria)
  :components ((:module :src
                        :components ((:file "packages")
                                     (:file "parser" :depends-on ("packages"))
                                     (:file "dokuwiki" :depends-on ("parser"))))))
