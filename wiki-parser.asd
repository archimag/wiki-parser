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
                                     (:file "dokuwiki" :depends-on ("parser"))
                                     (:file "re-structured-text" :depends-on ("parser"))))))

(defmethod perform ((o test-op) (c (eql (find-system 'wiki-parser))))
  (operate 'load-op 'wiki-parser-test)
  (operate 'test-op 'wiki-parser-test :force t))

(defsystem wiki-parser-test
  :depends-on (#:wiki-parser #:lift)
  :components ((:module "t"
                        :components ((:file "core")
                                     (:file "rst" :depends-on ("core"))))))

(defmethod perform ((o test-op) (c (eql (find-system 'wiki-parser-test))))
  (operate 'load-op 'wiki-parser-test )
  (let* ((test-results (funcall (read-from-string "wiki-parser.test:run-wiki-parser-tests")))
         (errors (funcall (read-from-string "lift:errors")  test-results))
         (failures (funcall (read-from-string "lift:failures") test-results)))
    (if (or errors failures)
        (error "test-op failed: ~A"
               (concatenate 'list errors failures))
        (print test-results))))