;;;; t/core.lisp
;;;;
;;;; This file is part of the wiki-parser library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:wiki-parser.test
  (:use #:cl #:lift)
  (:export #:run-wiki-parser-tests))

(in-package #:wiki-parser.test)

(deftestsuite wiki-parser-tests () ())

(defun run-wiki-parser-tests  (&optional (suite 'wiki-parser-tests))
  (run-tests :suite suite
             :report-pathname nil))
