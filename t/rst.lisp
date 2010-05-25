;;;; t/rst.lisp
;;;;
;;;; This file is part of the wiki-parser library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:wiki-parser.test)

(deftestsuite re-structured-text-tests (wiki-parser-tests) ())

(addtest (re-structured-text-tests)
  simple-1
  (ensure-same '(wiki-parser.re-structured-text:toplevel
                 (wiki-parser.re-structured-text:paragraph "Hello world"))
               (wiki-parser:parse :re-structured-text
                                  "Hello world")))

(addtest (re-structured-text-tests)
  emphasis-1
  (ensure-same '(wiki-parser.re-structured-text:toplevel
                 (wiki-parser.re-structured-text:paragraph
                  (wiki-parser.re-structured-text:emphasis "Hello world")))
               (wiki-parser:parse :re-structured-text
                                  "*Hello world*")))

(addtest (re-structured-text-tests)
  strong-1
  (ensure-same '(wiki-parser.re-structured-text:toplevel
                 (wiki-parser.re-structured-text:paragraph
                  (wiki-parser.re-structured-text:strong "Hello world")))
               (wiki-parser:parse :re-structured-text
                                  "**Hello world**")))


(addtest (re-structured-text-tests)
  domain-depend-1
  (ensure-same '(wiki-parser.re-structured-text:toplevel
                 (wiki-parser.re-structured-text:paragraph
                  (wiki-parser.re-structured-text:domain-depend nil "Hello world")))
               (wiki-parser:parse :re-structured-text
                                  "`Hello world`")))

(addtest (re-structured-text-tests)
  domain-depend-2
  (ensure-same '(wiki-parser.re-structured-text:toplevel
                 (wiki-parser.re-structured-text:paragraph
                  (wiki-parser.re-structured-text:domain-depend :ref "Hello world")))
               (wiki-parser:parse :re-structured-text
                                  ":ref:`Hello world`")))

(addtest (re-structured-text-tests)
  domain-depend-3
  (ensure-same '(wiki-parser.re-structured-text:toplevel
                 (wiki-parser.re-structured-text:paragraph
                  (wiki-parser.re-structured-text:domain-depend :code-_. "Hello world")))
               (wiki-parser:parse :re-structured-text
                                  ":code-_.:`Hello world`")))

(addtest (re-structured-text-tests)
  inline-literal-1
  (ensure-same '(wiki-parser.re-structured-text:toplevel
                 (wiki-parser.re-structured-text:paragraph
                  (wiki-parser.re-structured-text:inline-literal "Hello world")))
               (wiki-parser:parse :re-structured-text
                                  "``Hello world``")))

(addtest (re-structured-text-tests)
  reference-1
  (ensure-same '(wiki-parser.re-structured-text:toplevel
                 (wiki-parser.re-structured-text:paragraph
                  (wiki-parser.re-structured-text:reference "wiki-parser")))
               (wiki-parser:parse :re-structured-text
                                  "wiki-parser_")))

(addtest (re-structured-text-tests)
  reference-2
  (ensure-same '(wiki-parser.re-structured-text:toplevel
                 (wiki-parser.re-structured-text:paragraph
                  (wiki-parser.re-structured-text:reference "wiki-parser")))
               (wiki-parser:parse :re-structured-text
                                  "`wiki-parser`_")))

(addtest (re-structured-text-tests)
  reference-3
  (ensure-same '(wiki-parser.re-structured-text:toplevel
                 (wiki-parser.re-structured-text:paragraph
                  (wiki-parser.re-structured-text:reference "Common Lisp")))
               (wiki-parser:parse :re-structured-text
                                  "`Common Lisp`_")))

(addtest (re-structured-text-tests)
  anonymous-reference-1
  (ensure-same '(wiki-parser.re-structured-text:toplevel
                 (wiki-parser.re-structured-text:paragraph
                  (wiki-parser.re-structured-text:anonymous-reference "wiki-parser")))
               (wiki-parser:parse :re-structured-text
                                  "wiki-parser__")))

(addtest (re-structured-text-tests)
  anonymous-reference-2
  (ensure-same '(wiki-parser.re-structured-text:toplevel
                 (wiki-parser.re-structured-text:paragraph
                  (wiki-parser.re-structured-text:anonymous-reference "my favourite programming language")))
               (wiki-parser:parse :re-structured-text
                                  "`my favourite programming language`__")))

(addtest (re-structured-text-tests)
  cross-reference-1
  (ensure-same '(wiki-parser.re-structured-text:toplevel
                 (wiki-parser.re-structured-text:paragraph
                  (wiki-parser.re-structured-text:cross-reference "inline internal target")))
               (wiki-parser:parse :re-structured-text
                                  "_`inline internal target`")))


(addtest (re-structured-text-tests)
  substitution-reference-1
  (ensure-same '(wiki-parser.re-structured-text:toplevel
                 (wiki-parser.re-structured-text:paragraph
                  (wiki-parser.re-structured-text:substitution-reference "Hello world")))
               (wiki-parser:parse :re-structured-text
                                  "|Hello world|_")))

(addtest (re-structured-text-tests)
  footnote-reference-1
  (ensure-same '(wiki-parser.re-structured-text:toplevel
                 (wiki-parser.re-structured-text:paragraph
                  (wiki-parser.re-structured-text:footnote-reference "1")))
               (wiki-parser:parse :re-structured-text
                                  "[1]_")))

(addtest (re-structured-text-tests)
  citation-reference-1
  (ensure-same '(wiki-parser.re-structured-text:toplevel
                 (wiki-parser.re-structured-text:paragraph
                  (wiki-parser.re-structured-text:citation-reference "Hello")))
               (wiki-parser:parse :re-structured-text
                                  "[Hello]_")))

(addtest (re-structured-text-tests)
  standalone-hyperlink-1
  (ensure-same '(wiki-parser.re-structured-text:toplevel
                 (wiki-parser.re-structured-text:paragraph
                  (wiki-parser.re-structured-text:standalone-hyperlink "http://www.lisp.org/")))
               (wiki-parser:parse :re-structured-text
                                  "http://www.lisp.org/")))

(addtest (re-structured-text-tests)
  section-1
  (ensure-same '(wiki-parser.re-structured-text:toplevel
                 (wiki-parser.re-structured-text:section "Hello world" #\= t))
               (wiki-parser:parse :re-structured-text
                                  "=============
 Hello world
=============")))

(addtest (re-structured-text-tests)
  section-2
  (ensure-same '(wiki-parser.re-structured-text:toplevel
                 (wiki-parser.re-structured-text:section "Hello world" #\= nil))
               (wiki-parser:parse :re-structured-text
                                  "Hello world
===========")))

(addtest (re-structured-text-tests)
  section-3
  (ensure-same '(wiki-parser.re-structured-text:toplevel
                 (wiki-parser.re-structured-text:section "Hello world" #\! nil))
               (wiki-parser:parse :re-structured-text
                                  "Hello world
!!!!!!!!!!!")))

(addtest (re-structured-text-tests)
  section-4
  (ensure-different 'wiki-parser.re-structured-text:section
                    (first (second (wiki-parser:parse :re-structured-text
                                                      "Hello world
==========")))))

(addtest (re-structured-text-tests)
  section-5
  (ensure-different 'wiki-parser.re-structured-text:section
                    (first (second (wiki-parser:parse :re-structured-text
                                                      "Hello world
======================")))))

