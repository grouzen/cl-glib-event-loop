;;;; suite.lisp
;;;;
;;;; This file is part of the CL-GLIB-EVENT-LOOP library, released under MIT license.
;;;;
;;;; Author: Nedokushev Michael <michael.nedokushev@gmail.com>

(in-package #:cl-glib-event-loop-test)

(deftestsuite cl-glib-event-loop-test ()
  ())

(defparameter *test-print-test-case-names* t)

(defun run-all-tests ()
  (describe (run-tests :suite 'cl-glib-event-loop-test)))

