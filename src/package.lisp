;;;; package.lisp
;;;;
;;;; This file is part of the CL-GLIB-EVENT-LOOP library, released under MIT license.
;;;;
;;;; Author: Nedokushev Michael <michael.nedokushev@gmail.com>

(defpackage #:cl-glib-event-loop
  (:use #:cl)
  (:nicknames #:evloop)
  (:export #:make-event-source
           #:attach-source
           #:detach-source
           #:event-loop-run
           #:event-loop-stop
           #:event-loop-iterate
           #:set-mode
           #:running-p
           #:define-event-source
           #:with-event-loop
           ;; Classes
           #:event-loop))

