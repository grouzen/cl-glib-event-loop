;;;; cl-glib-event-loop-test.asd
;;;;
;;;; This file is part of the CL-GLIB-EVENT-LOOP library, released under MIT license.
;;;;
;;;; Author: Nedokushev Michael <michael.nedokushev@gmail.com>

(in-package #:cl-user)

(asdf:defsystem #:cl-glib-event-loop-test
  :name    "cl-glib-event-loop-test"
  :author  "Michael Nedokushev <michael.nedokushev@gmail.com>"
  :license "MIT"
  :depends-on (:cl-glib-event-loop :lift :usocket)
  :components ((:module "t"
                        :serial t
                        :components ((:file "package")
                                     (:file "suite")
                                     (:file "event-source-test")))))

  
