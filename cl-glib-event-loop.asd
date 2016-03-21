;;;; cl-glib-event-loop.asd
;;;;
;;;; This file is part of the CL-GLIB-EVENT-LOOP library, released under MIT license.
;;;;
;;;; Author: Nedokushev Michael <michael.nedokushev@gmail.com>

(in-package #:cl-user)

(asdf:defsystem cl-glib-event-loop
  :name "cl-glib-event-loop"
  :author "Michael Nedokushev <michael.nedokushev@gmail.com>"
  :license "Lisp-LGPL"
  :depends-on (:bordeaux-threads :alexandria)
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "event-loop")
                                     (:file "event-sources")))))

  
