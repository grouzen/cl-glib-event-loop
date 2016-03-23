;;;; event-source-test.lisp
;;;;
;;;; This file is part of the CL-GLIB-EVENT-LOOP library, released under MIT license.
;;;;
;;;; Author: Nedokushev Michael <michael.nedokushev@gmail.com>

(in-package #:cl-glib-event-loop-test)

(deftestsuite event-source-test (cl-glib-event-loop-test)
  ())


;; 
;; FIX:
;;   find out how to pass fields of testsuite inside threads
;;
(defconstant +io-event-source-test-hostname+ "localhost")
(defconstant +io-event-source-test-port+     3888)
(defconstant +io-event-source-test-message+  "io event test")

(deftestsuite io-event-source-test (event-source-test)
  ()
  (:setup
   (bt:make-thread
    #'(lambda ()
        (let* ((socket (usocket:socket-listen +io-event-source-test-hostname+
                                              +io-event-source-test-port+))
               (client (usocket:socket-accept socket :element-type 'character))
               (sconn  (usocket:socket-stream client)))
          (unwind-protect
               (progn
                 (format sconn "~A~%" +io-event-source-test-message+)
                 (force-output sconn)
                 ;; To be able to receive on client side something before socket will be closed
                 (sleep 1))
            (usocket:socket-close client)
            (usocket:socket-close socket)))))))


(addtest (io-event-source-test)
  receive-io-event-from-socket
  (let ((result nil))
    ;; To be able to connect to the server side
    (sleep 1)

    (let ((socket (usocket:socket-connect +io-event-source-test-hostname+
                                          +io-event-source-test-port+
                                          :element-type 'character)))
      (unwind-protect 
           (evloop:with-event-loop (evl)
             (evloop:attach-source evl (evloop:make-io-event-source
                                        #'(lambda ()
                                            (usocket:wait-for-input socket :timeout 3)
                                            (read-line (usocket:socket-stream socket) nil :end))
                                        ;; (let ((socket (usocket:socket-connect "localhost" 3888
                                        ;;                                       :element-type 'character)))
                                        ;;   (unwind-protect
                                        ;;        (progn
                                        ;;          (usocket:wait-for-input socket :timeout 3)
                                        ;;          (read-line (usocket:socket-stream socket) nil :end))
                                        ;;     (usocket:socket-close socket))))
                                        #'(lambda (ev s e)
                                            (declare (ignore s))
                                            (setf result e)
                                            (evloop:event-loop-stop ev)))))
        (usocket:socket-close socket))
      (ensure (string= result +io-event-source-test-message+)))))
