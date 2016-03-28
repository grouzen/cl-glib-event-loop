;;;; event-source-test.lisp
;;;;
;;;; This file is part of the CL-GLIB-EVENT-LOOP library, released under MIT license.
;;;;
;;;; Author: Nedokushev Michael <michael.nedokushev@gmail.com>

(in-package #:cl-glib-event-loop-test)

(deftestsuite event-source-test (cl-glib-event-loop-test)
  ())

(deftestsuite idle-event-source-basic-test (event-source-test)
  ())

(addtest (idle-event-source-basic-test)
  event-source-test
  (let ((result nil))
    (evloop:with-event-loop (evl)
      (evloop:attach-source evl (evloop:make-idle-event-source
                                 #'(lambda (ev s e)
                                     (declare (ignore s e))
                                     (setf result "idle event")
                                     (evloop:event-loop-stop ev)))))
    (sleep 1)
    (ensure-same result "idle event")))

;; (deftestsuite io-event-source-test (event-source-test)
;;   ())


;; (deftestsuite io-event-source-socket-test (io-event-source-test)
;;   ((hostname "localhost")
;;    (port     3888)
;;    (message  "io event test"))
;;   (:setup
;;    (bt:make-thread
;;     #'(lambda ()
;;         (let* ((socket (usocket:socket-listen hostname% port%))
;;                (client (usocket:socket-accept socket :element-type 'character))
;;                (sconn  (usocket:socket-stream client)))
;;           (unwind-protect
;;                (progn
;;                  (format sconn "~A" message%)
;;                  (force-output sconn)
;;                  ;; To be able to receive on client side something before socket will be closed
;;                  (sleep 1))
;;             (usocket:socket-close client)
;;             (usocket:socket-close socket))))
;;     :initial-bindings (list (cons 'hostname% hostname)
;;                             (cons 'port%     port)
;;                             (cons 'message%  message)))))


;; (addtest (io-event-source-socket-test)
;;   receive-io-event-from-socket
;;   (let ((result nil))
;;     ;; To be able to connect to the server side
;;     (sleep 1)

;;     (let ((socket (usocket:socket-connect hostname port :element-type 'character)))
;;       (unwind-protect 
;;            (evloop:with-event-loop (evl)
;;              (evloop:attach-source evl (evloop:make-io-event-source
;;                                         #'(lambda (ev s e)
;;                                             (declare (ignore s))
;;                                             (setf result e)
;;                                             (evloop:event-loop-stop ev))
;;                                         :io-source (usocket:socket-stream socket))))
;;         (usocket:socket-close socket))
;;       (ensure (string= result message)))))


;; (deftestsuite io-event-source-string-input-stream-test (io-event-source-test)
;;   ((message "io event test")
;;    (in-stream (make-string-input-stream "io event test"))))

;; (addtest (io-event-source-string-input-stream-test)
;;   read-io-event-from-string-input-stream
;;   (let ((result nil))
;;     (evloop:with-event-loop (evl)
;;       (evloop:attach-source evl (evloop:make-io-event-source
;;                                  #'(lambda (ev s e)
;;                                      (declare (ignore s))
;;                                      (setf result e)
;;                                      (format t "CALLBACK: ~A~%" result)
;;                                      (evloop:event-loop-stop ev))
;;                                  :io-source in-stream)))
;;     (sleep 1)
;;     (ensure (string= result message))))


;; (deftestsuite io-event-source-stdin-stream-test (io-event-source-test)
;;   ((message "io event test")))

;; (addtest (io-event-source-stdin-stream-test)
;;   read-io-event-from-stdin-stream
;;   (let ((result nil))
;;     (evloop:with-event-loop (evl :detach t)
      
