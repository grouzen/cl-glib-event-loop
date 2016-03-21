;;;; event-loop.lisp
;;;;
;;;; This file is part of the CL-GLIB-EVENT-LOOP library, released under MIT license.
;;;;
;;;; Author: Nedokushev Michael <michael.nedokushev@gmail.com>

(in-package #:cl-glib-event-loop)

;;;;
;;;; This event-loop implementation based on GLib-like design.
;;;;
;;;; You should create event-loop's instance, attach various sources to it,
;;;; and call event-loop-run which will return only after event-loop-stop.
;;;; 

(defclass event-loop ()
  ((mode    :accessor mode    :initarg :mode    :initform :default)
   (sources :accessor sources :initarg :sources :initform nil)
   (prisrcs :accessor prisrcs :initarg :prisrcs :initform nil)
   (state   :accessor state   :initarg :state   :initform :stopped)
   (lock    :accessor lock    :initarg :lock    :initform (bt:make-lock))))

(defmethod print-object ((obj event-loop) stream)
  (with-slots (prisrcs state mode) obj
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "~A, mode: ~A, number of sources: ~A"
              state mode (length prisrcs)))))

(defmethod set-mode ((evloop event-loop) mode)
  (setf (mode evloop) mode))

(defmethod running-p ((evloop event-loop))
  (eql (state evloop) :running))

(defmethod event-loop-run ((evloop event-loop) &key (sleep .2))
  (with-slots (sources state lock) evloop
    (loop :for (k v) :on sources :by #'cddr
       :do (event-source-run evloop v))
    (setf state :running)
    (loop :while (eql state :running)
       :do (progn
             (event-loop-iterate evloop)
             (sleep sleep)))))

(defmethod event-loop-stop ((evloop event-loop))
  (with-slots (sources state) evloop
    (loop :for (k v) :on sources :by #'cddr
       :do (with-slots (thread alive) v
             (when (bt:thread-alive-p thread)
               (bt:destroy-thread thread)
               (setf thread nil
                     alive nil))))
    (setf state :stopped)))

(defmethod event-loop-iterate ((evloop event-loop))
  (with-slots (prisrcs mode lock) evloop
    (loop :for src :in prisrcs
       :do (with-slots (events callback) src
             (bt:with-lock-held (lock)
               (loop :while (> (length events) 0)
                  :do (funcall callback evloop src (pop events))))))))

(defmacro with-event-loop ((evloop &key (sleep .2)) &body body)
  `(let ((,evloop (make-instance 'evloop:event-loop)))
     (unwind-protect
          (progn
            ,@body
            (event-loop-run ,evloop :sleep ,sleep))
       (event-loop-stop ,evloop))))
