;;;; event-sources.lisp
;;;;
;;;; This file is part of the CL-GLIB-EVENT-LOOP library, released under MIT license.
;;;;
;;;; Author: Nedokushev Michael <michael.nedokushev@gmail.com>

(in-package #:cl-glib-event-loop)

(defconstant +default-priority+    0)
(defconstant +low-priority+      300)
(defconstant +high-priority+    -100)
(defconstant +io-priority+       100)
(defconstant +ui-priority+      -200)
(defconstant +timer-priority+    200)
(defconstant +idle-priority+     400)

(defclass event-source ()
  ((id       :accessor id       :initarg :id       :initform (gensym "evsource"))
   (thread   :accessor thread   :initarg :thread   :initform nil)
   (priority :accessor priority :initarg :priority :initform +default-priority+)
   (mode     :accessor mode     :initarg :mode     :initform :default)
   (alive    :accessor alive    :initarg :alive    :initform t)
   (events   :accessor events   :initarg :events   :initform nil)
   (action   :accessor action   :initarg :action   :initform nil)
   (callback :accessor callback :initarg :callback :initform nil)))

(defmacro define-event-source (source-class (&rest initargs) &body body)
  (let* ((direct-slots (loop :for (slot initform) :on initargs :by #'cddr
                          :collect (let ((slot-name (find-symbol (symbol-name slot))))
                                     (list slot-name :accessor slot-name
                                           :initarg slot :initform initform))))
         (make      (getf (car body) :make))
         (make-name (alexandria:symbolicate 'make '- `,source-class))
         (make-args (car make))
         (make-body (cdr make)))
    `(progn
       (defclass ,source-class (event-source) ,direct-slots)
       (defun ,make-name ,make-args ,@make-body)
       (export '(,source-class ,make-name)))))

;;
;; Attaching/detaching sources to/from the event loop's instance.
;;

(defmethod make-prisrcs% ((evloop event-loop))
  (with-slots (sources) evloop
    (sort (loop :for (k v) :on sources :by #'cddr :collect v)
          #'(lambda (a b)
              (< (priority a) (priority b))))))

(defmethod attach-source ((evloop event-loop) (evsource event-source))
  (with-slots (sources prisrcs lock) evloop
    (setf (getf sources (id evsource)) evsource
          prisrcs (make-prisrcs% evloop))))
  
(defmethod detach-source ((evloop event-loop) (evsource event-source))
  (with-slots (sources prisrcs) evloop
    (remf sources (id evsource))
    (setf prisrcs (make-prisrcs% evloop))))

(defmethod event-source-run ((evloop event-loop) (evsource event-source))
  (with-slots (alive action events thread) evsource
    (setf alive t)
    (let ((new-thread (bt:make-thread
                       #'(lambda ()
                           (loop :while alive
                              :do (let ((ret (funcall action)))
                                    (bt:with-lock-held ((lock evloop))
                                      (push ret events))))))))
      (setf thread new-thread))))

(defmethod event-source-stop ((evsource event-source))
  (with-slots (thread alive) evsource
    (when (and thread (bt:thread-alive-p thread))
      (bt:destroy-thread thread)
      (setf thread nil
            alive nil))))


(define-event-source ui-event-source (:priority +ui-priority+ :getfn nil)
  (:make ((callback &key getfn)
     (let ((instance (make-instance 'ui-event-source :callback callback)))
       (setf (action instance) (lambda () (funcall (or getfn (getfn instance)))))
       instance))))

(define-event-source io-event-source (:priority +io-priority+ :error-handler nil)
  (:make ((action callback &key (error-handler #'(lambda (c) (format nil "io-event-source error: ~A" c))) (mode :default))
     (let ((instance (make-instance 'io-event-source :mode mode :error-handler error-handler)))
       (setf (action instance) (lambda ()
                                 (handler-case (funcall action)
                                   (end-of-file (c) (funcall error-handler c))))
             (callback instance) callback)
       instance))))

;; (define-event-source io-event-source (:fh nil :priority +io-priority+ :error-handler nil)
;;   (:make ((fh callback &key (error-handler #'(lambda (c) (format nil "io-event-source error: ~A" c))) (mode :default))
;;      (labels ((get-read-fn (lambda (fh)
;;                              (let ((seltype (stream-element-type fh)))
;;                                (cond ((listp seltype) #'read-byte)
;;                                      ((eq seltype 'character) #'read-char)))))
;;               (handle-fh (lambda (fh)
;;                            (let* ((read-fn (get-read-fn fh))
;;                                   (update-c #'(lambda () (funcall read-fn fh nil :end))))
;;                              (do ((c   (funcall #'update-c) (funcall #'update-c)))
;;                                  ((eq c :end) t)
;;                                (make-array 128   
;;      (let ((instance (make-instance 'io-event-source :mode mode :error-handler error-handler)))
;;        (setf (action instance) (lambda ()
;;                                  (handler-case (handle-fh fh)
;;                                    (end-of-file (c) (funcall error-handler c))))
;;              (callback instance) callback)
;;        instance))))

(define-event-source timer-event-source (:priority +timer-priority+ :one-shot nil)
  (:make ((callback &key sleep (one-shot nil) (mode :default))
     (let ((instance (make-instance 'timer-event-source :one-shot one-shot :mode mode)))
       (setf (action   instance) (lambda ()
                                   (sleep sleep)
                                   (when (one-shot instance)
                                     (setf (alive  instance) nil
                                           (thread instance) nil)))
             (callback instance) callback)
       instance))))

(define-event-source idle-event-source (:priority +idle-priority+)
  (:make ((action callback &key (mode :default))
     (let ((instance (make-instance 'idle-event-source :mode mode)))
       (setf (action   instance) action
             (callback instance) callback)
       instance))))

