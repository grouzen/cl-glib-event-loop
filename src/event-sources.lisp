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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Attaching/detaching sources to/from an event loop instance
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Run/Stop event sources
;;

(defmethod event-source-run ((evloop event-loop) (evsource event-source))
  (with-slots (alive action events thread) evsource
    (setf alive t)
    (let ((new-thread (bt:make-thread
                       #'(lambda ()
                           (loop
                              :while alive
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Event sources
;;

(defmacro define-event-source (source-class priority make-args &body action)
  (let* ((make-name (alexandria:symbolicate 'make '- `,source-class))
         (inst-var  (gensym)))
    `(progn
       ;; Define a new class represents a particular event source            
       (defclass ,source-class (event-source) ())
       ;; Define a 'constructor/maker' for this event source,
       ;; which automatically does the following:
       ;; - creates an instance
       ;; - provides slots of the instance as variables available inside `make-body`
       ;; - sets a callback for the instance
       (defun ,make-name (cb &key ,@make-args)
         (let ((,inst-var (make-instance ',source-class)))
           (with-slots (priority callback action) ,inst-var
             (setf priority ,priority
                   callback cb
                   action   #'(lambda () ,@action))
             ,inst-var)))
       ;; Explicitly export the new class and its custom contstructor
       (export '(,source-class ,make-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic event sources
;;

;;
;; FIXME:
;;   incorporate "poll/select" mechanics for io-event-source
;;

;; (define-event-source io-event-source +io-priority+
;;     (io-source error-handler)
;;   (let* ((elt  (stream-element-type io-source))
;;          (rfn  (cond ((eq elt 'character) #'read-char)
;;                      (t #'read-byte))))
;;     (coerce (loop
;;                :for c = (funcall rfn io-source nil :eof)
;;                :while (not (eq c :eof))
;;                :collect c)
;;             'vector)))
                   
(define-event-source timer-event-source +timer-priority+
    (period)
  (sleep period))
        
(define-event-source ui-event-source +ui-priority+
    (getfn)
  (funcall getfn))

(define-event-source idle-event-source +idle-priority+
    ()
  t)
