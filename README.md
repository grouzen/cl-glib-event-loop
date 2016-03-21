# cl-glib-event-loop

Implementation of [the GLIB's Main Event Loop](https://developer.gnome.org/glib/stable/glib-The-Main-Event-Loop.html)
written in Common Lisp.

# Usage

```commonlisp
(ql:quickload :cl-glib-event-loop)

;; Create a new event-loop instance (context)
(let ((evl (make-instance 'evloop:event-loop))) 
  ;; Attach as much as you want a number of event sources to the event loop 
  (evloop:attach-source evl (evloop:make-timer-event-source
                             #'(lambda (evloop src event)
                                 (declare (ignore evloop src event))
                                 (format t "Time is ticking...~%"))
                             :sleep 1.0))
  ;; Make it alive!
  (evloop:event-loop-run evl))

;; Same with `with-event-loop` macro
(evloop:with-event-loop (evl)
  (evloop:attach-source evl (evloop:make-timer-event-source
                             #'(lambda (evloop src event)
                                 (declare (ignore evloop src event))
                                 (format t "Time is ticking...~%"))
                             :sleep 1.0))
  (evloop:event-loop-run evl))

```

There are 4 different predefined event sources: ui-event-source, io-event-source,
timer-event-source, idle-event-source; and you can easily define your own kind of
event source by using `evloop:define-event-source macro`.
