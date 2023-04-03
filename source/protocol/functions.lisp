(cl:in-package #:giddy.protocol)


(defun run-flownet (*flownet* function)
  (let ((threads (make-threads *flownet*))
        (killed nil))
    (unwind-protect
         (handler-case
             (funcall function)
           (error (e)
             (map nil
                  (lambda (thread) (ignore-errors (bt:destroy-thread thread)))
                  threads)
             (setf killed t)
             (error e)))
      (map nil
           (lambda (thread) (ignore-errors (bt:join-thread thread)))
           threads)
      (unless killed
        (iterate
          (with lock = (bt:make-lock))
          (until (bt:with-lock-held ((lock *flownet*))
                   (endp (active-cells *flownet*))))
          (bt:condition-wait (condition-variable *flownet*) lock))))
       *flownet*))
