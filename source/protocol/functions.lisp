(cl:in-package #:giddy.protocol)


(defun make-message (sender connection-name content)
  (make 'message
        :sender sender
        :connection-name connection-name
        :content content))

(defun make-flownet (class sink-names pipe-names &rest cells)
  (make-instance class
                 :cells cells
                 :sink-names sink-names
                 :pipe-names pipe-names))

(defun run-flownet (*flownet* function)
  (let ((threads (make-threads *flownet*)))
    (unwind-protect
         (funcall function)
      (mapcar #'bt:join-thread threads)
      (iterate
        (with lock = (bt:make-lock))
        (until (bt:with-lock-held ((lock *flownet*))
                 (endp (active-cells *flownet*))))
        (bt:condition-wait (condition-variable *flownet*) lock)))
       *flownet*))
