(cl:in-package #:giddy.protocol)


(defun make-threads (flownet)
  (mapcar (lambda (cell &aux (queue (queue cell)))
            (bt:make-thread (lambda ()
                              (iterate
                                (for callback = (lparallel.queue:pop-queue queue))
                                (until (null callback))
                                (funcall callback)))))
          (cells flownet)))
