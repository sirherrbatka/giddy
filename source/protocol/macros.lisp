(cl:in-package #:giddy.protocol)


(defmacro execute-flownet ((flownet-form)
                           &body body)
  (with-gensyms (!threads)
    `(bind ((*flownet* ,flownet-form)
            (,!threads (make-threads *flownet*)))
       ,@body
       (mapcar #'bt:join-thread ,!threads)
       (iterate
         (with lock = (bt:make-lock))
         (until (bt:with-lock-held ((lock *flownet*))
                  (endp (active-cells))))
         (bt:condition-wait (condition-variable *flownet*) lock))
       *flownet*)))
