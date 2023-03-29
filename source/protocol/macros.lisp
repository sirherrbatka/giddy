(cl:in-package #:giddy.protocol)


(defmacro execute-flownet ((flownet-form)
                           &body body)
  `(let ((*flownet* ,flownet-form))
     ,@body
     (iterate
       (with lock = (bt:make-lock))
       (until (bt:with-lock-held ((lock *flownet*))
                (endp (active-cells))))
       (bt:condition-wait (condition-variable *flownet*) lock))
     *flownet*))
