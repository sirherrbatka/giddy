(cl:in-package #:giddy.protocol)


(defmacro with-flownet ((flownet-form)
                        &body body)
  (with-gensyms (!read-thread !flownet)
    `(let* ((*flownet* ,flownet-form)
            (,!flownet *flownet*)
            (,!read-thread
              (bt:make-thread (lambda ()
                                (iterate
                                  (with lock = (bt:make-lock))
                                  (until (bt:with-lock-held ((lock ,!flownet))
                                           (endp (active-cells))))
                                  (bt:condition-wait (condition-variable ,!flownet) lock))))))
       (unwind-protect
            ,@body
         (bt:join-thread ,!read-thread))
       *flownet*)))
