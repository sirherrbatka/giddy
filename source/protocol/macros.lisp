(cl:in-package #:giddy.protocol)


(defmacro with-flownet ((flownet-form) &body body)
  (with-gensyms (!read-thread)
    `(let* ((*flownet* ,flownet-form)
            (tasks (tasks *flownet*))
            (,!read-thread
              (bt:make-thread (lambda ()
                                (iterate
                                  (for task = (lparallel.queue:pop-queue tasks))
                                  (lparallel:force task))))))
       (unwind-protect
            ,@body
         (bt:join-thread ,!read-thread)))))
