(cl:in-package #:giddy.protocol)


(defun flush (receiver-cell message &optional (more (constantly nil)))
  (flet ((impl (&aux messages)
           (unwind-protect
                (bt:with-lock-held ((lock receiver-cell))
                  (handler-case
                      (bind ((*cell* receiver-cell)
                             ((:values input-formed m)
                              (form-input receiver-cell (merger receiver-cell))))
                        (setf messages m)
                        (if input-formed
                            (let ((decision (input-accepted-p receiver-cell
                                                              (merger receiver-cell)
                                                              (acceptor receiver-cell)
                                                              (input receiver-cell))))
                              (econd ((eq decision :accept)
                                      (perform-action receiver-cell (input receiver-cell))
                                      (reset-input receiver-cell (merger receiver-cell)))
                                     ((eq decision :reject)
                                      (reset-input receiver-cell (merger receiver-cell)))
                                     ((eq decision :wait)
                                      (reset-input receiver-cell (merger receiver-cell)))))
                            nil))
                    (configuration-error (e) (declare (ignore e))
                      nil)))
             (iterate
               (for sink in (sinks receiver-cell))
               (send-message receiver-cell sink message))
             (funcall more))))
    (if (parallel receiver-cell)
        (lparallel:future (impl))
        (impl))))
