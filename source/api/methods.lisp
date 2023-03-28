(cl:in-package #:giddy.api)


(defmethod protocol:form-input ((cell protocol:action-cell)
                                (merger list-merger))

  (let* ((pipes (protocol:pipes cell))
         (locks (mapcar (compose #'bt:acquire-lock #'protocol:lock)
                        pipes))
         (queues (mapcar (lambda (pipe) (cl-ds:replica (protocol:queue pipe) t))
                         pipes)))
    (unwind-protect
         (let* ((messages '())
                (input (or (protocol:input cell)
                           (make-hash-table :test 'eq))))
           (when (endp pipes)
             (error 'no-pipes))
           (iterate
             (for pipe in pipes)
             (for pipe-name = (protocol:name pipe))
             (for queue in queues)
             (cl-ds:mod-bind (container found value) (cl-ds:take-out-front! queue)
               (when found
                 (push value messages)
                 (push (protocol:content value) (gethash pipe-name input)))))
           (setf (protocol:input cell) input)
           (return-from protocol:form-input (values t messages)))
      (progn
        (map nil
             (lambda (pipe queue) (setf (protocol:queue pipe) queue))
             pipes
             queues)
        (map nil #'bt:release-lock locks)))))
