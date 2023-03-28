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
                (input (make-hash-table :test 'eq)))
           (when (endp pipes)
             (error 'protocol:no-pipes))
           (iterate
             (for pipe in pipes)
             (for pipe-name = (protocol:name pipe))
             (for queue in queues)
             (cl-ds:mod-bind (container found value) (cl-ds:take-out-front! queue)
               (unless found
                 (return-from protocol:form-input (values nil messages)))
               (push value messages)
               (push (protocol:content value) (gethash pipe-name input))))
           (setf (protocol:input cell) (apply (implementation merger)
                                              (protocol:input cell)
                                              (hash-table-plist input)))
           (map nil
             (lambda (pipe queue) (setf (protocol:queue pipe) queue))
             pipes
             queues)
           (return-from protocol:form-input (values messages messages)))
      (map nil #'bt:release-lock locks))))
