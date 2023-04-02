(cl:in-package #:giddy.api)


(defmethod protocol:form-input ((cell protocol:action-cell)
                                (merger list-merger)
                                &optional (message nil message-p) sink pipe)
  (declare (ignore sink))
  (bind ((input.messages
          (ensure (protocol:input cell)
            (iterate
              (with result = (make-hash-table :test 'eq))
              (for pipe in (protocol:pipes cell))
              (setf (gethash (protocol:name pipe) result) '())
              (finally (return (cons result '())))))))
    (when message-p
      (push (protocol:content message)
            (gethash (protocol:name pipe) (car input.messages)))
      (push message (cdr input.messages)))
    (if (block m
          (~>> input.messages car (maphash-values (lambda (v) (when (endp v) (return-from m nil)))))
          t)
        (values (~> input.messages car hash-table-plist) t (cdr input.messages))
        (values nil nil nil))))

(defmethod protocol:perform-action ((cell collecting-cell) merger input)
  (vector-push-extend input (result cell))
  t)
