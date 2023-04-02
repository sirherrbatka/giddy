(cl:in-package #:giddy.protocol)


(defmethod initialize-instance :after ((cell fundamental-cell)
                                       &rest initargs
                                       &key &allow-other-keys)
  (declare (ignore initargs))
  (iterate
    (with sinks = (make-hash-table :test 'eq))
    (for sink-name in (sink-names cell))
    (for sink = (make-instance 'sink
                               :name sink-name
                               :cell cell))
    (setf (gethash sink-name sinks) sink)
    (finally
     (setf (slot-value cell '%sinks) (hash-table-keys sinks)
           (slot-value cell '%sinks-hash-table) sinks)))
  (iterate
    (with pipes = (make-hash-table :test 'eq))
    (for pipe-name in (pipe-names cell))
    (for pipe = (make-instance 'pipe
                               :name pipe-name
                               :cell cell))
    (setf (gethash pipe-name pipes) pipe)
    (finally
     (setf (slot-value cell '%pipes) (hash-table-values pipes)
           (slot-value cell '%pipes-hash-table) pipes))))

(defmethod initialize-instance :after ((object flownet)
                                       &rest initargs
                                       &key
                                       &allow-other-keys)
  (declare (ignore initargs))
  (bind ((pipes (make-hash-table :test 'eq))
         (sinks (make-hash-table :test 'eq))
         ((:flet connect-pipe (pipe))
          (iterate
            (with pipe-name = (name pipe))
            (for sink in (gethash pipe-name sinks))
            (connect sink pipe)))
         ((:flet connect-sink (sink))
          (iterate
            (with sink-name = (name sink))
            (for pipe in (gethash sink-name sinks))
            (connect sink pipe))))
    (iterate
      (for cell in (cells object))
      (setf (flownet cell) object)
      (iterate
        (for sink in (sinks cell))
        (for sink-name = (name sink))
        (push sink (gethash sink-name sinks))))
    (iterate
      (for cell in (cells object))
      (iterate
        (for pipe in (pipes cell))
        (connect-pipe pipe)
        (push pipe (gethash (name pipe) pipes))))
    (let ((flownet-sinks (sinks object))
          (flownet-pipes (cl-ds.alg:to-hash-table
                          (pipe-names object)
                          :hash-table-value (lambda (pipe-name)
                                              (lret ((result (gethash pipe-name pipes)))
                                                (if (null result)
                                                    (error 'unknown-channel :name pipe-name))))
                          :test 'eq)))
      (map nil #'connect-sink flownet-sinks)
      (setf (slot-value object '%pipes-hash-table) flownet-pipes
            (slot-value object '%pipes) (hash-table-values flownet-pipes)))))

(defmethod connect ((sink sink) (pipe pipe))
  (vector-push-extend sink (connected-sinks pipe))
  (vector-push-extend pipe (connected-pipes sink))
  nil)

(defmethod send-message ((sender-cell fundamental-cell)
                         sink
                         message)
  (iterate
    (for pipe in-vector (connected-pipes sink))
    (for cell = (cell pipe))
    (react-to-message cell sender-cell message sink pipe)))

(defmethod form-input ((cell action-cell)
                       merger
                       &optional (message nil message-p)
                         sink
                         pipe)
  (declare (ignore sink pipe))
  (if message-p
      (values nil nil '())
      (values (content message) t (list message))))

(defmethod react-to-message ((receiver-cell action-cell)
                             sender-cell
                             (message flush-message)
                             (sink sink)
                             (pipe pipe))
  (flet ((impl (&aux messages)
           (unwind-protect
                (iterate
                  (handler-case
                      (bind ((*cell* receiver-cell)
                             ((:values input input-formed m)
                              (form-input receiver-cell (merger receiver-cell))))
                        (setf messages m)
                        (if input-formed
                            (let ((decision (input-accepted-p receiver-cell
                                                              (merger receiver-cell)
                                                              (acceptor receiver-cell)
                                                              input)))
                              (econd ((eq decision :accept)
                                      (when (perform-action receiver-cell (input receiver-cell))
                                        (reset-input receiver-cell (merger receiver-cell))))
                                     ((eq decision :reject)
                                      (reset-input receiver-cell (merger receiver-cell)))
                                     ((eq decision :wait)
                                      (reset-input receiver-cell (merger receiver-cell)))))
                            (finish))
                        (iterate
                          (for sink in (sinks receiver-cell))
                          (send-message receiver-cell sink message)))
                    (configuration-error (e) (declare (ignore e))
                      nil)))
             (iterate
               (for sink in (sinks receiver-cell))
               (send-message receiver-cell sink message)))))
    (lparallel.queue:push-queue #'impl (queue receiver-cell))))

(defmethod notify-end ((cell fundamental-cell))
  (remove-active-cell (flownet cell) cell))

(defmethod remove-active-cell ((flownet flownet) cell)
  (bt:with-lock-held ((lock flownet))
    (setf #1=(active-cells flownet) (remove cell #1# :test 'eq)))
  (bt:condition-notify (condition-variable flownet)))

(defmethod react-to-message ((receiver-cell action-cell)
                             sender-cell
                             (message end-message)
                             (sink sink)
                             (pipe pipe))
  (bt:with-lock-held ((lock receiver-cell))
    (when (shiftf (gethash (cons pipe sink) (finished-channels receiver-cell)) t)
      (return-from react-to-message nil)))
  (flet ((impl (messages)
           (unwind-protect
                (iterate
                  (handler-case
                      (bind ((*cell* receiver-cell)
                             ((:values input input-formed m)
                              (form-input receiver-cell (merger receiver-cell))))
                        (setf messages m)
                        (if input-formed
                            (let ((decision (input-accepted-p receiver-cell
                                                              (merger receiver-cell)
                                                              (acceptor receiver-cell)
                                                              input)))
                              (econd ((eq decision :accept)
                                      (when (perform-action receiver-cell
                                                            (merger receiver-cell)
                                                            input)
                                        (reset-input receiver-cell (merger receiver-cell))))
                                     ((eq decision :reject)
                                      (reset-input receiver-cell (merger receiver-cell)))
                                     ((eq decision :wait)
                                      (reset-input receiver-cell (merger receiver-cell)))))
                            (finish))
                        (iterate
                          (for sink in (sinks receiver-cell))
                          (send-message receiver-cell sink message)))
                    (configuration-error (e) (declare (ignore e))
                      nil)))
             (when (= (~> receiver-cell finished-channels hash-table-count)
                      (reduce #'+ (pipes receiver-cell) :key (compose #'length #'connected-sinks)))
               (notify-end receiver-cell)))))
    (lparallel.queue:push-queue #'impl (queue receiver-cell))
    (lparallel.queue:push-queue nil (queue receiver-cell))))

(defmethod react-to-message ((receiver-cell action-cell)
                             sender-cell
                             (message content-message)
                             (sink sink)
                             (pipe pipe))
  (flet ((impl (&aux messages)
           (bt:with-lock-held ((lock receiver-cell))
             (handler-case
                 (bind ((*cell* receiver-cell)
                        ((:values input-formed m)
                         (form-input receiver-cell
                                     (merger receiver-cell)
                                     message)))
                   (setf messages m)
                   (if input-formed
                       (let ((decision (input-accepted-p receiver-cell
                                                         (merger receiver-cell)
                                                         (acceptor receiver-cell)
                                                         (input receiver-cell))))
                         (econd ((eq decision :accept)
                                 (perform-action (merger receiver-cell)
                                                 receiver-cell
                                                 (input receiver-cell)))
                                ((eq decision :reject)
                                 (reset-input receiver-cell (merger receiver-cell)))
                                ((eq decision :wait)
                                 nil)))
                       nil))
               (configuration-error (e) (declare (ignore e))
                 nil)))))
    (lparallel.queue:push-queue #'impl (queue receiver-cell))))

(defmethod react-to-message ((receiver-cell action-cell)
                             sender-cell
                             (message fundamental-message)
                             (sink sink)
                             (pipe pipe))
  nil)

(defmethod reset-input ((cell action-cell) merger)
  (setf (input cell) nil))

(defmethod input-accepted-p ((action-cell action-cell)
                             merger
                             acceptor
                             input)
  :accept)

(defmethod sink ((cell fundamental-cell)
                 name)
  (or (gethash name (sinks-hash-table cell))
      (error 'no-such-sink :name name)))

(defmethod pipe ((cell fundamental-cell)
                 name)
  (or (gethash name (pipes-hash-table cell))
      (error 'no-such-pipe :name name)))
