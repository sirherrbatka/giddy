(cl:in-package #:giddy.api)


(defun respond (name content)
  (let* ((cell protocol:*cell*))
    (protocol:send-message cell
                           (protocol:sink cell name)
                           (make 'protocol:content-message
                                 :content content
                                 :sender cell
                                 :connection-name name))))

(defun send (name message)
  (let* ((cell protocol:*flownet*))
    (protocol:send-message cell
                           (protocol:sink cell name)
                           message)))

(defun send-content (name content)
  (let* ((cell protocol:*flownet*))
    (protocol:send-message cell
                           (protocol:sink cell name)
                           (make 'protocol:content-message
                                 :content content
                                 :sender cell
                                 :connection-name name))))

(defun end ()
  (iterate
    (with cell = protocol:*flownet*)
    (for sink in (protocol:sinks cell))
    (protocol:send-message cell
                           sink
                           (make 'protocol:end-message
                                 :sender cell
                                 :connection-name (protocol:name sink)))))

(defmethod reset-input (&aux (cell protocol:*cell*))
  (protocol:reset-input cell (protocol:merger cell)))

(defun make-cell (class &rest keys)
  (apply #'make class
         :queue (lparallel.queue:make-queue)
         keys))

(defun make-flownet (sinks pipes cells)
  (declare (optimize (debug 3)))
  (lret ((result (make 'protocol:flownet
                       :cells cells
                       :sink-names sinks
                       :pipe-names pipes)))))
