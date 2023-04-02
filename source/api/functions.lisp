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

(defmethod reset-input (&aux (cell protocol:*cell*))
  (protocol:reset-input cell (protocol:merger cell)))

(defun make-cell (class &rest keys &key queue-capacity)
  (apply #'make class
         :queue (lparallel.queue:make-queue :fixed-capacity queue-capacity)
         keys))
