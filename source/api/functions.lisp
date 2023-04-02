(cl:in-package #:giddy.api)


(defun send (name content)
  (let* ((cell protocol:*cell*))
    (protocol:send-message cell
                           (protocol:sink cell name)
                           (make 'protocol:content-message
                                 :content content
                                 :sender cell
                                 :connection-name name))))
