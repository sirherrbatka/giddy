(cl:in-package #:giddy.protocol)


(defun make-message (sender connection-name content)
  (make 'message
        :sender sender
        :connection-name connection-name
        :content content))

(defun make-flownet (class sink-names pipe-names &rest cells)
  (make-instance class
                 :cells cells
                 :sink-names sink-names
                 :pipe-names pipe-names))
