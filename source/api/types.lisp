(cl:in-package #:giddy.api)


(defclass list-merger (protocol:fundamental-merger)
  ((%implementation :initarg :implementation
                    :reader implementation))
  (:default-initargs :implementation #'identity))
