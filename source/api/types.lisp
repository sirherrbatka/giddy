(cl:in-package #:giddy.api)


(defclass list-merger (protocol:fundamental-merger)
  ((%implementation :initarg :implementation
                    :reader implementation))
  (:default-initargs :implementation #'identity))

(defclass collecting-cell (protocol:action-cell)
  ((%result :initarg :resut
            :reader result))
  (:default-initargs :result (vect)))

(defclass callback-cell (protocol:action-cell)
  ((%callback :initarg :callback
              :reader callback)))
