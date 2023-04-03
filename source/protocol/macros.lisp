(cl:in-package #:giddy.protocol)


(defmacro execute-flownet ((flownet-form)
                           &body body)
  `(run-flownet ,flownet-form (lambda () (,@body))))
