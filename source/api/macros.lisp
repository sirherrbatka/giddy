(cl:in-package #:giddy.api)


(defmacro execute (((first-cell &rest more-cells) &optional sinks pipes) &body body)
  `(protocol:run-flownet (make-flownet (list ,@sinks) (list ,@pipes)
                                       (list ,@(mapcar (lambda (def)
                                                         (bind (((first . rest) def))
                                                           `(make-cell ',first ,@rest)))
                                                       (cons first-cell more-cells))))
                         (lambda () ,@body)))
