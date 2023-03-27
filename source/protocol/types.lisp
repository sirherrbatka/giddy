(cl:in-package #:giddy.protocol)


(defclass fundamental-message ()
  ((%sender :initarg :sender
            :reader sender)
   (%connection-name :initarg :connection-name
                     :reader connection-name)))

(defclass content-message (fundamental-message)
  ((%content :initarg :content
             :reader content)
   (%attributes :initarg :attributes
                :initform nil
                :reader attributes)))

(defclass control-message (fundamental-message)
  ())

(defclass flush-message (control-message)
  ())

(defclass channel ()
  ((%name :initarg :name
          :reader name)
   (%cell :initarg :cell
          :reader cell)))

(defclass sink (channel)
  ((%connected-pipes :reader connected-pipes
                     :initarg :connected-pipes))
  (:default-initargs
   :connected-pipes (vect)))

(defclass pipe (channel)
  ((%queue :initarg :queue
           :accessor queue)
   (%lock :initarg :lock
          :reader lock))
  (:default-initargs
   :lock (bt:make-lock)
   :queue (cl-ds.queues.2-3-tree:make-transactional-2-3-queue)))

(defclass fundamental-cell ()
  ((%sinks :reader sinks)
   (%pipes :reader pipes)
   (%sink-names :reader sink-names)
   (%pipe-names :reader pipe-names)
   (%sinks-hash-table :reader sinks-hash-table)
   (%pipes-hash-table :reader pipes-hash-table)))

(defclass action-cell (fundamental-cell)
  ((%sink-names :initarg :sink-names
                :reader sink-names)
   (%pipe-names :initarg :pipe-names
                :reader pipe-names)
   (%acceptor :reader acceptor
              :initarg :acceptor)
   (%merger :reader merger
            :initarg :merger)
   (%input :accessor input
           :initform nil)
   (%parallel :initarg :parallel
              :reader parallel)
   (%lock :reader lock
          :initform (bt:make-lock)))
  (:default-initargs
   :parallel t))

(defclass parallel-action-cell (action-cell)
  ())

(defclass flownet (fundamental-cell)
  ((%cells :reader cells
           :initarg :cells)))

(defclass fundamental-merger ()
  ())

(defclass fundamental-acceptor ()
  ())

(define-condition unknown-channel (error)
  ((%name :initarg :name
          :reader name))
  (:report
   (lambda (condition stream)
     (format stream "Channel ~a could not be found~%" (name condition)))))

(define-condition no-pipes (error)
  ())
