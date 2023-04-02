(cl:in-package #:giddy.protocol)


(defclass fundamental-message ()
  ((%sender :initarg :sender
            :reader sender)
   (%connection-name :initarg :connection-name
                     :reader connection-name)))

(defclass content-message (fundamental-message)
  ((%content :initarg :content
             :reader content)))

(defclass control-message (fundamental-message)
  ())

(defclass flush-message (control-message)
  ())

(defclass end-message (control-message)
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
  ((%connected-sinks :reader connected-sinks
                     :initarg :connected-sinks))
  (:default-initargs
   :connected-sinks (vect)))

(defclass fundamental-cell ()
  ((%flownet :initarg :flownet
             :accessor flownet)
   (%sinks :reader sinks)
   (%pipes :reader pipes)
   (%sink-names :reader sink-names)
   (%pipe-names :reader pipe-names)
   (%sinks-hash-table :reader sinks-hash-table)
   (%pipes-hash-table :reader pipes-hash-table))
  (:default-initargs :flownet nil))

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
   (%finished-channels :initform (make-hash-table :test 'equal)
                       :reader finished-channels)
   (%queue :initform (lparallel.queue:make-queue)
           :reader queue)
   (%lock :reader lock
          :initform (bt:make-lock)))
  (:default-initargs
   :merger nil
   :acceptor nil))

(defclass flownet (fundamental-cell)
  ((%cells :reader cells
           :initarg :cells)
   (%active-cells :accessor active-cells
                  :initarg :cells)
   (%lock :initform (bt:make-lock)
          :reader lock)
   (%condition-variable :initform (bt:make-condition-variable)
                        :reader condition-variable)))

(defclass fundamental-merger ()
  ())

(defclass fundamental-acceptor ()
  ())

(define-condition configuration-error (error)
  ())

(define-condition unknown-channel (configuration-error)
  ((%name :initarg :name
          :reader name))
  (:report
   (lambda (condition stream)
     (format stream "Channel ~a could not be found~%" (name condition)))))

(define-condition no-pipes (configuration-error)
  ())

(define-condition no-such-sink (error)
  ((%name :initarg :name
          :reader name)))

(define-condition no-such-pipe (error)
  ((%name :initarg :name
          :reader name)))
