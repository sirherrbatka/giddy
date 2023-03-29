(cl:in-package #:giddy.protocol)


(defgeneric name (connection))
(defgeneric queue (pipe))
(defgeneric (setf queue) (queue pipe))
(defgeneric pipes-lock (cell))
(defgeneric sink-push (sink value))
(defgeneric connected-pipes (sink))
(defgeneric connect (sink pipe))
(defgeneric lock (object))

(defgeneric sender (message))
(defgeneric content (message))

(defgeneric sinks (cell))
(defgeneric sinks-hash-table (cell))
(defgeneric pipes-hash-table (cell))
(defgeneric pipes (cell))
(defgeneric sink-names (cell))
(defgeneric pipe-names (cell))

(defgeneric send-message (sender-cell sink message))
(defgeneric react-to-message (receiver-cell sender-cell message sink pipe))

(defgeneric perform-action (action-cell input))
(defgeneric form-input (action-cell merger))
(defgeneric reset-input (action-cell merger))
(defgeneric input-accepted-p (action-cell merger acceptor input))
(defgeneric acceptor (action-cell))
(defgeneric merger (action-cell))
(defgeneric notify-end (cell))
(defgeneric remove-active-cell (flownet cell))

(defgeneric active-cells (flownet))
(defgeneric (setf active-cells) (new-value flownet))

(defgeneric condition-variable (flownet))
