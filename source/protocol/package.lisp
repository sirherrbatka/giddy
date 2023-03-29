(cl:defpackage #:giddy.protocol
  (:use #:cl #:giddy.aux-package)
  (:export
   #:fundamental-message
   #:content-message
   #:channel
   #:control-message
   #:flush-message
   #:fundamental-cell
   #:action-cell
   #:flownet
   #:fundamental-merger
   #:fundamental-acceptor
   #:unknown-channel
   #:configuration-error
   #:no-pipes
   #:with-flownet

   #:name
   #:queue
   #:pipes-lock
   #:sink-push
   #:connected-pipes
   #:connect
   #:input
   #:lock
   #:sender
   #:content
   #:sinks
   #:sinks-hash-table
   #:pipes-hash-table
   #:pipes
   #:sink-names
   #:pipe-name
   #:send-message
   #:react-to-message
   #:perform-action
   #:form-input
   #:input-accepted-p
   #:acceptor
   #:merger
   ))
