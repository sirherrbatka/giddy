(cl:defpackage #:giddy.protocol
  (:use #:cl #:giddy.aux-package)
  (:export
   #:fundamental-message
   #:content-message
   #:channel
   #:control-message
   #:flush-message))
