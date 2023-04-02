(cl:defpackage #:giddy.api
  (:use #:giddy.aux-package #:cl)
  (:local-nicknames (#:protocol #:giddy.protocol))
  (:export
   #:callback-cell
   #:make-cell
   #:reset-input
   #:respond
   #:send
   #:list-merger))
