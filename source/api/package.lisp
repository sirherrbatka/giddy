(cl:defpackage #:giddy.api
  (:use #:giddy.aux-package #:cl)
  (:local-nicknames (#:protocol #:giddy.protocol))
  (:export
   #:list-merger))
