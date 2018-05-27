;;;; package.lisp

(defpackage #:utau-write
  (:use #:cl)
  (:shadow #:write-stream)
  (:documentation "Supporting functions for writing files"))

(defpackage #:lut
  (:use #:cl)
  (:documentation "The API for LUT files."))
