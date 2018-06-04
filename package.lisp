;;;; package.lisp

(defpackage #:utau-write
  (:use #:cl #:py-configparser)
  (:shadow #:write-stream)
  (:documentation "Supporting functions for writing files"))

(defpackage #:lut
  (:documentation "The API for LUT files.")
  (:export
   ;; LUT functions
   #:lut-setup #:measure #:note #:tempo #:time-signature #:key-signature

   ;; Romanisations
   #:hepburn #:nihon)
  (:import-from #:cl #:nil))
