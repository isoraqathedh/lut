;;;; utau-write.lisp
;;; This file handles writing a completed file into a file.
;;; This relies on the config being completed
;;; prior to sending off to the functions in this file.

(in-package #:utau-write)

(defclass lut-file (note-collection)
  ((properties :accessor properties
               :initarg :properties
               :initform (make-hash-table))
   (variable-store :accessor variable-store
                   :initform (make-hash-table)))
  (:documentation "A representation of a lut-file.

This turns out to be just a note collection with extra details."))

(defgeneric create-config (lut-file)
  (:documentation "Set up a new output.")
  (:method ((lut-file lut-file))
    (let ((config-file (make-config :section-name-transform-fn #'string-upcase
                                    :option-name-transform-fn #'identity)))
      (add-section config-file "#SETTING")
      )))

;;; Final compiling and output
(defgeneric generate-config (lut-file)
  (:documentation "Generate the output format from the data stream in"))

(defgeneric dump-to-stream (generated-config stream)
  (:documentation "Write the generated output file to a stream."))

(defgeneric dump-to-file (lut-file)
  (:documentation "Generate the output and write the file to stream."))
