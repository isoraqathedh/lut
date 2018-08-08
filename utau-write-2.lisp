;;;; utau-write.lisp
;;; This file handles writing a completed file into a file.
;;; This relies on the config being completed
;;; prior to sending off to the functions in this file.

(in-package #:utau-write)

(defclass lut-file (note-collection)
  ((properties :accessor properties
               :initarg :properties
               :initform (make-instance 'lut-settings))
   (note-counter :accessor note-counter
                 :initform 0)
   (variable-store :accessor variable-store
                   :initform (make-hash-table)))
  (:documentation "A representation of a lut-file.

This turns out to be just a note collection with extra details."))

;;; Define translation of notes to the config system.
(defgeneric append-to-file (config-file thing lut-variables)
  (:documentation "Add the THING to the CONFIG-FILE.")
  (:method ((config-file config) (thing lut-settings) lut-variables)
    (add-section config-file "#SETTING")
    ;; guff...
    )
  (:method ((config-file config) (thing note) lut-variables)
    (add-section config-file ;; more guff...
                 ))
  (:method ((config-file config) (thing variable) (lut-variables lut-variable-store))
    ())
  (:method ((config-file config) (thing note-collection))
    (loop for i in (get-notes note-collection)
          do (append-to-file config-file thing))))

;;; Final compiling and output
(defgeneric generate-config (lut-file)
  (:documentation "Generate the output format from the lut-file.")
  (:method ((lut-file lut-file))
    (let ((config-file (make-config :section-name-transform-fn #'string-upcase
                                    :option-name-transform-fn #'identity)))
      (append-to-file config-file (lut-settings lut-file))
      (loop for i in (get-notes lut-file)
            do (append-to-file config-file )))))

(defgeneric dump-to-stream (generated-config stream)
  (:documentation "Write the generated output file to a stream."))

(defgeneric dump-to-file (lut-file)
  (:documentation "Generate the output and write the file to stream."))
