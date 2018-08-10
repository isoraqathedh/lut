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

;;; We need a CRLF for newlines.
(defvar *crlf* (format nil "~c~c" #\Return #\Newline))
(defvar *version-header* "#VERSION"
  "The section header for the version indicator.")
(defvar *setting-header* "#SETTING"
  "The section header for the settings.")
(defvar *eof-header* "#TRACKEND"
  "The section header for the end of the file.")

;;; Variables
(define-condition variable-not-found (error)
  ((variable-name :accessor variable-name
                  :initarg :variable-name)
   (variable-store :accessor variable-store
                   :initarg :variable-store))
  (:documentation "Error signalled when a variable is not found.")
  (:report (lambda (c s)
             (format s "Variable ~s is not found in store ~s."
                     (variable-name c)
                     (variable-store c)))))

(defgeneric get-variable (lut-file variable-name)
  (:documentation "Get the object behind the name of the variable.

Signal an error if the variable is not found.")
  (:method ((lut-file lut-file) variable-name)
    (let ((store (variable-store lut-file)))
      (or (gethash variable-name store)
          (restart-case (error 'variable-not-found
                               :variable-name variable-name
                               :variable-store (variable-store lut-file))
            (use-value (value) value)
            (use-empty-collection ()
              :report "Return an empty collection."
              (make-instance 'note-collection)))))))

;;; Define translation of notes to the config system.
(defgeneric append-to-file ()
  (:documentation "Add the THING to the CONFIG-FILE given LUT-FILE.")
  (:method ((lut-file lut-file) (config-file config) (thing (eql :preamble)))
    (add-section config-file *version-header*)
    (set-option config-file *version-header*
                (format nil "UST Version~a" version)
                t)) ; The exact value does not matter, we won't be writing it.
  (:method ((lut-file lut-file) (config-file config) (thing (eql :postamble)))
    (add-section config-file *eof-header*))
  (:method ((lut-file lut-file) (config-file config) (thing lut-settings))
    (add-section config-file *setting-header*)
    ;; guff...
    )
  (:method ((lut-file lut-file) (config-file config) (thing note))
    (add-section config-file (format nil "#~4,'0d" (note-counter lut-file)))
    ;; more guff...
    (incf (note-counter lut-file)))
  (:method ((lut-file lut-file) (config-file config) (thing variable))
    (append-to-file lut-file config-file (get-variable lut-file thing)))
  (:method ((lut-file lut-file) (config-file config) (thing note-collection))
    (loop for i in (get-notes note-collection)
          do (append-to-file config-file thing))))

;;; Final compiling and output
(defgeneric generate-config (lut-file)
  (:documentation "Generate the output format from the lut-file.")
  (:method ((lut-file lut-file))
    (let ((config-file (make-config :section-name-transform-fn #'string-upcase
                                    :option-name-transform-fn #'identity)))
      (append-to-file lut-file config-file :preamble)
      (append-to-file lut-file config-file (lut-settings lut-file))
      (loop for i in (get-notes lut-file)
            do (append-to-file lut-file config-file i))
      (append-to-file lut-file config-file :postamble)
      config-file)))

(defgeneric dump-to-file (lut-file)
  (:documentation "Generate the output and write the file to stream.")
  (:method ((lut-file lut-file))
    (with-open-file (file (filename lut-file)
                          :direction :output
                          :external-format :shift_jis)
      (write-stream (generate-config lut-file) file))))
