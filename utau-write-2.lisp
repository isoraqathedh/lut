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

(defgeneric (setf get-variable) (value lut-file variable-name)
  (:documentation "Set the name VARIABLE-NAME to mean VALUE in LUT-FILE.")
  (:method ((value note-collection) (lut-file lut-file) variable-name)
    (setf (gethash variable-name (variable-store lut-file)) value)))

;;; Define translation of notes to the config system.
(defun grab-details (config-file config-section setting-hash details)
  "Transfer information from SETING-HASH to CONFIG-SECTION.

For each item in DETAILS, retrieve the item in SETTING-HASH
and put the result in CONFIG-SECTION."
  (loop for setting-name in details
        do (set-option config-file config-section
                       (get-setting-name setting-name)
                       (get-property setting-hash setting-name))))

(defgeneric append-to-file (lut-file config-file thing)
  (:documentation "Add the THING to the CONFIG-FILE given LUT-FILE.")
  (:method ((lut-file lut-file) (config-file config) (thing (eql :preamble)))
    (declare (ignore lut-file))
    (add-section config-file *version-header*)
    (set-option config-file *version-header*
                (format nil "UST Version~a" (version lut-file))
                t)) ; The exact value does not matter, we won't be writing it.
  (:method ((lut-file lut-file) (config-file config) (thing (eql :postamble)))
    (declare (ignore lut-file thing))
    (add-section config-file *eof-header*))
  (:method ((lut-file lut-file) (config-file config) (thing lut-settings))
    (add-section config-file *setting-header*)
    (grab-details config-file *setting-header*
                  (other-properties thing) *setting-keywords*))
  (:method ((lut-file lut-file) (config-file config) (thing note))
    (let ((serial-number (format nil "#~4,'0d" (note-counter lut-file))))
      (add-section config-file serial-number)
      (grab-details config-file serial-number
                    (other-properties thing) *note-keywords*)
      (incf (note-counter lut-file))))
  (:method ((lut-file lut-file) (config-file config) (thing variable))
    (append-to-file lut-file config-file (get-variable lut-file thing)))
  (:method ((lut-file lut-file) (config-file config) (thing note-collection))
    (declare (ignore lut-file))
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
