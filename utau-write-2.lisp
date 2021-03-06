;;;; utau-write.lisp
;;; This file handles writing a completed file into a file.
;;; This relies on the config being completed
;;; prior to sending off to the functions in this file.

(in-package #:utau-write)

;;; We need a CRLF for newlines.
(defvar *crlf* (format nil "~c~c" #\Return #\Newline))
(defvar *version-header* "#VERSION"
  "The section header for the version indicator.")
(defvar *setting-header* "#SETTING"
  "The section header for the settings.")
(defvar *eof-header* "#TRACKEND"
  "The section header for the end of the file.")

;;; Define translation of notes to the config system.
(defun grab-details (config-file config-section setting-hash details)
  "Transfer information from SETING-HASH to CONFIG-SECTION.

For each item in DETAILS, retrieve the item in SETTING-HASH
and put the result in CONFIG-SECTION."
  (loop for setting-name in details
        for (value presence) = (multiple-value-list
                                (gethash setting-name setting-hash))
        when presence
        do (set-option config-file config-section
                       (get-setting-name setting-name)
                       value)))

(defun note-length (quarter-notes)
  "Compute the length of the object in UST time units."
  (* quarter-notes 480))

(defgeneric append-to-file (lut-file config-file thing)
  (:documentation "Add the THING to the CONFIG-FILE given LUT-FILE.")
  (:method ((lut-file lut-file) (config-file config) (thing (eql :preamble)))
    ;; Currently not used;
    ;; the version header has different syntax
    ;; and shouldn't be considered as part of the thing.
    (add-section config-file *version-header*)
    (set-option config-file *version-header*
                "Version" ; The exact value does not matter, we won't be writing it.
                (format nil "UST Version~a" (version lut-file))))
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
      (set-option config-file serial-number
                  "Length" (note-length (duration thing)))
      (set-option config-file serial-number
                  "Lyric" (lyric thing))
      (set-option config-file serial-number
                  "NoteNum" (note-value thing))
      (set-option config-file serial-number "Intensity" (volume thing))
      (grab-details config-file serial-number
                    (other-properties thing) *note-keywords*)
      (incf (note-counter lut-file))))
  (:method ((lut-file lut-file) (config-file config) (thing lut-variable))
    (append-to-file lut-file config-file (get-variable lut-file thing)))
  (:method ((lut-file lut-file) (config-file config) (thing note-collection))
    (loop for i in (get-notes thing)
          do (append-to-file lut-file config-file i))))

;;; Final compiling and output
(defgeneric generate-config (lut-file)
  (:documentation "Generate the output format from the lut-file.")
  (:method ((lut-file lut-file))
    (let ((config-file (make-config :section-name-transform-fn #'string-upcase
                                    :option-name-transform-fn #'identity)))
      (append-to-file lut-file config-file (properties lut-file))
      (loop for i in (get-notes lut-file)
            do (append-to-file lut-file config-file i))
      (append-to-file lut-file config-file :postamble)
      config-file)))

(defgeneric dump-to-file (lut-file)
  (:documentation "Generate the output and write the file to stream.")
  (:method ((lut-file lut-file))
    (with-open-file (file (filename lut-file)
                          :direction :output
                          :external-format :shift_jis
                          :if-does-not-exist :create)
      (setf (note-counter lut-file) 0)
      (format file "[#VERSION]~a" *crlf*)
      (format file "UST Version~a~a~a" (version lut-file) *crlf* *crlf*)
      (write-stream (generate-config lut-file) file))))
