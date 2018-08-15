;;;; notes-and-settings.lisp
;;; This file contains functions that transform notes
;;; to conform to the UTAU file.

(in-package #:utau-write)

(defclass lut-settings ()
  ((version
    :initform "1.2"
    :initarg :version
    :accessor version
    :documentation "The UTAU version that the file requires.")
   (key-signature
    :initform (parse-key-signature "1 = C4")
    :initarg :key-signature
    :accessor key-signature
    :documentation "A selected key signature of the song.")
   (time-signature
    :initform #(4 4)
    :initarg :time-signature
    :accessor time-signature
    :documentation "A selected time signature of the song.")
   (kana-romanisation
    :initform nil
    :initarg :kana-romanisation
    :reader kana-romanisation
    :documentation
    "The setting for transforming romaji into kana.")
   (other-properties
    :accessor other-properties
    :initform (make-hash-table)
    :documentation "Other properties that can be part of the file."))
  (:documentation "The settings for transforming a LUT file to a UST file."))

(defclass finalised-note (absolute-note)
  ((duration
    :accessor duration
    :initarg :duration
    :initform 1
    :documentation "The duration of the note in quarter-notes.")
   (volume
    :accessor volume
    :initarg :volume
    :initform 100
    :documentation "The loudness of the note.")
   (lyric
    :accessor lyric
    :initarg :lyric
    :initform "R"
    :documentation "The lyric of the note.")
   (other-properties
    :accessor other-properties
    :initform (make-hash-table)
    :documentation "Other properties that can be part of the note."))
  (:documentation "A note that is adjusted to be writable into a UST file."))

(defmethod print-object ((object finalised-note) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-accessors ((note-value note-value)
                     (duration duration)
                     (volume volume)
                     (lyric lyric)
                     (other-properties other-properties)) object
      (format stream "NUM ~d DUR ~a LYR ~s~:[ VOL ~a~;~]~:[ +~d~;~]"
              note-value
              duration
              lyric
              (= 100 volume) volume
              (zerop (hash-table-count other-properties))
              (hash-table-count other-properties)))))

(defun %finalise-note (note duration lyric volume other-properties)
  (let ((new-note (make-instance 'finalised-note
                                 :value note
                                 :lyric lyric
                                 :volume volume
                                 :duration duration)))
    (when other-properties
      (setf (other-properties new-note)
            (ensure-type (other-properties note)
              hash-table
              (list alexandria:plist-hash-table))))
    new-note))

(defgeneric finalise-note (note settings duration
                           &key lyric volume other-properties)
  (:documentation "Create a note.")
  (:method ((note absolute-note) settings duration
            &key (lyric "R") (volume 100) other-properties)
    (declare (ignore settings))
    (%finalise-note (note-number note) duration lyric volume other-properties))
  (:method ((note solfege) settings duration
            &key (lyric "R") (volume 100) other-properties)
    (%finalise-note (note-number note (key-signature settings))
                    duration lyric volume other-properties)))

(defun get-setting-name (keyword)
  "Transform a setting keyword to a config key."
  (delete #\- (format nil "~:(~a~)" (symbol-name keyword))))

(defvar *note-keywords*
  '(:pre-utterance :intensity :modulation :p-b-type :pitch-bend
    :envelope :p-b-s :p-b-w :p-b-start)
  "Keywords that correspond to options in notes.")

(defvar *setting-keywords*
  '(:tempo :tracks :project-name :voice-dir :out-file :cache-dir
    :tool-1 :tool-2)
  "Keywords that correspond to options in settings.")

(defgeneric get-property (property-object custom-keyword)
  (:documentation "Get a single CUSTOM-KEYWORD from PROPERTY-OBJECT.")
  (:method ((property-object lut-settings) (keyword symbol))
    (gethash keyword property-object))
  (:method ((property-object note) (keyword symbol))
    (gethash keyword (other-properties property-object))))

(defgeneric (setf get-property) (value property-object keyword)
  (:documentation "Set a single CUSTOM-KEYWORD from PROPERTY-OBJECT.")
  (:method (value (property-object lut-settings) (keyword symbol))
    (setf (gethash keyword (other-properties property-object)) value)))
