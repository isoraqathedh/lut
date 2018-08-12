;;;; notes-and-settings.lisp
;;; This file contains functions that transform notes
;;; to conform to the UTAU file.

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
    "The setting for transforming romaji into kana."))
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
    :initform (make-hash-table :test #'equal)
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

(defgeneric finalise-note (note settings duration
                           &key lyric volume other-properties)
  (:documentation "Create a note.")
  (:method ((note absolute-note) settings duration
            &key (lyric "R") (volume 100) other-properties)
    (declare (ignore settings other-properties))
    (make-instance 'finalised-note
                   :value (note-number note)
                   :lyric lyric
                   :volume volume
                   :duration duration))
  (:method ((note solfege) settings duration
            &key (lyric "R") (volume 100) other-properties)
    (declare (ignore other-properties))
    (make-instance 'finalised-note
                   :value (note-number note (key-signature settings))
                   :lyric lyric
                   :volume volume
                   :duration duration)))

(defun get-setting-name (keyword)
  "Transform a setting keyword to a config key."
  (delete #\- (format nil "~:(~a~)" (symbol-name keyword))))

(defgeneric get-settings ())
(defgeneric (setf get-settings) ())
