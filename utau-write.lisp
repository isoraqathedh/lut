;;;; utau-write.lisp

(in-package #:utau-write)

;;; Basic file writing and sections
(defvar *crlf* (format nil "~c~c" #\Return #\Newline))

(defclass lut-file ()
  ((filename :initarg :filename
             :initform (error "Requires filename")
             :accessor filename
             :documentation "The location of the UST file to write to.")
   (contents :accessor contents
             :initform (make-config
                        :section-name-transform-fn #'string-upcase
                        :option-name-transform-fn #'identity)
             :documentation "The representation of the config file.")
   (note-store :accessor note-store
               :initform nil
               :documentation "A temporary store for notes.")
   (version :initform "1.2"
            :initarg :version
            :accessor version
            :documentation "The UTAU version that the file requires.")
   (note-counter :initform 0
                 :accessor note-counter
                 :documentation "The 'fill-pointer' for notes.")
   (key-signature :initform "1 = C4"
                  :initarg :key-signature
                  :accessor key-signature
                  :documentation "A selected key signature of the song.")
   (time-signature :initform #(4 4)
                   :initarg :time-signature
                   :accessor time-signature
                   :documentation "A selected time signature of the song.")
   (kana-romanisation :initform nil
                      :initarg :kana-romanisation
                      :reader kana-romanisation
                      :documentation
                      "The setting for transforming romaji into kana."))
  (:documentation "Internal representation of an LUT file"))

(defgeneric dump-to-stream (lut-file stream)
  (:documentation "Write the parsed configuration into a stream.")
  (:method ((lut-file lut-file) (stream stream))
    (with-accessors ((contents contents)
                     (version version)) lut-file
      (format stream "[#VERSION]~aUST Version~a~a" *crlf* version *crlf*)
      (write-stream contents stream)
      (format stream "[#TRACKEND]~a" *crlf*))))

(defgeneric dump-file (lut-file)
  (:documentation "Write the parsed configuration into the file.")
  (:method ((lut-file lut-file))
    (with-open-file (file (filename lut-file)
                          :direction :output
                          :external-format :shift_jis)
      (dump-to-stream lut-file file))))

(defgeneric dump-stream (stream lut-file))

(defgeneric create-setting-section (lut-file options)
  (:documentation "Create the setting section of the UST file.")
  (:method ((lut-file lut-file) options)
    (with-accessors ((contents contents)) lut-file
      (let ((setting-header "#SETTING"))
        (add-section contents setting-header)
        (dolist (i options)
          (set-option contents setting-header (car i) (cdr i)))))))

(defmacro ensure-type (accessor-form &body acceptable-and-convertible-types)
  "Ensure that the item named in ACCESSOR-FORM is of the correct type.

To do this, it analyses acceptable-and-convertable-type.
There are two possible forms:

- A single symbol, which is an acceptable type. Nothing will be done.
- A list of the form (TYPE CONVERTER), which is a convertible type.
  In this case CONVERTER will be called against ACCESSOR-FORM,
  and the results will be SETFed back into ACCESSOR-FORM.
- Any other type will signal an error."
  `(etypecase ,accessor-form
     ,@(loop for i in acceptable-and-convertible-types
             if (listp i)
             collect `(,(first i)
                       (setf ,accessor-form (,(second i) ,accessor-form)))
             else collect `(,i))))

(defun set-up-file (name tempo voice-name
                    &key
                      (version "1.2")
                      (insert-voice-dir t)
                      (file-dir (uiop:getcwd))
                      (tracks 1)
                      (alternate-name name)
                      kana-romanisation
                      (key-signature (parse-key-signature "1 = C"))
                      (time-signature #(4 4)))
  "Create a LUT file object, and load set-up information into it."
  (let ((object (make-instance
                 'lut-file
                 :filename (make-pathname :name name
                                          :type "ust"
                                          :defaults file-dir)
                 :kana-romanisation kana-romanisation
                 :time-signature time-signature
                 :key-signature key-signature
                 :version version)))
    (ensure-type (key-signature object)
      key-signature
      (string parse-key-signature))
    (ensure-type (time-signature object)
      vector
      null
      (string parse-time-signature))
    (create-setting-section
     object (list (cons "Tempo" tempo)
                  (cons "Tracks" tracks)
                  (cons "ProjectName" alternate-name)
                  (cons "OutFile" (format nil "~a.wav" name))
                  (cons "CacheDir" (format nil "~a.cache" name))
                  (cons "VoiceDir" (format nil "~:[~;%VOICE%~]~a"
                                           insert-voice-dir voice-name))
                  (cons "Tool1" "wavtool.exe")
                  (cons "Tool2" "resampler.exe")))
    object))

;;; Make note
(defun note-length (quarter-notes)
  "Compute the length of the object in UST time units."
  (* quarter-notes 480))

(defgeneric make-note (lut-file note
                       &rest params
                       &key lyric length volume raw-length)
  (:documentation "Create an object that will represent a note.")
  (:method ((lut-file lut-file) note
            &rest params
            &key lyric (length 1) (volume 100) raw-length &allow-other-keys)
    (let ((ht (alexandria:plist-hash-table params)))
      (setf (gethash :lyric ht)
            (let ((scheme (kana-romanisation lut-file)))
              (cond ((null lyric) "R")
                    (scheme (kanafy-string lyric scheme))
                    (t lyric)))
            (gethash :note ht)
            (etypecase note
              (number note)
              (string (note-number (parse-note note)
                                   (key-signature lut-file))))
            (gethash :volume ht) volume
            (gethash :length ht) (if raw-length
                                     length
                                     (floor (note-length length)))
            (gethash :modulation ht) 0)
      ht)))

(defparameter *key-convert*
  '((:lyric . "Lyric")
    (:volume . "Intensity")
    (:modulation . "Modulation")
    (:length . "Length")
    (:note . "NoteNum"))
  "Convert a keyword into a setting option.")

(defgeneric record-note (lut-file params)
  (:documentation "Create a note with the specified parameters.")
  (:method ((lut-file lut-file) (params hash-table))
    (push params (note-store lut-file))))

(defgeneric create-note (lut-file note
                         &rest params
                         &key lyric length volume &allow-other-keys)
  (:documentation "Create and push a note into the note store.")
  (:method ((lut-file lut-file) note
            &rest params
            &key lyric length volume &allow-other-keys)
    (declare (ignore lyric length volume))
    (record-note lut-file (apply #'make-note lut-file note params))))

(defgeneric drop-notes (lut-file)
  (:documentation "Delete the notes in the buffer.")
  (:method ((lut-file lut-file))
    (setf (note-store lut-file) ())))

(defgeneric dump-notes (lut-file)
  (:documentation "Flush the queue of notes into the config file.")
  (:method ((lut-file lut-file))
    (with-accessors ((contents contents)
                     (note-counter note-counter)
                     (note-store note-store)) lut-file
      (loop for id-number from note-counter
            for note-id = (format nil "#~4,'0d" id-number)
            for i in (reverse (note-store lut-file))
            do (add-section contents note-id)
               (loop for (k . v) in *key-convert*
                     do (set-option contents note-id v (gethash i k)))
               (incf note-counter)
            finally (drop-notes lut-file)))))

;;; Higher-level functions
(defgeneric process-measure (lut-file properties notes)
  (:documentation "Add several notes a given LUT file.

This function checks if the notes given (as arguments to `make-note')
have enough notes as mentioned in the signature of the lut-file
(or can be passed individually as an override).
If it does not, then it will signal an error.
If there is no time signature, then send out a warning
but always validate the time signature.")
  (:method ((lut-file lut-file) properties notes)
    (let ((effective-time-signature
           (or (getf properties :time-signature)
               (time-signature lut-file)
               (warn "No time signature indicated for measurement. ~
No measure validation will be attempted.")))
          note-list
          actual-length)
      (loop for i in notes
            for j = (apply #'make-note lut-file i)
            collect j into %note-list
            sum (print (gethash :length j)) into %actual-length
            finally (setf note-list %note-list
                          actual-length %actual-length))
      (when effective-time-signature
        (let ((expected-length
                (note-length
                 (* 4
                    (aref effective-time-signature 0)
                    (/ (aref effective-time-signature 1))))))
          (unless (= expected-length actual-length)
            (restart-case (error "Time signature requires note length ~s, ~
but got length ~s" expected-length actual-length)
              (adjust-measure ()
                :report "Truncate or fill the measure until the length fits."
                (loop for i in note-list
                      sum (gethash :length i) into duration-so-far
                      collect i into temp-list
                      do (cond ((= expected-length duration-so-far)
                                (setf note-list temp-list)
                                (return))
                               ((< expected-length duration-so-far)
                                (decf (gethash :length i)
                                      (- duration-so-far expected-length))
                                (setf note-list temp-list)
                                (return)))
                      finally (nconc note-list
                                     (list
                                      (make-note
                                       lut-file "C4"
                                       :length (- expected-length actual-length)
                                       :raw-length t)))))
              (ignore-measure ()
                :report "Drop all notes and do nothing."
                (return-from process-measure nil))
              (ignore-validation ()
                :report "Ignore the validation and accept the measure.")))))
      (loop for i in note-list
            do (record-note lut-file i)))))
