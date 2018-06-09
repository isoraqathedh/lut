;;;; utau-write.lisp

(in-package #:utau-write)

;;; Basic file writing and sections
(defvar *crlf* (format nil "~c~c" #\Return #\Newline))

(defclass lut-file ()
  ((filename :initarg :filename
             :initform (error "Requires filename")
             :accessor filename)
   (contents :accessor contents
             :initform (make-config
                        :section-name-transform-fn #'string-upcase
                        :option-name-transform-fn #'identity))
   (version :initform "1.2"
            :initarg :version
            :accessor version)
   (note-counter :initform 0
                 :accessor note-counter)
   (key-signature :initform "1 = C4"
                  :initarg :key-signature
                  :accessor key-signature)
   (time-signature :initform #(4 4)
                   :initarg :time-signature
                   :accessor time-signature)
   (kana-romanisation :initform nil
                      :initarg :kana-romanisation
                      :reader kana-romanisation))
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
(defgeneric make-note (lut-file &key (note))
  (:documentation "Create an object that will represent a note.")
  (:method ((lut-file lut-file) &key note)
    (make-hash-table)))

(defgeneric create-note (lut-file params)
  (:documentation "Create a note with the specified parameters.")
  (:method ((lut-file lut-file) (params hash-table))
    (with-accessors ((contents contents) (note-counter note-counter)) lut-file
      (let ((note-id (format nil "#~4,'0d" note-counter)))
        (add-section contents note-id)
        (loop for key being the hash-keys of params
              for value being the hash-values of params
              do (set-option note-option key value)))
      (incf note-counter))))
