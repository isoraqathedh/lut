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
                 :accessor note-counter))
  (:documentation "Internal representation of an LUT file"))

(defgeneric dump-file (lut-file)
  (:documentation "Write the parsed configuration into the file.")
  (:method ((lut-file lut-file))
    (with-accessors ((filename filename)
                     (contents contents)
                     (version version))
        lut-file
      (with-open-file (file filename
                            :direction :output
                            :external-format :shift_jis)
        (format file "[#VERSION]~aUST Version~a~a" *crlf* version *crlf*)
        (write-stream contents file)
        (format file "[#TRACKEND]~a" *crlf*)))))

(defgeneric create-setting-section (lut-file options)
  (:documentation "Create the setting section of the UST file.")
  (:method ((lut-file lut-file) options)
    (with-accessors ((contents contents)) lut-file
      (let ((setting-header "#SETTING"))
        (add-section contents setting-header)
        (dolist (i options)
          (set-option contents setting-header (car i) (cdr i)))))))

(defun set-up-file (name tempo voice-name &key
                                            (version "1.2")
                                            (insert-voice-dir t)
                                            (file-dir (uiop:getcwd))
                                            (tracks 1)
                                            (alternate-name name))
  (let ((object (make-instance
                 'lut-file
                 :filename (make-pathname :name name
                                          :type "ust"
                                          :defaults file-dir)
                 :version version)))
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
(defun make-note ())

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
