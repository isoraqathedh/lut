;;;; lut.lisp

(in-package #:utau-write)

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

;;; Basic file writing and sections

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
        (format file "~&[#VERSION]~&UST Version~a~%" version)
        (write-stream contents file)))))

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

;;; Note utilities
(defun normalise-note-letter (thing)
  "Normalise THING into a certain note."
  (etypecase thing
    (symbol (char (string-upcase (symbol-name thing)) 0))
    (string (char (string-upcase thing) 0))
    (character (char-upcase thing))))

(defun normalise-accidental (thing)
  "Normalise THING to an accidental."
  :sharp :flat nil ; just ensure that these things are interned.
  (case thing
    (("sharp" "#" #\# "♯" #\♯ :sharp) :sharp)
    (("flat" "b" #\b "♭" #\♭ :flat) :flat)
    (t :natural)))

(defun note->note-number (note accidental octave)
  "Turn a note into a number."
  (+ (ecase (normalise-note-letter note)
       (#\C 0) (#\D 2) (#\E 4) (#\F 5)
       (#\G 7) (#\A 9) (#\B 11))
     (ecase (normalise-accidental accidental)
       (:sharp 1) (:flat -1) (:natural 0))
     (* 12 (+ octave 1))))

(defun parse-note-name (note)
  "Turn a note name into a note number."
  (note->note-number
   (char note 0)
   (char note 1)
   (parse-integer note :start 2)))
