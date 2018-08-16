;;;; lut.lisp

(in-package #:utau-write)

(defparameter *state* nil
  "The state of the object.")

;; Setup
(defmacro lut:lut-setup (project-name
                         &key (tempo 120)
                              (voice "uta") voice-custom-directory
                              out-file
                              cache-dir
                              (tool-1 "wavtool.exe")
                              (tool-2 "resampler.exe")
                              key-signature
                              (time-signature #(4 4))
                              kana-conversion)
  `(setf *state* (make-lut-file ,project-name ,voice
                                :key-signature ,key-signature
                                :time-signature ,time-signature
                                :kana-romanisation ,kana-romanisaiton
                                :tempo ,tempo
                                :prepend-voice-prefix t
                                :tool-1 ,tool-1
                                :tool-2 ,tool-2
                                :cache-dir cache-dir
                                :out-file out-file)))

;; Contents
(defmacro lut:note (length note lyric
                    &rest params
                    &key (volume 100)
                    &allow-other-keys)
  `(note (properties *state*) ,length ,lyric ,@params))

(defmacro lut:rest (length)
  `(lut:note ,length 60 "R"))

(defmacro lut:measure ((&key name measure-override) &body body)
  `(measure *state* (:name ,name :measure-length ,measure-override)
     ,@body))

(defmacro lut:group (name &body body)
  `(with-note-collection *state* (:name ,name)
     ,@body))

(defmacro lut:var (name &optional (repetitions 1))
  `(with-variable *state* ,name ,repetitions))

;; Modifications
(defmacro lut:time-signature (time-signature)
  `(setf (time-signature *state*) time-signature))

(defmacro lut:key-signature (key-signature)
  `(setf (key-signature *state*) key-signature))

(defmacro lut:tempo (tempo)
  `(setf (get-property *state* :tempo) ,tempo))

;;; The actual loading and processing
(defun read-script (file-location)
  "Read the script from file."
  (let ((*state* nil)
        (*package* (find-package :lut))
        (*read-eval* nil))
    (load (or file-location *standard-input*) :external-format :utf-8)
    *state*))

(defun load-script (file-location)
  "Load the script from the file and output the required file."
  (dump-to-file (read-script file-location)))

(defun main (args)
  "Entry point to lut."
  (handler-bind ((error (lambda (condition)
                          (uiop:die 1 "Error: ~a" condition))))
    (load-script
     (or (find "lut" args :key #'pathname-type :test #'string-equal)
         *standard-input*))))
