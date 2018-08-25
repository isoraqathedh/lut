;;;; lut.lisp

(in-package #:utau-write)

(defparameter *state* nil
  "The state of the object.")

;; Setup
(defmacro lut:lut-setup (project-name
                         &rest params
                         &key (tempo 120)
                              (voice-dir "uta") voice-custom-directory
                              out-file
                              file-dir
                              cache-dir
                              (tool-1 "wavtool.exe")
                              (tool-2 "resampler.exe")
                              key-signature
                              (time-signature #(4 4))
                              kana-romanisation
                              title
                              &allow-other-keys)
  (declare (ignore voice-custom-directory tempo out-file file-dir
                   cache-dir tool-1 tool-2 key-signature time-signature
                   kana-romanisation title))
  (let ((real-params (copy-list params)))
    (remf real-params :voice-dir)
    `(setf *state* (make-lut-file ,project-name ,voice-dir ,@real-params)
           *current-receptor* *state*)))

;; Contents
(defmacro lut:note (length note lyric
                    &rest params
                    &key (volume 100)
                    &allow-other-keys)
  (declare (ignore volume))
  `(,(if (listp lyric) 'notes 'note) *state* ,length ,note ',lyric ,@params))

(defmacro lut:rest (length)
  `(lut:note ,length 60 "R"))

(defmacro lut:measure ((&key name measure-override) &body body)
  `(measure *state* (:name ,name :measure-length ,measure-override)
     ,@body))

(defmacro lut:group (name &body body)
  `(with-note-collection *state* (:name ',name)
     ,@body))

(defmacro lut:var (name &optional (repetitions 1))
  `(with-variable *state* ,name ,repetitions))

;; Modifications
(defmacro lut:time-signature (time-signature)
  `(setf (time-signature *state*) ,time-signature))

(defmacro lut:key-signature (key-signature)
  `(setf (key-signature *state*) ,key-signature))

(defmacro lut:tempo (tempo)
  `(setf (get-property *state* :tempo) ,tempo))

;;; The actual loading and processing
(defun read-script (file-location)
  "Read the script from file."
  (let ((*state* nil)
        (*package* (find-package :lut))
        (*current-receptor* nil)
        (*read-eval* nil))
    (load (or file-location *standard-input*) :external-format :utf-8)
    *state*))

(defun load-script (file-location)
  "Load the script from the file and output the required file."
  (dump-to-file (read-script file-location)))

(defun main (args)
  "Entry point to lut."
  ;; This should only be run non-interactively
  ;; (i.e. as a shell script entry point.)
  ;; If you run this interactively, you might break Lisp
  ;; Because it automatically kills the image if *any* errors show up.
  (handler-bind ((error (lambda (condition)
                          (uiop:die 1 "Error: ~a" condition))))
    (load-script
     (or (find "lut" args :key #'pathname-type :test #'string-equal)
         *standard-input*))))
