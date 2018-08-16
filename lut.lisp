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
                              kana-conversion))

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
