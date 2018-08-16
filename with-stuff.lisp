;;;; with-stuff.lisp
;;; A bunch of macros to be used as a setting stone to LUT.
;;; It automates the creation and modification of collections.

(in-package #:utau-write)
(defvar *current-receptor*)

;;; Functions for putting things in collections.
(defun make-lut-file (title voice
                      &key (time-signature #(4 4))
                           (key-signature "1 = C4")
                           (version "1.2")
                           kana-romanisation
                           (tempo 120)
                           (prepend-voice-prefix t)
                           (tool-1 "wavtool.exe")
                           (tool-2 "resampler.exe")
                           (file-dir (uiop:getcwd))
                           cache-dir
                           out-file
                           (alternate-name title)
                           (tracks 1))
  "Create a LUT file with the provided values, and then return it."
  (let* ((settings (make-instance
                    'lut-settings
                    :kana-romanisation kana-romanisation
                    :time-signature time-signature
                    :key-signature (parse-key-signature key-signature)
                    :filename (make-pathname :name title
                                             :type "ust"
                                             :defaults file-dir)
                    :version version))
         (file     (make-instance
                    'lut-file
                    :properties settings)))
    (setf (other-properties settings)
          (alexandria:plist-hash-table
           (list :project-name alternate-name
                 :voice (format nil "~:[~;%VOICE%~]~a"
                                prepend-voice-prefix voice)
                 :tool-1 tool-1
                 :tool-2 tool-2
                 :cache-dir (or cache-dir
                                (format nil "~a.cache" title))
                 :out-file (or out-file
                               (format nil "~a.wav" title))
                 :tracks tracks
                 :tempo tempo)))
    file))

(defun note (setting length tone lyric
                 &rest props &key (volume 100) &allow-other-keys)
  "Put a note inside a COLLECTION given SETTING."
  (let ((note (finalise-note
               (parse-note tone)
               setting
               length
               :volume volume
               :lyric (alexandria:if-let ((scheme (kana-romanisation setting)))
                        (kanafy-string lyric scheme)
                        lyric))))
    ;; (format t "~&Current receptor: ~s" *current-receptor*)
    (alexandria:when-let* ((props* (copy-list props))
                           (props-no-volume (remf props* :volume)))
      (setf (other-properties note)
            (alexandria:plist-hash-table props-no-volume)))
    (add-note *current-receptor* note)))

;;; The actual macros
(defmacro with-note-collection (file (&key name measure-length)
                                &body body)
  "Create a note-collection, execute BODY, and return the collection."
  `(add-note
    *current-receptor*
    (let ((*current-receptor*
            (cond (,measure-length
                   (make-instance 'measure
                                  :intended-length ,measure-length))

                  (t (make-instance 'note-collection)))))
      ,@body
      (when ',name
        (setf (get-variable ,file ',name) *current-receptor*))
      *current-receptor*)))

(defmacro measure (file (&key name measure-length) &body body)
  "Create a measure that is stored in FILE."
  `(with-note-collection ,file (:name ,name
                                :measure-length (or ,measure-length
                                                    (time-signature-length
                                                     (time-signature
                                                      (properties ,file)))))
     ,@body))

(defun %variable (file name &optional (repetitions 1))
  (loop repeat repetitions
        do (add-note file (get-variable file name))))

(defmacro with-variable (file name &optional (repetitions 1))
  `(%variable ,file ',name ,repetitions))

(defmacro with-lut-file ((name title voice
                          &rest lut-params
                          &key (time-signature #(4 4))
                               (key-signature "1 = C4")
                               (version "1.2")
                               kana-romanisation
                               (tempo 120)
                               (prepend-voice-prefix t)
                               (tool-1 "wavtool.exe")
                               (tool-2 "resampler.exe")
                               (file-dir (uiop:getcwd))
                               cache-dir
                               out-file
                               (alternate-name title)
                               (tracks 1))
                         &body body)
  "Create a LUT file, execute BODY, and then return the file.

The set-up information required to create the LUT file
is provided in the first argument.
The file is bound to NAME, and is returned at the end of the body."
  (declare (ignore time-signature key-signature version
                   kana-romanisation tempo prepend-voice-prefix
                   tool-1 tool-2 file-dir cache-dir out-file alternate-name
                   tracks))
  `(let* ((,name (make-lut-file ,title ,voice ,@lut-params))
          (*current-receptor* ,name))
     ,@body
     ,name))
