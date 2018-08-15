;;;; with-stuff.lisp
;;; A bunch of macros to be used as a setting stone to LUT.
;;; It automates the creation and modification of collections.

;;; Functions for putting things in collections.
(defun note (setting collection length tone lyric
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
    (alexandria:when-let* ((props* (copy-list props))
                           (props-no-volume (remf props* :volume)))
      (setf (other-properties note)
            (alexandria:plist-hash-table props-no-volume)))
    (add-note collection note)))

;;; The actual macros
(defmacro with-note-collection ((&optional measure-length) &body body)
  "Create a note-collection, execute BODY, and return the collection."
  `(let ((,collection-name
           (if ,measure-length
               (make-instance 'measure :intended-length ,measure-length)
               (make-instance 'note-collection))))
     ,@body
     ,collection-name))

(defmacro measure (file (&key name measure-length) &body body)
  "Create a measure that is stored in FILE."
  (let ((collection-name (or name (gensym "COLLECTION"))))
    `(with-note-collection (,collection-name ,measure-length)
       ,@body
       (when ,name
         (setf (get-variable ,file ',name) ,collection-name))
       (add-note ,file ,collection-name))))

(defun %variable (file name &optional (repetitions 1))
  (loop repeat repetitions
        do (add-note file (get-variable file name))))

(defmacro with-variable (file name &optional (repetitions 1))
  `(%variable ,file ',name ,repetitions))

(defmacro with-lut-file ((name title voice
                          &key (time-signature #(4 4))
                               (key-signature "1 = C4")
                               (version "1.2")
                               kana-romanisation
                               (tempo 120)
                               (prepend-voice-prefix t))
                         &body body)
  "Create a LUT file, execute BODY, and then return the file.

The set-up information required to create the LUT file
is provided in the first argument.
The file is bound to NAME, and is returned at the end of the body."
  (alexandria:with-gensyms (lut-settings-name)
    `(let* ((,lut-settings-name
              (make-instance
               'lut-settings
               :kana-romanisation ,kana-romanisation
               :time-signature ,time-signature
               :key-signature (parse-key-signature ,key-signature)
               :version ,version))
            (,name
              (make-instance
               'lut-file
               :properties ,lut-settings-name)))
       (setf (other-properties ,lut-settings-name)
             (alexandria:plist-hash-table
              (list :title ,title
                    :voice ,voice
                    :prepend-voice-prefix ,prepend-voice-prefix
                    :tempo ,tempo)))
       ,@body
       ,name)))
