;;; note-utils.lisp
;;; Functions for altering notes.

(in-package #:utau-write)

;;; Note utilities
;;; ==============
(defclass note ()
  ((value :reader note-value
          :initarg :value)
   (octave :reader octave
           :initarg :octave)))

(defclass absolute-note (note)
  ((accidental :reader accidental
               :initarg :accidental
               :initform :natural)))

(defmethod print-object ((object absolute-note) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (value accidental octave) object
      (format stream "~c~c~d"
              value
              (ecase accidental
                (:sharp #\♯) (:flat #\♭) (:natural #\-))
              octave))))

(defclass solfege (note)
  ())

(defvar *valid-solfege*
  '(#|de = ti|# (:do . 0)  (:di . 1)
    (:ra . 1)   (:re . 2)  (:ri . 3)
    (:me . 3)   (:mi . 4)
    #|fe = mi|# (:fa . 5)  (:fi . 6)
    (:se . 6)   (:so . 7)  (:si . 8)
    (:le . 8)   (:la . 9)  (:li . 10)
    (:te . 10)  (:ti . 11)
    #|------|#  (:do\' . 12))
  "Symbols corresponding to correct solfege notes,
and their corresponding offsets from do. ")

(defmethod print-object ((object solfege) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (value octave) object
      (format stream "~a" value)
      (loop repeat (abs octave)
            do (format stream "~:[,~;'~]" (plusp octave))))))

(defgeneric note-order (note)
  (:documentation "Find the order of notes from C or do.")
  (:method ((note solfege))
    (cdr (assoc (note-value note) *valid-solfege*)))
  (:method ((note absolute-note))
    (+ (ecase (note-value note)
         (#\C 0) (#\D 2) (#\E 4) (#\F 5)
         (#\G 7) (#\A 9) (#\B 11))
       (ecase (accidental note)
         (:sharp 1) (:natural 0) (:flat -1)))))

(defclass key-signature ()
  ((starting-note :reader starting-note
                  :initarg :starting-note
                  :initform (make-instance 'absolute-note
                                           :value #\C
                                           :accidental :natural
                                           :octave 4))
   (mode :reader mode
         :initform :major
         :initarg :mode)))

(defmethod print-object ((object key-signature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (starting-note mode) object
      (with-slots (value accidental octave) starting-note
        (format stream "~c~c~d ~a"
                value
                (ecase accidental
                  (:sharp #\♯) (:flat #\♭) (:natural #\-))
                octave
                mode)))))

(defgeneric tonic (key-signature)
  (:documentation "Get the tonic of a particular mode of the key signature.")
  (:method ((key-signature key-signature))
    (ecase (mode key-signature)
      ((:major :ionian)  :do\')
      ((:minor :aeolian) :la)
      (:dorian           :re)
      (:phrygian :mi)
      (:lydian :fa)
      (:mixolydian :so)
      (:locrian :ti))))

(defgeneric mode-offset (key-signature)
  (:documentation "Find the offset that links a mode to its parallel major.")
  (:method ((key-signature key-signature))
    (ecase (mode key-signature)
      ((:major :ionian) 0)
      ((:minor :aeolian) 3)
      (:dorian 9)
      (:phrygian 8)
      (:lydian 7)
      (:mixolydian 5)
      (:locrian 1))))

;;; Notes
(defun normalise-note-letter (thing)
  "Normalise THING into a certain note."
  (etypecase thing
    (symbol (char (string-upcase (symbol-name thing)) 0))
    (string (char (string-upcase thing) 0))
    (character (char-upcase thing))))

(defun normalise-accidental (thing)
  "Normalise THING to an accidental."
  (case thing
    (("sharp" "#" #\# "♯" #\♯ :sharp) :sharp)
    (("flat" "b" #\b "♭" #\♭ :flat) :flat)
    (t :natural)))

(defun normalise-solfege (thing)
  "Normalise THING into a solfege."
  (let ((parsed-solfege (etypecase thing
                          (symbol (string-upcase (symbol-name thing)))
                          (string (string-upcase thing)))))
    (if (member parsed-solfege *valid-solfege*
                :key (alexandria:compose #'symbol-name #'car)
                :test #'string=)
        (alexandria:ensure-symbol parsed-solfege "KEYWORD")
        (error "Value ~s is not a known solfege." parsed-solfege))))

(defun note->note-number (note accidental octave)
  "Turn a note into a number."
  ) ;; offset

(defun parse-note-name (string &key (start 0) end)
  "Parse a note name into the corresponding object."
  (let ((has-accidental-p (position (char string (+ start 1)) "#♯b♭- ")))
    (make-instance 'absolute-note
                   :value (normalise-note-letter (char string start))
                   :accidental (if has-accidental-p
                                   (normalise-accidental
                                    (char string (+ start 1)))
                                   :natural)
                   :octave (parse-integer
                            string
                            :start (+ start (if has-accidental-p 2 1))
                            :end end))))

(defun parse-solfege-name (string &key (start 0) end)
  "Parse a solfege into the corresponding object."
  (let ((octave-boundary (+ start 2)))
    (make-instance
     'solfege
     :value (normalise-solfege
             (subseq string start octave-boundary))
     :octave (- (count #\' string :start octave-boundary :end end)
                (count #\, string :start octave-boundary :end end)))))

(defun parse-key-signature (string &key (start 0) end)
  "Parse a key signature of a given form into the corresponding object."
  (let ((substring (subseq string start end)))
    (cond
      ((find #\= substring)                 ; numeric style system, e.g. 1 = C, 6=A
       (destructuring-bind (mode-number tonic-note)
           (mapcar #'str:trim
                   (split-sequence:split-sequence #\= substring
                                                  :start start :end end ))
        (make-instance
         'key-signature
         :starting-note (parse-note-name tonic-note)
         :mode (ecase (char mode-number 0)
                 (#\1 :major) (#\2 :dorian) (#\3 :phrygian)
                 (#\4 :lydian) (#\5 :mixolydian) (#\6 :minor)
                 (#\7 :locrian)))))
     (t                                 ; Generic system, e.g. C4 maj, A3 min
      (destructuring-bind (starting-note mode-name)
          (mapcar #'str:trim
                  (split-sequence:split-sequence #\Space substring
                                                 :start start :end end))
        (make-instance
         'key-signature
         :starting-note (parse-note-name starting-note)
         :mode (alexandria:switch (mode-name :test #'string-equal)
                 ("maj" :major)
                 ("min" :minor))))))))

(defgeneric note-number (note &optional key-signature)
  (:documentation "Compute the note number for a particular note.

If the note given is in solfege, a key signature must be given.
Otherwise, it is ignored.")
  (:method ((note absolute-note) &optional key-signature)
    (declare (ignore key-signature))
    (+ (deviation-from-C note)
       (* 12 (octave note))
       12))
  (:method ((note solfege) &optional key-signature)
    (typecase key-signature
      (null (error "Key signature must be given."))
      (string (parse-key-signature key-signature))
      ((not key-signature) (error "Not a key signature: ~s" key-signature)))
    (with-slots (starting-note) key-signature
      (+ (cdr (assoc (note-value note) *valid-solfege*))
         (deviation-from-C starting-note)
         (ecase (mode key-signature)
           ((:major :ionian) 0)
           ((:minor :aeolian) 3)
           (:dorian 9)
           (:phrygian 8)
           (:lydian 7)
           (:mixolydian 5)
           (:locrian 1))
         (* (octave note) 12)
         (* (octave starting-note) 12)
         (if (<= (position (tonic key-signature) *valid-solfege*
                           :key #'car
                           :test #'string-equal)
                 (position (note-value note) *valid-solfege*
                           :key #'car
                           :test #'string-equal))
             0
             12)))))

;;; Lyric utilities
;;; ===============
;;; Kana-fication
(defun romaji->kana (romaji)
  "Convert a romaji to kana."
  (gethash ))
