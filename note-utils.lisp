;;; note-utils.lisp
;;; Functions for altering notes.

(in-package #:utau-write)

;;; Note utilities
;;; ==============

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

(defun note->note-number (note accidental octave)
  "Turn a note into a number."
  (+ (ecase (normalise-note-letter note)
       (#\C 0) (#\D 2) (#\E 4) (#\F 5)
       (#\G 7) (#\A 9) (#\B 11))
     (ecase (normalise-accidental accidental)
       (:sharp 1) (:flat -1) (:natural 0))
     (* 12 octave)
     12)) ;; offset

(defun %parse-note-name (string)
  "Break down a string into its constituent notes."
  (ecase (length string)
    (2
     (list (char string 0) #\- (parse-integer string :start 1)))
    (3
     (list (char string 0) (char string 1) (parse-integer string :start 2)))))

(defun parse-note-name (note)
  "Turn a note name into a note number."
  (apply #'note->note-number (%parse-note-name note)))

;;; Key transformation
(defun solfege->note-number (solfege octave key mode base-octave)
  "Convert a note in the movable-do system into absolute notes."
  (let ((note-correspondences
          '(#|de = ti|# ("do" . 0)  ("di" . 1)
            ("ra" . 1)  ("re" . 2)  ("ri" . 3)
            ("me" . 3)  ("mi" . 4)
            #|fe = mi|# ("fa" . 5)  ("fi" . 6)
            ("se" . 6)  ("so" . 7)  ("si" . 8)
            ("le" . 8)  ("la" . 9)  ("li" . 10)
            ("te" . 10) ("ti" . 11)
            #|------|#  ("do'" . 12)))
        (tonic
          (case mode
            ((:major :ionian)  "do'")
            ((:minor :aeolian) "la")
            (:dorian           "re")
            (:phrygian "mi")
            (:lydian "fa")
            (:mixolydian "so")
            (:locrian "ti"))))
    (+ (cdr (assoc solfege note-correspondences :test #'string-equal))
       (alexandria:eswitch (key :test #'string-equal)
         ("C" 0) ("C#" 1) ("Db" 1)
         ("D" 2) ("D#" 3) ("Eb" 3)
         ("E" 4)
         ("F" 5) ("F#" 6) ("Gb" 6)
         ("G" 7) ("G#" 8) ("Ab" 8)
         ("A" 9) ("A#" 10) ("Bb" 10)
         ("B" 11))
       (ecase mode
         ((:major :ionian) 0)
         ((:minor :aeolian) 3)
         (:dorian 9)
         (:phrygian 8)
         (:lydian 7)
         (:mixolydian 5)
         (:locrian 1))
       (* octave 12)
       (* base-octave 12)
       (if (<= (position tonic note-correspondences
                         :key #'car
                         :test #'string-equal)
               (position solfege note-correspondences
                         :key #'car
                         :test #'string-equal))
           0
           12))))

(defun %parse-key-signature (string)
  (cond
    ((find #\= string)                  ; numeric style system, e.g. 1 = C, 6=A
     (destructuring-bind (key accidental base-octave)
         (%parse-note-name
          (remove #\Space (subseq string (1+ (position #\= string)))))
       (list
        (if (char-equal accidental #\-)
            (string key)
            (coerce (list key accidental) 'string))
        (ecase (char string 0)
          (#\1 :major) (#\2 :dorian) (#\3 :phrygian)
          (#\4 :lydian) (#\5 :mixolydian) (#\6 :minor)
          (#\7 :locrian))
        base-octave)))
    (t                                  ; Generic system, e.g. C4maj, A3min
     (let (tonic-boundary octave-boundary)
       (ecase (length string)
         (5 (setf tonic-boundary 1 octave-boundary 2))
         (6 (setf tonic-boundary 2 octave-boundary 3)))
       (list (subseq string 0 tonic-boundary)
             (alexandria:switch ((subseq string octave-boundary)
                                 :test #'string-equal)
               ("maj" :major)
               ("min" :minor))
             (parse-integer string
                            :start tonic-boundary
                            :end octave-boundary))))))

(defun parse-solfege (note key-signature)
  "Parse a solfège note and turn it into a note number."
  (destructuring-bind (tonic mode base-octave)
      (%parse-key-signature key-signature)
    (solfege->note-number (subseq note 0 2)
                          (- (count #\' note)
                             (count #\, note))
                          tonic
                          mode
                          base-octave)))

;;; Lyric utilities
;;; ===============
;;; Kana-fication
(defun romaji->kana (romaji)
  "Convert a romaji to kana."
  (gethash ))

