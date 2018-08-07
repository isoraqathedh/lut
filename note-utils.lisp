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

(defgeneric note-name (note)
  (:method ((object absolute-note))
    (with-slots (value accidental octave) object
      (format nil "~c~c~d"
              value
              (ecase accidental
                (:sharp #\♯) (:flat #\♭) (:natural #\-))
              octave))))

(defmethod print-object ((object absolute-note) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (note-name object))))

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
  (alexandria:switch (thing :test (lambda (needle haystack)
                                    (or (eql haystack t)
                                        (member needle haystack :test #'equal))))
    ((list "sharp" "#" #\# "♯" #\♯ :sharp) :sharp)
    ((list "flat" "b" #\b "♭" #\♭ :flat) :flat)
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

(defun parse-note-name (string &key (start 0) end)
  "Parse a note name into the corresponding object."
  (cl-ppcre:register-groups-bind (note accidental octave)
      ("^([A-Ga-g])([-#♯b♭ ])?([0-9])*$" string
       :start start :end end :sharedp t)
    (make-instance 'absolute-note
                   :value (normalise-note-letter note)
                   :accidental (if accidental
                                   (normalise-accidental accidental)
                                   :natural)
                   :octave (if octave
                               (parse-integer octave)
                               4))))

(defun parse-solfege-name (string &key (start 0) end)
  "Parse a solfege into the corresponding object."
  (let ((octave-boundary (+ start 2)))
    (make-instance
     'solfege
     :value (normalise-solfege
             (subseq string start octave-boundary))
     :octave (- (count #\' string :start octave-boundary :end end)
                (count #\, string :start octave-boundary :end end)))))

(defun parse-note (string)
  "Parse a note.

First try reading it as a solfege.
If that fails, read it as an absolute note name.
If that fails, read it as a note number.
If that fails, signal an error."
  (or (ignore-errors (parse-solfege-name string))
      (ignore-errors (parse-note-name string))
      (parse-integer string :junk-allowed t)
      (error "~s cannot be parsed as a valid note." string)))

(defun parse-key-signature (string &key (start 0) end)
  "Parse a key signature of a given form into the corresponding object."
  (let ((substring (subseq string start end)))
    (cond
      ((find #\= substring)             ; numeric style system, e.g. 1 = C, 6=A
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
      (t                                ; Generic system, e.g. C4 maj, A3 min
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

(defgeneric deviation-from-C (absolute-note)
  (:documentation "Calculate the number of half-steps above the next lower C.")
  (:method ((note absolute-note))
    (with-slots (value accidental) note
      (+ (ecase value
           (#\C 0) (#\D 2) (#\E 4) (#\F 5)
           (#\G 7) (#\A 9) (#\B 11))
         (ecase accidental
           (:sharp 1) (:flat -1) (:natural 0))))))

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

;;; Time signatures
;;; ===============
(defun parse-time-signature (string)
  "Parse a time signature."
  (cond ((string-equal string "common") #(4 4))
        ((string-equal string "cut-common") #(2 2))
        ((= 1 (count #\/ string))
         (map 'vector
              #'parse-integer
              (split-sequence:split-sequence #\/ string)))
        (t (error "Cannot parse string into a time signature: ~s" string))))

;;; Lyric utilities
;;; ===============
;;; Kana-fication
(defvar *common-conversion*
  (alexandria:plist-hash-table
   '("a"   "あ"   "i"  "い"   "u"   "う"    "e"  "え"   "o"   "お"
     "ka"  "か"   "ki" "き"   "ku"  "く"    "ke" "け"   "ko"  "こ"
     "kya" "きゃ"             "kyu" "きゅ"              "kyo" "きょ"
     "ga"  "が"   "gi" "ぎ"   "gu"  "ぐ"    "ge" "げ"   "go"  "ご"
     "gya" "ぎゃ"             "gyu" "ぎゅ"              "gyo" "ぎょ"
     "sa"  "さ"               "su"  "す"    "se" "せ"   "so"  "そ"
     "za"  "ざ"   "zi" "じ"   "zu"  "ず"    "ze" "ぜ"   "zo"  "ぞ"
     "ta"  "た"   "ti" "ち"   "tu"  "つ"    "te" "て"   "to"  "と"
     "da"  "だ"                             "de" "で"   "do"  "ど"
     "na"  "な"   "ni" "に"   "nu"  "ぬ"    "ne" "ね"   "no"  "の"
     "nya" "にゃ"             "nyu" "にゅ"              "nyo" "にょ"
     "ha"  "は"   "hi" "ひ"                 "he" "へ"   "ho"  "ほ"
     "fa"  "ふぁ" "fi" "ふぃ"               "fe" "ふぇ" "fo"  "ふぉ"
     "hya" "ひゅ"             "hyu" "ひゃ"              "hyo" "ひょ"
     "ba"  "ば"   "bi" "び"   "bu"  "ぶ"    "be" "べ"   "bo"  "ぼ"
     "bya" "びゃ"             "byu" "びゅ"              "byo" "びょ"
     "pa"  "ぱ"   "pi" "ぴ"   "pu"  "ぷ"    "pe" "ぺ"   "po"  "ぽ"
     "pya" "びゃ"             "pyu" "びゅ"              "pyo" "びょ"
     "ma"  "ま"   "mi" "み"   "mu"  "む"    "me" "め"   "mo"  "も"
     "mya" "みゃ"             "myu" "みゅ"              "myo" "みょ"
     "ra"  "ら"   "ri" "り"   "ru"  "る"    "re" "れ"   "ro"  "ろ"
     "rya" "りゃ"             "ryu" "りゅ"              "ryo" "りょ"
     "ya"  "や"               "yu"  "ゆ"    "ye" "いぇ" "yo"  "よ"
     "wa"  "わ"   "wi" "うぃ"               "we" "うぇ" "wo"  "うぉ"
     "va"  "ゔぁ" "vi" "ゔぃ" "vu"  "ゔ"    "ve" "ゔぇ" "vo"  "ゔぉ"
     "nn"  "ん")
   :test #'equal))

(defvar *nihon-shiki*
  (loop
    with nhs = (alexandria:copy-hash-table *common-conversion*)
    for (latin . kana) in
    '(("sya"."しゃ") ("syi"."すぃ") ("syu"."しゅ") ("sye"."しぇ") ("syo"."しょ")
      ("zya"."じゃ") ("zyi"."じ")   ("zyu"."じゅ") ("zye"."じぇ") ("zyo"."じょ")
      #|spacer|#     ("si"."し")
      ("tya"."ちゃ") ("tyi"."てぃ") ("tyu"."ちゅ") ("tye"."ちぇ") ("tyo"."ちょ")
      #|spacer|#     ("ti"."ち")    ("tu" ."つ")
      ("dya"."ぢゃ") ("dyi"."でぃ") ("dyu"."ぢゅ") ("dye"."ぢぇ") ("dyo"."ぢょ")
      #|spacer|#     ("di"."ぢ")    ("du" ."づ")
      #|spacer|#                    ("deyu"."でゅ")
      #|spacer|#                    ("hwu"."ほぅ")
      #|spacer|#                    ("hu" ."ふ")
      ("nn" . "ん"))
    do (setf (gethash latin nhs) kana)
    finally (return nhs))
  "The conversion table for the nihon-shiki Romanisation scheme.")

(defvar *hepburn*
  (loop
    with nhs = (alexandria:copy-hash-table *common-conversion*)
    for (latin . kana) in
    '(("sha"."しゃ") ("shi"."し")  ("shu"."しゅ") ("she"."しぇ") ("sho"."しょ")
      ("ja"."じゃ")  ("ji"."じ")   ("ju"."じゅ")  ("je"."じぇ")  ("jo"."じょ")
      #|spacer|#     ("si"."すぃ")
      ("cha"."ちゃ") ("chi"."ち")  ("chu"."ちゅ") ("che"."ちぇ") ("cho"."ちょ")
      #|spacer|#     ("ti"."てぃ") ("tu" ."とぅ")
      #|spacer|#     ("di"."でぃ") ("du" ."どぅ")
      #|spacer|#                   ("dyu"."でゅ")
      ("fa"."ふゃ")  ("fi"."ふぃ") ("fu" ."ふ")   ("fe"."ふぇ") ("fo"."ふょ")
      #|spacer|#                   ("hu" ."ほぅ")
      ("n" . "ん"))
    do (setf (gethash latin nhs) kana)
    finally (return nhs))
  "The conversion table for the hepburn-shiki Romanisation scheme.")

(defun romaji->kana (romaji scheme &key (start 0) end)
  "Convert a romaji to kana."
  (gethash (subseq romaji start end)
           (ecase scheme
             ((lut:hepburn :hepburn hepburn) *hepburn*)
             ((lut:nihon :nihon nihon) *nihon-shiki*))))

(defun kanafy-string (string scheme)
  "Convert a romaji-only string to a kana, as appropriate."
  ;; We only need to deal with CV and VCV banks,
  ;; as those are the most common types that need to turn into kana anyway.
  (destructuring-bind (prefix &optional main)
      (split-sequence:split-sequence #\Space string)
    (when (null main)
      (rotatef prefix main))
    (format nil "~:[~;~:*~a ~]~a" prefix (romaji->kana main scheme))))
