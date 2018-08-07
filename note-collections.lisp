;;;; note-collections.lisp
;;; This file deals with bundling notes into collections.
;;; These can be as simple as a list free of structure
;;; or a measure with a specified length
;;; or a bunch of measures for the purposes of repetition.

(defclass note-collection ()
  ((store
    :accessor note-store
    :initform ()))
  (:documentation "A collection of notes."))

(defclass measure (note-collection)
  ((intended-length
    :accessor intended-length
    :initform nil
    :initarg :intended-length
    :documentation "The intended length of the measure in quarter-notes,
or nil for no specific length."))
  (:documentation "A note-collection that is also a measure."))

(defmethod print-object ((object measure) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~d" (intended-length object))))

(defgeneric add-note (note-collection note)
  (:documentation "Push a note to the collection at the beginning.")
  (:method ((note-collection note-collection) (note note))
    (push note (note-store note-collection))))

(defgeneric drop-note (note-collection)
  (:documentation "Drop the last note from the collection.")
  (:method ((note-collection note-collection))
    (pop (note-store note-collection))))

(defgeneric collection-length (note-collection)
  (:documentation "Calculate the length of the collection of notes.")
  (:method ((note-collection note-collection))
    (reduce #'+ (note-store note-collection) :key #'duration))
  (:method ((note note))
    (duration note)))

(defgeneric get-notes (collection)
  (:documentation "Get all notes from the collection.

Use this method rather than accessing the note-store directly.")
  (:method ((note-collection note-collection))
    (reverse (note-store note-collection))))

;;; Measure-specific methods and conditions
(define-condition measure-incomplete-error (error)
  ((measure :accessor measure
            :initarg :measure))
  (:documentation "Error showing that the measure is incomplete.")
  (:report (lambda (c s)
             (format s "The measure ~s is not complete (~d expected, got ~d)"
                     (measure c)
                     (intended-length (measure c))
                     (collection-length (measure c))))))

(define-condition measure-not-full-error (measure-incomplete-error) ())
(define-condition measure-overfull-error (measure-incomplete-error) ())

(defgeneric measure-deficit (measure)
  (:documentation "Compute how much space is left in the measure.")
  (:method ((measure measure))
    (- (collection-length measure)
       (intended-length measure))))

(defgeneric assure-measure-complete (measure)
  (:documentation "Assure that the measure is completed.

If the store has the correct amount of digits, this function returns nil.
Else, the ")
  (:method ((measure measure))
    (let ((deficit (measure-deficit measure)))
      (cond
        ((plusp deficit)  (error 'measure-overfull-error :measure measure))
        ((zerop deficit)  nil)
        ((minusp deficit) (error 'measure-not-full-error :measure measure))))))

;;; Measure fixing things.
(defgeneric pad-measure (measure)
  (:documentation
   "Add a rest to the measure to pad the measure to its intended length.")
  (:method ((measure measure))
    (add-note (make-instance
               'finalised-note
               :duration (abs (measure-deficit measure)))
              measure)))

(defgeneric clip-measure (measure)
  (:documentation "Clip the measure to its intended length.")
  (:method ((measure measure))
    (loop for dropped-note = (drop-note measure)
          for current-duration = (collection-length measure)
          when (< current-duration (intended-length measure))
          do (setf (duration dropped-note)
                   (- (intended-length measure) current-duration))
             (add-note dropped-note measure)
          and return measure)))
