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

(defgeneric add-note (note-collection note)
  (:documentation "Add a note "))

(defgeneric drop-note (note-collection)
  (:documentation "Drop the first note from the collection."))

(defgeneric collection-length (note-collection)
  (:documentation "Calculate the length of the collection of notes."))

(defgeneric measure-defecit (measure)
  (:documentation ))
