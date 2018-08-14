;;;; variables.lisp

(defclass lut-variable ()
  ((name :reader name
         :initarg :name
         :initform (error "Must provide name for variable.")))
  (:documentation "A note collection with a name, which can be repeated later."))

(defmethod print-object ((object lut-variable) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (name object))))

(define-condition variable-not-found (error)
  ((variable-name :accessor variable-name
                  :initarg :variable-name)
   (variable-store :accessor variable-store
                   :initarg :variable-store))
  (:documentation "Error signalled when a variable is not found.")
  (:report (lambda (c s)
             (format s "Variable ~s is not found in store ~s."
                     (variable-name c)
                     (variable-store c)))))

(defgeneric get-variable (lut-file variable-name)
  (:documentation "Get the object behind the name of the variable.

Signal an error if the variable is not found.")
  (:method ((lut-file lut-file) variable-name)
    (let ((store (variable-store lut-file)))
      (or (gethash variable-name store)
          (restart-case (error 'variable-not-found
                               :variable-name variable-name
                               :variable-store (variable-store lut-file))
            (use-value (value) value)
            (use-empty-collection ()
              :report "Return an empty collection."
              (make-instance 'note-collection)))))))

(defgeneric (setf get-variable) (value lut-file variable-name)
  (:documentation "Set the name VARIABLE-NAME to mean VALUE in LUT-FILE.")
  (:method ((value note-collection) (lut-file lut-file) variable-name)
    (setf (gethash variable-name (variable-store lut-file)) value)))
