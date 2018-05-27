(in-package #:py-configparser)

;; A small patch to py-configparser.
;; By default it prints spaces to separate option and delimiter,
;; but we don't want that, so we'll patch that here.

;; Also we want CRLF, not LF explicitly,
;; so we'll patch it as well.

(defun utau-write::write-stream (config stream)
  "Writes the configuration file corresponding to the
in-memory config state. Reloading the file
with `read-stream' or `read-files' will restore config state."
  (flet ((write-section (section)
           (format stream "[~a]~a" (section-name section) utau-write::*crlf*)
           (format stream "~:{~A=~{~A~A~}~}~A"
                   (mapcar #'(lambda (option)
                               (list (car option)
                                     (list (%format-value (cdr option))
                                           utau-write::*crlf*)))
                           (section-options section))
                   utau-write::*crlf*)))
    (let ((*print-radix* nil)
          (*print-base* 10))
      ;; set the printer output as expected by python
      (when (defaults config)
        ;; write the defaults too!!
        (write-section (config-defaults config)))
      (mapcar #'write-section (config-sections config)))))
