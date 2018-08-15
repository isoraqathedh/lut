;;;; lut.asd

(asdf:defsystem #:lut
  :description "Text interface to build UST files."
  :author "Isoraķatheð Zorethan <isoraqathedh.zorethan@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:py-configparser #:alexandria #:split-sequence #:str #:cl-ppcre)
  :components ((:file "package")
               (:file "note-utils")
               (:file "notes-and-settings")
               (:file "note-collections")
               (:file "variables")
               (:file "with-stuff")
               (:file "config-patch")
               (:file "utau-write-2")
               (:file "lut")))
