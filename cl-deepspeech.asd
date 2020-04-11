;;;; +----------------------------------------------------------------+
;;;; | cl-deepspeech                                                  |
;;;; +----------------------------------------------------------------+

(asdf:defsystem #:cl-deepspeech
  :description "DeepSpeech bindings for Common Lisp"
  :author "death <github.com/death>"
  :license "MIT"
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("cl-deepspeech/all"))
