;;;; parameter-optimization.asd

(asdf:defsystem #:parameter-optimization
  :description "Tries to find good noisy parameter values according to user-input scoring, through an adapted version of the cross-entropy method."
  :author "Nuno Trocado"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:cl-randist)
  :components ((:file "package")
               (:file "parameter-optimization")))
