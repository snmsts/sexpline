(defsystem "sexpline"
  :version "1.0.0"
  :description "S-expression line encoding/decoding library inspired by JSONLines"
  :author "SANO Masatoshi"
  :license "MIT"
  :depends-on ("uiop")
  :serial t
  :components ((:file "package")
               (:file "sexpline"))
  :in-order-to ((test-op (test-op "sexpline/tests"))))

(defsystem "sexpline/tests"
  :description "Tests for sexpline"
  :depends-on ("sexpline" "rove")
  :components ((:file "tests"))
  :perform (test-op (op c) (symbol-call :rove :run c)))
