(defsystem "sat-backend-dpll"
  :version "0.1.0"
  :description "sat-protocol backend: DPLL with unit propagation, pure-literal elimination and decision heuristics"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "sat-protocol" "0.1.0"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "dpll"))
  :in-order-to ((test-op (test-op "sat-backend-dpll/tests"))))

(defsystem "sat-backend-dpll/tests"
  :depends-on ("sat-backend-dpll" "sat-protocol/conformance" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "dpll-test")
               (:file "oracle-test"))
  :perform (test-op (o c)
             (unless (symbol-call :sat-backend-dpll/tests :run-with-dpll)
               (error "tests failed for ~A" (component-name c)))))
