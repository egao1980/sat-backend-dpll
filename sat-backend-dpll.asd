(defsystem "sat-backend-dpll"
  :version "0.1.0"
  :description "sat-protocol backend: DPLL with unit propagation, pure-literal elimination and decision heuristics"
  :author "egao1980"
  :license "MIT"
  :depends-on ("sat-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")))
