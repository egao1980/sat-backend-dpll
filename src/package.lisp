;;;; sat-backend-dpll: DPLL for sat-protocol. Brief: cl-stack/docs/capabilities/logic.md (L2).

(defpackage #:sat-backend-dpll
  (:use #:cl #:sat-protocol)
  (:export
   #:dpll-backend
   #:dpll-backend-heuristic
   #:dpll-backend-pure-literals
   #:dpll-backend-statistics))

(in-package #:sat-backend-dpll)
