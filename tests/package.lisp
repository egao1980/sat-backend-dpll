;;;; sat-backend-dpll/tests: sat-protocol's conformance suite on DPLL-BACKEND
;;;; (default :dlis heuristic, and again with :first and no pure literals), plus
;;;; DPLL-specific tests. RUN-WITH-DPLL is what test-op calls.

(defpackage #:sat-backend-dpll/tests
  (:use #:cl #:rove #:sat-protocol #:sat-backend-dpll)
  (:import-from #:sat-protocol/conformance
                #:run-conformance
                #:reference-backend
                #:f
                #:make-rng
                #:rng-int
                #:pigeonhole
                #:clauses->cnf
                #:clauses-satisfied-p
                #:model->assignment
                #:true-names
                #:latest-first-order
                #:pkg-var
                #:package-formula)
  (:export #:run-with-dpll))

(in-package #:sat-backend-dpll/tests)

(defparameter *backend-at-load* *sat-backend*
  "*SAT-BACKEND* right after sat-backend-dpll loaded (before any test binds it).")

(defun run-with-dpll (&key (style :spec))
  (let ((default (let ((*sat-backend* (make-instance 'dpll-backend)))
                   (rove:run '("sat-protocol/conformance" "sat-backend-dpll/tests") :style style)))
        (first-no-pure (run-conformance (make-instance 'dpll-backend :heuristic :first
                                                                     :pure-literals nil)
                                        :style style)))
    (and default first-no-pure)))

(defun backend-fixture (name)
  (asdf:system-relative-pathname "sat-backend-dpll" (format nil "tests/fixtures/~A" name)))

(defun stats (backend key)
  (getf (dpll-backend-statistics backend) key))
