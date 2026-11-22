(in-package #:sat-backend-dpll/tests)

;;; DPLL behaviour: propagation, pure literals, heuristics, statistics, size.

(deftest default-backend-installed
  (ok (typep *backend-at-load* 'dpll-backend) "loading sets *sat-backend* when it was NIL"))

(deftest backend-slots
  (let ((b (make-instance 'dpll-backend)))
    (ok (typep b 'sat-backend))
    (ok (eq :dlis (dpll-backend-heuristic b)))
    (ok (eq t (dpll-backend-pure-literals b)))
    (ok (null (dpll-backend-statistics b)) "no statistics before the first call")))

(deftest unit-propagation-chain
  (let ((b (make-instance 'dpll-backend)))
    (multiple-value-bind (model sat)
        (solve (f '(:and "a" (:implies "a" "b") (:implies "b" "c") (:implies "c" "d")))
               :backend b :complete nil)
      (ok sat)
      (ok (equal '(("a" . t) ("b" . t) ("c" . t) ("d" . t)) model))
      (ok (eql 0 (stats b :decisions)) "no decision needed")
      (ok (eql 4 (stats b :propagations)) "the unit clause and three implications")
      (ok (eql 0 (stats b :conflicts))))))

(deftest pure-literal-elimination
  (let ((b (make-instance 'dpll-backend)))
    (multiple-value-bind (model sat) (solve (f '(:and (:or "a" "b") (:or "a" "c"))) :backend b :complete nil)
      (ok sat)
      (ok (equal '(("a" . t)) model) "the lowest pure literal satisfies both clauses")
      (ok (eql 1 (stats b :pure-literals)))
      (ok (eql 0 (stats b :decisions))))))

(deftest pure-literals-off-with-order
  (let ((b (make-instance 'dpll-backend)))
    (multiple-value-bind (model sat)
        (solve (f '(:and (:or "a" "b") (:or "a" "c"))) :backend b :complete nil
               :order (lambda (names) (car (last names))) :phase :true)
      (ok sat)
      (ok (equal '(("b" . t) ("c" . t)) model) ":order owns every choice")
      (ok (eql 0 (stats b :pure-literals)))
      (ok (eql 2 (stats b :decisions))))))

(deftest pure-literals-slot-off
  (let ((b (make-instance 'dpll-backend :pure-literals nil)))
    (solve (f '(:and (:or "a" "b") (:or "a" "c"))) :backend b :complete nil)
    (ok (eql 0 (stats b :pure-literals)))
    (ok (eql 1 (stats b :decisions)) "DLIS picks a (two occurrences)")))

(deftest heuristic-first
  (let ((b (make-instance 'dpll-backend :heuristic :first :pure-literals nil)))
    (multiple-value-bind (model sat) (solve (f '(:or "a" "b" "c")) :backend b :complete nil)
      (ok sat)
      (ok (equal '(("a" . nil) ("b" . nil) ("c" . t)) model) "lowest variable first, false first")
      (ok (eql 2 (stats b :decisions)))
      (ok (eql 1 (stats b :propagations))))))

(deftest heuristic-dlis
  (let ((b (make-instance 'dpll-backend :pure-literals nil)))
    (multiple-value-bind (model sat)
        (solve (f '(:and (:or "a" "b") (:or (:not "a") "b") (:or "b" "c"))) :backend b :complete nil)
      (ok sat)
      (ok (equal '(("b" . t)) model) "b occurs three times")
      (ok (eql 1 (stats b :decisions))))
    (multiple-value-bind (model sat)
        (solve (f '(:and (:or (:not "a") "b") (:or (:not "a") "c"))) :backend b :complete nil)
      (ok sat)
      (ok (equal '(("a" . nil)) model) "the most frequent literal is -a: phase false"))))

(deftest phase-overrides-dlis
  (let ((b (make-instance 'dpll-backend :pure-literals nil)))
    (multiple-value-bind (model sat)
        (solve (f '(:and (:or (:not "a") "b") (:or (:not "a") "c"))) :backend b :complete nil
               :phase :true)
      (ok sat)
      (ok (equal '(("a" . t) ("b" . t) ("c" . t)) model) "a = T, then b and c propagate"))))

(deftest conflicts-counted
  (let ((b (make-instance 'dpll-backend)))
    (multiple-value-bind (clauses n) (pigeonhole 3 2)
      (ok (null (nth-value 1 (solve (clauses->cnf clauses n) :backend b)))))
    (ok (plusp (stats b :conflicts)))
    (ok (plusp (stats b :decisions)))))

(deftest statistics-reset-per-call
  (let ((b (make-instance 'dpll-backend)))
    (multiple-value-bind (clauses n) (pigeonhole 3 2)
      (solve (clauses->cnf clauses n) :backend b))
    (solve (f "a") :backend b)
    (ok (equal '(0 1 0 0) (list (stats b :decisions) (stats b :propagations)
                                (stats b :pure-literals) (stats b :conflicts))))))

(deftest assumptions-contradiction
  (let ((b (make-instance 'dpll-backend)))
    (ok (null (nth-value 1 (solve (f '(:or "a" "b")) :backend b
                                  :assumptions (list (sat-var "a") (sat-not (sat-var "a")))))))))

(deftest-parametrize larger-instances
    ((file satisfiable)
     :ids ("sat-1" "sat-2" "sat-3" "unsat-1" "unsat-2" "unsat-3")
     ("rand3-50-218-sat-1.cnf" t) ("rand3-50-218-sat-2.cnf" t) ("rand3-50-218-sat-3.cnf" t)
     ("rand3-50-218-unsat-1.cnf" nil) ("rand3-50-218-unsat-2.cnf" nil) ("rand3-50-218-unsat-3.cnf" nil))
  (let ((cnf (read-dimacs (backend-fixture file))))
    (multiple-value-bind (model sat) (solve cnf :backend (make-instance 'dpll-backend))
      (ok (eq satisfiable sat))
      (when sat
        (ok (clauses-satisfied-p (cnf-clauses cnf) (model->assignment model)))))))

(deftest pigeonhole-6-5
  (multiple-value-bind (clauses n) (pigeonhole 6 5)
    (ok (null (nth-value 1 (solve (clauses->cnf clauses n) :backend (make-instance 'dpll-backend)))))))

(deftest random-3sat-models-evaluate
  (let ((rng (make-rng 404)) (sat-count 0))
    (dotimes (i 20)
      (let ((clauses (sat-protocol/conformance:random-3sat rng 40 160)))
        (multiple-value-bind (model sat) (solve (clauses->cnf clauses 40)
                                                :backend (make-instance 'dpll-backend))
          (when sat
            (incf sat-count)
            (ok (clauses-satisfied-p clauses (model->assignment model)) (format nil "instance ~D" i))))))
    (ok (plusp sat-count))))

(deftest budget-on-large-instance
  (let ((cnf (read-dimacs (backend-fixture "rand3-50-218-unsat-1.cnf"))))
    (ok (signals (solve cnf :backend (make-instance 'dpll-backend) :budget 3) 'sat-budget-exceeded))
    (ok (equal '(:stopped nil)
               (handler-bind ((sat-budget-exceeded (lambda (c) (use-value :stopped c))))
                 (multiple-value-list (solve cnf :backend (make-instance 'dpll-backend) :budget 3)))))))
