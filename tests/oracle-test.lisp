(in-package #:sat-backend-dpll/tests)

;;; The exact search of dpll.lisp's header, as an oracle, on cl-repository-shaped
;;; problems with the latest-version-first hook (:order, :phase :true, so no
;;; pure literals). The model must equal the oracle's assignment exactly.
;;;
;;; This also records where DPLL and cl-repository's naive search differ:
;;; unit propagation satisfies clauses before the naive search would decide
;;; their other variables, so DPLL leaves unrequired packages unassigned where
;;; the naive search sets them true (about 14% of random package instances in
;;; a 3000-instance experiment, 2026-11-22; DPLL's true set was a subset every
;;; time). See .team-solve/task.md, "cl-repository (deferred)".

(defun dpll-oracle (clauses &key order phase)
  "Assigned literals (sorted by variable) of the DPLL loop with ORDER and PHASE
given (no pure literals), or :UNSAT."
  (let ((assignment (make-hash-table)))
    (labels ((lit-value (lit)
               (multiple-value-bind (v bound) (gethash (abs lit) assignment)
                 (if bound (if (plusp lit) v (not v)) :free)))
             (clause-state (c)
               ;; :sat, :conflict, or the list of unassigned literals.
               (let ((free nil))
                 (dolist (l c (or free :conflict))
                   (let ((v (lit-value l)))
                     (cond ((eq v :free) (push l free))
                           (v (return :sat)))))))
             (propagate ()
               (loop
                 (let ((changed nil))
                   (dolist (c clauses)
                     (let ((s (clause-state c)))
                       (cond ((eq s :conflict) (return-from propagate nil))
                             ((and (listp s) (null (rest s)))
                              (setf (gethash (abs (first s)) assignment) (plusp (first s))
                                    changed t)))))
                   (unless changed (return t)))))
             (candidates ()
               (let ((cands nil))
                 (dolist (c clauses (sort (remove-duplicates cands) #'<))
                   (let ((s (clause-state c)))
                     (when (listp s) (dolist (l s) (push (abs l) cands)))))))
             (snapshot () (let ((h (make-hash-table)))
                            (maphash (lambda (k v) (setf (gethash k h) v)) assignment)
                            h))
             (search-node ()
               (unless (propagate) (return-from search-node nil))
               (let ((cands (candidates)))
                 (when (null cands) (return-from search-node t))
                 (let* ((var (funcall order cands))
                        (first-value (and (funcall phase var) t))
                        (saved (snapshot)))
                   (dolist (value (list first-value (not first-value)) nil)
                     (clrhash assignment)
                     (maphash (lambda (k v) (setf (gethash k assignment) v)) saved)
                     (setf (gethash var assignment) value)
                     (when (search-node) (return t)))))))
      (if (search-node)
          (sort (loop for k being the hash-keys of assignment using (hash-value v)
                      collect (if v k (- k)))
                #'< :key #'abs)
          :unsat))))

(defun random-package-problem (rng)
  "Keyword arguments for PACKAGE-FORMULA: a random cl-repository-shaped problem."
  (let* ((k (+ 2 (rng-int rng 5)))
         (names (loop for i below k collect (format nil "p~D" i)))
         (versions (loop repeat k collect (loop for v from 1 to (1+ (rng-int rng 4))
                                                collect (princ-to-string v))))
         (depends nil))
    (loop for name in names
          for vs in versions
          do (dolist (v vs)
               (dotimes (d (rng-int rng 3))
                 (let ((target (rng-int rng k)))
                   (unless (string= name (nth target names))
                     (let ((ms (remove-if (lambda (m) (declare (ignore m)) (>= (rng-int rng 10) 6))
                                          (nth target versions))))
                       (when ms (push (list* name v (nth target names) ms) depends))))))))
    (list :root (list (first names) (nth (rng-int rng (length (first versions))) (first versions)))
          :packages (mapcar #'cons names versions)
          :depends (nreverse depends)
          :pins (when (zerop (rng-int rng 4))
                  (let ((i (rng-int rng k)))
                    (list (list (nth i names) (nth (rng-int rng (length (nth i versions)))
                                                   (nth i versions)))))))))

(defun model-literals (model cnf)
  "MODEL (an alist) as sorted integer literals over CNF's indices."
  (sort (mapcar (lambda (pair)
                  (let ((i (cnf-variable-index cnf (car pair))))
                    (if (cdr pair) i (- i))))
                model)
        #'< :key #'abs))

(deftest latest-first-matches-oracle
  (let ((rng (make-rng 2211)) (unsat 0))
    (dotimes (i 200)
      (let* ((formula (apply #'package-formula (random-package-problem rng)))
             (cnf (to-cnf formula))
             (expected (dpll-oracle
                        (cnf-clauses cnf)
                        :order (lambda (cands)
                                 (cnf-variable-index
                                  cnf (latest-first-order
                                       (mapcar (lambda (c) (cnf-variable-name cnf c)) cands))))
                        :phase (constantly t))))
        (multiple-value-bind (model sat)
            (solve formula :order #'latest-first-order :phase :true :complete nil
                           :backend (make-instance 'dpll-backend))
          (if (eq expected :unsat)
              (progn (incf unsat) (ok (null sat) (format nil "instance ~D unsat" i)))
              (ok (equal expected (model-literals model cnf)) (format nil "instance ~D assignment" i))))))
    (ok (< 0 unsat 200) "both outcomes exercised")))

(deftest divergence-from-naive-search
  ;; Nothing requires lib, but lib's clause (lib -> app) is open while app is
  ;; undecided, and lib sorts after app: the naive search decides lib = T.
  ;; DPLL propagates the root first, so lib stays unassigned.
  (let ((formula (package-formula :root '("app" "1")
                                  :packages '(("app" "1") ("lib" "1"))
                                  :depends '(("lib" "1" "app" "1")))))
    (ok (equal '("app-v1" "lib-v1")
               (true-names (solve formula :order #'latest-first-order :phase :true :complete nil
                                          :backend (make-instance 'reference-backend))))
        "cl-repository's search installs lib")
    (ok (equal '("app-v1")
               (true-names (solve formula :order #'latest-first-order :phase :true :complete nil
                                          :backend (make-instance 'dpll-backend))))
        "DPLL installs only what is required")))
