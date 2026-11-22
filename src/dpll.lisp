;;;; DPLL: cl-repository's backtracking search (src/client/solver.lisp) over CNF,
;;;; plus unit propagation, pure-literal elimination and decision heuristics.
;;;; Class and load-time default GIVEN; BACKEND-SOLVE implements the loop below.
;;;;
;;;; BACKEND-SOLVE (the sat-protocol contract, protocol.lisp) runs exactly this
;;;; loop; the tests pin it (oracle-test.lisp):
;;;;   0. Assign ASSUMPTIONS; two contradicting ones -> (values NIL NIL).
;;;;   1. Unit propagation to a fixpoint: a clause with no true literal and one
;;;;      unassigned literal assigns it (counted in :PROPAGATIONS; a unit clause
;;;;      of the input is propagated too). A clause with every literal false is a
;;;;      conflict (counted in :CONFLICTS): backtrack.
;;;;   2. No clause without a true literal -> success: return the assigned
;;;;      literals. Variables in no open clause stay unassigned.
;;;;   3. Pure literals, only when ORDER is NIL and PURE-LITERALS is true: if some
;;;;      unassigned variable occurs in the open clauses (no true literal) with
;;;;      one polarity only, assign the lowest-index such variable that polarity
;;;;      (counted in :PURE-LITERALS, not a decision) and go to 1.
;;;;   4. Decision (counted in :DECISIONS; BUDGET is checked first with
;;;;      SIGNAL-BUDGET-EXCEEDED). CANDIDATES = ascending unassigned variables of
;;;;      the open clauses. Variable: (funcall ORDER candidates) when ORDER, else
;;;;        :FIRST - the lowest candidate;
;;;;        :DLIS  - the literal occurring in the most open clauses among the
;;;;                 candidates' literals; ties: lower index, then positive.
;;;;      First value: (funcall PHASE var) when PHASE, else :DLIS the chosen
;;;;      literal's polarity, :FIRST NIL. Try it, then the other value;
;;;;      backtracking is chronological and undoes every assignment made after
;;;;      the decision. Both values failing -> backtrack further.
;;;; DPLL-BACKEND-STATISTICS: plist (:decisions n :propagations n
;;;; :pure-literals n :conflicts n) of the last BACKEND-SOLVE call on this
;;;; backend (NIL before the first).

(in-package #:sat-backend-dpll)

(defclass dpll-backend (sat-backend)
  ((heuristic :initarg :heuristic :initform :dlis :reader dpll-backend-heuristic
              :type (member :first :dlis)
              :documentation "Decision heuristic when no :ORDER is given.")
   (pure-literals :initarg :pure-literals :initform t :reader dpll-backend-pure-literals
                  :documentation "Pure-literal elimination when no :ORDER is given.")
   (statistics :initform nil :accessor dpll-backend-statistics
               :documentation "Counters of the last BACKEND-SOLVE call."))
  (:documentation "DPLL with unit propagation, pure literals and DLIS / first-variable decisions."))

(defmethod backend-solve ((backend dpll-backend) cnf &key assumptions order phase budget)
  "DPLL, exactly the loop in this file's header.
Clauses are taken as given (no deduplication): a clause is open while none of
its literal occurrences is true, unit when exactly one occurrence is
unassigned, a conflict when none is; DLIS counts occurrences in open clauses.
With ORDER given and PHASE NIL, the first value is, for :DLIS, the polarity of
the chosen variable with more unassigned occurrences in the open clauses (ties:
true); for :FIRST, NIL."
  (let* ((n (cnf-variable-count cnf))
         (clauses (map 'simple-vector
                       (lambda (c) (coerce c '(simple-array fixnum (*))))
                       (cnf-clauses cnf)))
         (m (length clauses))
         (true-count (make-array m :element-type 'fixnum :initial-element 0))
         (false-count (make-array m :element-type 'fixnum :initial-element 0))
         ;; Clause ids per literal, one entry per occurrence: index 2v for +v,
         ;; 2v+1 for -v.
         (occ (make-array (+ 2 (* 2 n)) :initial-element nil))
         (value (make-array (1+ n) :element-type '(integer -1 1) :initial-element 0))
         ;; Assigned literals; also the propagation queue from QHEAD to TLEN.
         (trail (make-array n :element-type 'fixnum :initial-element 0))
         (pos (make-array (1+ n) :element-type 'fixnum :initial-element 0))
         (neg (make-array (1+ n) :element-type 'fixnum :initial-element 0))
         (tlen 0) (qhead 0)
         (decisions 0) (propagations 0) (pures 0) (conflicts 0)
         (heuristic (dpll-backend-heuristic backend))
         (pure-p (and (null order) (dpll-backend-pure-literals backend))))
    (declare (type fixnum n m tlen qhead decisions propagations pures conflicts)
             (type simple-vector clauses occ)
             (type (simple-array fixnum (*)) true-count false-count trail pos neg)
             (type (simple-array (integer -1 1) (*)) value))
    (labels ((li (l)
               (declare (fixnum l))
               (if (plusp l) (* 2 l) (1+ (* -2 l))))
             (lit-val (l)
               ;; 1 true, -1 false, 0 unassigned.
               (declare (fixnum l))
               (if (plusp l) (aref value l) (- (aref value (- l)))))
             (assign (l)
               (declare (fixnum l))
               (setf (aref value (abs l)) (if (plusp l) 1 -1)
                     (aref trail tlen) l)
               (incf tlen)
               (dolist (c (svref occ (li l))) (incf (aref true-count c)))
               (dolist (c (svref occ (li (- l)))) (incf (aref false-count c))))
             (undo-to (mark)
               ;; Chronological backtracking: undo every assignment after MARK.
               (declare (fixnum mark))
               (loop while (> tlen mark)
                     do (let ((l (aref trail (decf tlen))))
                          (setf (aref value (abs l)) 0)
                          (dolist (c (svref occ (li l))) (decf (aref true-count c)))
                          (dolist (c (svref occ (li (- l)))) (decf (aref false-count c)))))
               (setf qhead (min qhead mark)))
             (check (c)
               ;; Step 1 for clause C: NIL on conflict; assigns a unit literal.
               (declare (fixnum c))
               (or (plusp (aref true-count c))
                   (let* ((cl (svref clauses c))
                          (free (- (length (the (simple-array fixnum (*)) cl))
                                   (aref false-count c))))
                     (declare (fixnum free))
                     (case free
                       (0 nil)
                       (1 (assign (find 0 (the (simple-array fixnum (*)) cl) :key #'lit-val))
                          (incf propagations)
                          t)
                       (t t)))))
             (propagate ()
               ;; Drain the queue to a fixpoint. NIL after counting a conflict.
               (loop while (< qhead tlen)
                     do (let ((l (aref trail qhead)))
                          (incf qhead)
                          (dolist (c (svref occ (li (- l))))
                            (unless (check c)
                              (incf conflicts)
                              (return-from propagate nil)))))
               t)
             (scan ()
               ;; One pass over the open clauses: NIL when none is open (step 2),
               ;; else T with POS / NEG = unassigned occurrences of +v / -v there.
               (fill pos 0)
               (fill neg 0)
               (let ((open nil))
                 (dotimes (c m open)
                   (when (zerop (aref true-count c))
                     (setf open t)
                     (loop for l of-type fixnum across (the (simple-array fixnum (*)) (svref clauses c))
                           when (zerop (lit-val l))
                             do (if (plusp l) (incf (aref pos l)) (incf (aref neg (- l)))))))))
             (candidate-p (v)
               (declare (fixnum v))
               (or (plusp (aref pos v)) (plusp (aref neg v))))
             (pure-literal ()
               ;; Step 3: the lowest-index pure literal, or NIL.
               (loop for v of-type fixnum from 1 to n
                     for p = (aref pos v) for q = (aref neg v)
                     when (and (plusp p) (zerop q)) return v
                     when (and (zerop p) (plusp q)) return (- v)))
             (choose ()
               ;; Step 4: the signed literal tried first.
               (let ((var 0) (dlis-lit nil))
                 (declare (fixnum var))
                 (cond (order
                        (setf var (funcall order (loop for v of-type fixnum from 1 to n
                                                       when (candidate-p v) collect v))))
                       ((eq heuristic :first)
                        (setf var (loop for v of-type fixnum from 1 to n
                                        when (candidate-p v) return v
                                        finally (return 0))))
                       (t
                        ;; Strict > over ascending v, + before -: ties go to the
                        ;; lower index, then positive.
                        (let ((best 0) (lit 0))
                          (declare (fixnum best lit))
                          (loop for v of-type fixnum from 1 to n
                                do (when (> (aref pos v) best) (setf best (aref pos v) lit v))
                                   (when (> (aref neg v) best) (setf best (aref neg v) lit (- v))))
                          (setf dlis-lit lit var (abs lit)))))
                 (if (cond (phase (funcall phase var))
                           (dlis-lit (plusp dlis-lit))
                           ((eq heuristic :dlis) (>= (aref pos var) (aref neg var)))
                           (t nil))
                     var
                     (- var))))
             (publish ()
               (setf (dpll-backend-statistics backend)
                     (list :decisions decisions :propagations propagations
                           :pure-literals pures :conflicts conflicts)))
             (search-node ()
               (unless (propagate) (return-from search-node nil))
               (loop                    ; steps 2 and 3
                 (unless (scan) (return-from search-node t))
                 (let ((p (and pure-p (pure-literal))))
                   (unless p (return))
                   (assign p)
                   (incf pures)
                   (unless (propagate) (return-from search-node nil))))
               (when (and budget (>= decisions budget)) ; step 4
                 (publish)
                 (setf budget (signal-budget-exceeded budget)))
               (incf decisions)
               (let ((lit (choose)) (mark tlen))
                 (declare (fixnum lit mark))
                 (dolist (l (list lit (- lit)) nil)
                   (assign l)
                   (when (search-node) (return t))
                   (undo-to mark)))))
      (publish)                         ; this call's statistics, even on early exit
      (unwind-protect
           (progn
             (dotimes (c m)
               (loop for l of-type fixnum across (the (simple-array fixnum (*)) (svref clauses c))
                     do (push c (svref occ (li l)))))
             (dolist (a assumptions)    ; step 0: neither propagations nor decisions
               (case (lit-val a)
                 (0 (assign a))
                 (-1 (return-from backend-solve (values nil nil)))))
             (dotimes (c m)             ; input unit clauses and empty clauses
               (unless (check c)
                 (incf conflicts)
                 (return-from backend-solve (values nil nil))))
             (if (search-node)
                 (values (loop for i of-type fixnum below tlen collect (aref trail i)) t)
                 (values nil nil)))
        ;; Normal return, UNSAT, the budget's USE-VALUE throw, or an error out of
        ;; ORDER / PHASE.
        (publish)))))

;;; Loading the backend makes it the default when none is set.
(unless *sat-backend*
  (setf *sat-backend* (make-instance 'dpll-backend)))
