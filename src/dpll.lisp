;;;; DPLL: cl-repository's backtracking search (src/client/solver.lisp) over CNF,
;;;; plus unit propagation, pure-literal elimination and decision heuristics.
;;;; Class and load-time default GIVEN; BACKEND-SOLVE is a stub.
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

(defun %dpll-pending (what)
  "Contract stub (L2 sat-backend-dpll). Every call site is replaced."
  (error "L2 sat-backend-dpll contract stub not implemented: ~S" what))

(defmethod backend-solve ((backend dpll-backend) cnf &key assumptions order phase budget)
  (declare (ignore cnf assumptions order phase budget))
  (%dpll-pending 'backend-solve))

;;; Loading the backend makes it the default when none is set.
(unless *sat-backend*
  (setf *sat-backend* (make-instance 'dpll-backend)))
