;;;; org.unaen.cl.sets/sets.lisp

(uiop:define-package #:org.unaen.cl.sets
  (:documentation "Simple Set source for sets.")
  (:shadow #:set)
  (:use #:common-lisp)
  (:export #:set
           #:*set-test*
           #:setp
           #:sets-list-p
           #:sets-list
           #:set-add-element
	   #:set-add-set-elements
           #:set-get-element
           #:set-del-element
           #:set-do-elements
	   #:set-not-default-element
	   #:set-default-element
	   #:set-find-element
           #:set->list
           #:set-member-p
           #:set-size
	   #:empty-set-p
           #:set-union
           #:set-intersect
           #:set-diff
           #:set-symm-diff
	   #:*elt-equal-test*
           #:set-equal))

(in-package #:org.unaen.cl.sets)

#| ---------- Standard Set Definition and Atomic Operations ------------------ |#
(defparameter *set-test* 'equal)

(defclass set ()
  ((members :initform (make-hash-table :test *set-test*))))

(defmethod print-object ((object set) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "{~{~a~^ ~}}"
	    (loop :for key :being :the :hash-keys :of (slot-value object 'members)
		  :collect key))))

(defun set-not-default-element (element)
  (not (null element)))

(defun set-default-element (element)
  (null element))

(defun set-add-element (element set)
  (declare (type set set))
  (if (and element
	   (setf (gethash element (slot-value set 'members)) t))
      element
      nil))

(defun set-add-set-elements (elements set)
  (declare (type (or set (eql nil)) elements) (type set set))
  (etypecase elements
    (set (when (loop :for element :being :the :hash-keys of (slot-value elements 'members) :always (set-add-element element set))
           elements))
    (null nil)))

(defun set-get-element (element set)
  (declare (type set set))
  (let ((element-present-p (gethash element (slot-value set 'members))))
    (when element-present-p
      element)))

(defun set-del-element (element set)
  (declare (type set set))
  (when (remhash element (slot-value set 'members))
    element))

(defun set-size (set)
  (declare (type set set))
  (hash-table-count (slot-value set 'members)))

(defun empty-set-p (set)
  (declare (type set set))
  (= 0 (hash-table-count (slot-value set 'members))))

(defun map-elements (element-function set)
  (declare (type function element-function) (type set set))
  (maphash #'(lambda (key value)
	       (declare (ignore value))
	       (funcall element-function key))
           (slot-value set 'members))
  nil)

(defmacro set-do-elements ((element-variable set &optional result) &body body)
  "Like DOLIST but over the elements of a set instead."
  `(progn
     (map-elements #'(lambda (,element-variable)
                       ,@body)
                   ,set)
     ,result))

(defun set-find-element (element set &key (test #'equal))
  "Iterate over every object in set according to comparision predicate function :TEST and return first object that matches."
  (declare (type set set) (type function test))
  (set-do-elements (element-of-set set)
    (when (funcall test element element-of-set)
      (return-from set-find-element element-of-set))))

#| ---------- Standard Set Types --------------------------------------------- |#
(defgeneric setp (object)
  (:method ((set set))
    t)
  (:method (object)
    nil))

(defgeneric sets-list-p (object)
  (:method ((object-list cons))
    (every #'setp
	   object-list))
  (:method ((object null))
    t)
  (:method (object)
    nil))

(deftype sets-list ()
  `(satisfies sets-list-p))

#| ---------- Set Iteration and Comparison ---------------------------------- |#
;; I want to compare all the elements in two sets by iterating over the elements in the first set against those same elements in the second set with a comparison predicate for which the outcome determines if an operation is run on the return set; Then doing the same thing again with the respective two sets reversed, however, if the element already exists in the first set then we assume the predicate was already ran, and so we don't need to run it again.
(defun set-compare-1-to-2 (elt-comp-fn set-1 set-2 set-result)
  "Compare the elements of set-1 to set-2 by iterating only on the elements in set-1 using element-compare-fn and then perform its return action."
  (declare (type function elt-comp-fn) (type set set-1 set-2 set-result))
    (set-do-elements (elt-of-set-1 set-1 set-result)
      (funcall (funcall elt-comp-fn
                        #'(lambda () (set-get-element elt-of-set-1 set-1))
                        #'(lambda () (set-get-element elt-of-set-1 set-2)))
               elt-of-set-1
               set-result)))

(defun set-compare (elt-comp-fn set-1 set-2 set-result)
  "Compare the elements of set-1 to set-2 by iterating on all the elements of both using element-compare-fn and performing its return action at most once."
  (declare (type function elt-comp-fn) (type set set-1 set-2 set-result))
  (set-compare-1-to-2 #'(lambda (elt-set-2-p elt-set-1-p)
                          (if (funcall elt-set-2-p)
                              #'no-action
                              (funcall elt-comp-fn elt-set-2-p elt-set-1-p)))
                      set-2
                      set-1
                      (set-compare-1-to-2 elt-comp-fn set-1 set-2 set-result)))

(defun no-action (element set)
  (declare (ignore element set)
	   (type set set))
  nil)

(defun elt-or-add/ign (elt-set-1-p elt-set-2-p)
  (declare (type function elt-set-1-p elt-set-2-p))
  (if (or (funcall elt-set-1-p) (funcall elt-set-2-p))
      #'set-add-element
      #'no-action))

(defun elt-and-add/del (elt-set-1-p elt-set-2-p)
  (declare (type function elt-set-1-p elt-set-2-p))
  (if (and (funcall elt-set-1-p) (funcall elt-set-2-p))
      #'set-add-element
      #'set-del-element))

(defun elt-left-xor-add/del (elt-set-1-p elt-set-2-p)
  (declare (type function elt-set-1-p elt-set-2-p))
  (if (and (funcall elt-set-1-p) (not (funcall elt-set-2-p)))
      #'set-add-element
      #'set-del-element))

(defun elt-left-xor-add/ign (elt-set-1-p elt-set-2-p)
  (declare (type function elt-set-1-p elt-set-2-p))
  (if (and (funcall elt-set-1-p) (not (funcall elt-set-2-p)))
      #'set-add-element
      #'no-action))

(defun elt-right-xor-add/del (elt-set-2-p elt-set-1-p)
  (declare (type function elt-set-1-p elt-set-2-p))
  (if (and (funcall elt-set-2-p) (not (funcall elt-set-1-p)))
      #'set-add-element
      #'set-del-element))

(defvar *elt-equal-test* #'equal)

(defun elt-equal (elt-set-1-p elt-set-2-p)
  (declare (type function elt-set-1-p elt-set-2-p))
  (if (funcall *elt-equal-test* (funcall elt-set-1-p) (funcall elt-set-2-p))
      #'no-action
      (throw 'elements-not-equal nil)))

#| ---------- Set Utility ---------------------------------------------------- |#
(defun set->list (set)
  (declare (type set set))
  (let ((set-list (list)))
    (set-do-elements (elt set (nreverse set-list))
      (push elt set-list))))

(defgeneric set-build (element set)
  (:documentation "Add elements from an existing set, list, or individual elements to set.")
  (:method ((set->elts set) (set set))
    (set-do-elements (elt set->elts)
      (set-add-element elt set)))
  (:method ((list->elts cons) (set set))
    (dolist (elt list->elts)
      (set-add-element elt set)))
  (:method (elt (set set))
    (set-add-element elt set)
    nil))

#| ---------- Fundamental Mathematical Set Operations ------------------------ |#
(defun set-member-p (element set)
  (declare (type set set))
  (if (and element
	   (gethash element (slot-value set 'members)))
      t
      nil))

(defun set (&rest elements)
  (let ((set (make-instance 'set)))
    (dolist (elt elements set)
      (set-build elt set))))

;; I wanted to keep the same pattern with the rest of the set operations, so the
;; newly consed set starts out with the elements of set-1 in it, and reduce adds
;; the elements of set-2 to set-n
(defun set-union (set-1 set-2 &rest set-ns)
  (declare (type set set-1 set-2)
	   (type sets-list set-ns))
  (let ((sets (list* set-1
		     set-2
		     set-ns))
        (set-union (set set-1))) ;Start off with elts in set-1.
    (reduce #'(lambda (set-1 set-2)
                (set-compare-1-to-2 #'elt-or-add/ign
                                    set-2
                                    set-1
                                    set-union)) ;Iterate on elts of right set and add those elts.
            sets)))

(defun set-intersect (set-1 set-2 &rest set-ns)
  (declare (type set set-1 set-2)
	   (type sets-list set-ns))
  (let ((sets (sort (list* set-1
			   set-2
			   set-ns)
		    #'<=
		    :key #'set-size))
        (set-intersect (set)))
    (reduce #'(lambda (set-1 set-2)
                (set-compare-1-to-2 #'elt-and-add/del
                                    set-1
                                    set-2
                                    set-intersect)) ;Add elts of L set in R set, del elts not in R sets.
            sets)))

(defun set-diff (set-1 set-2 &rest set-ns)
  (declare (type set set-1 set-2)
	   (type sets-list set-ns)) 
  (let ((sets (list* set-1
		     set-2
		     set-ns))
        (set-diff (set)))
    (reduce #'(lambda (set-1 set-2)
                (set-compare-1-to-2 #'elt-left-xor-add/del
                                    set-1
                                    set-2
                                    set-diff)) ;Add elts of L set not in R set, del elts also in R sets.
            sets)))

(defun set-symm-diff (set-1 set-2 &rest set-ns)
  (declare (type set set-1 set-2)
	   (type sets-list set-ns)) 
  (let ((sets (list* set-1
		     set-2
		     set-ns))
        (set-symm-diff (set)))
    (reduce #'(lambda (set-1 set-2)
`                (set-compare-1-to-2 #'elt-left-xor-add/ign
                                    set-1
                                    set-2
                                    set-symm-diff) ;Add elts of L set not in R set, otherwise ignore elt.
                (set-compare-1-to-2 #'elt-right-xor-add/del
                                    set-2
                                    set-1
                                    set-symm-diff)) ;Add elts of R set not in L set, otherwise del elt.
            sets)))

;; Note: add change for fold left/right options.
(defun set-equal (set-1 set-2 &rest set-ns)
  "Sets are set-equal if they are equal set-size and all their members are EQUAL; => Right most equal set | nil."
  (declare (type set set-1 set-2)
	   (type sets-list set-ns)) 
  (let ((sets (list* set-1
		     set-2
		     set-ns)))
    (when (reduce #'= sets :key #'set-size)
      (catch 'elements-not-equal ;=> nil
        (reduce #'(lambda (set-1 set-2)
                    (set-compare-1-to-2 #'elt-equal
                                        set-1
                                        set-2
                                        set-2))
                sets))))) ;=> set-2

