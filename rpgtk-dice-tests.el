;;; rpgtk-dice-tests.el --- Unit Tests for rpgtk-dice.el  -*- lexical-binding: t; -*-
;;
;; © 2023 Howard X. Abrams
;;   Licensed under a Creative Commons Attribution 4.0 International License.
;;   See http://creativecommons.org/licenses/by/4.0/
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams
;; Created:  1 October 2023
;;
;; Obviously, GNU Emacs does not include this file in its distribution.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Emacs Tests for validating functions in the rpgtk-dice.el file.
;;
;; Unit Testing for random numbers is tricky, so what I will want to do is call
;; a function a number of times. A little function that `repeatedly' calls a
;; function with arguments and returns a list of the results should be helpful.

;;; Code:

(require 'rpgtk-dice)

(defun rpgtk-repeatedly (fn args times)
  "Call a function, FN, with a list of arguments, ARGS, a number of TIMES.
Return a list of results."
  (let (value)
    (dotimes (number times value)
      (setq value (cons (apply fn args) value)))))

;; This function will run a large number of runs and verify that all
;; dice rolls fall between two ranges. This is completely accurate, as
;; our dice rolls could be in a smaller subset, so we also check to
;; make sure that at least one roll was at each end. This should be
;; _good enough_.

(defun rpgtk--test-rolls (fn args min max)
  "Run function FN with ARGS to validate all results.
The numbers returned should be between MIN and MAX, with an
average value of AVG, if given."
  (let ((rolls (rpgtk-repeatedly fn args 1000)))
    (should (--some?  (= it min) rolls))
    (should (--some?  (= it max) rolls))
    (should (--every? (>= it min) rolls))
    (should (--every? (<= it max) rolls))))

(defun rpgtk--test-roll-series (fn args min max)
  "Run function FN with ARGS to validate all results.
The numbers returned should be between MIN and MAX, with an
average value of AVG, if given."
  (let ((roll-sums (->> (repeatedly fn args 1000)
                        (-map 'rpgtk--sum))))
    ;; (should (--some?  (= it min) roll-sums))
    ;; (should (--some?  (= it max) roll-sums))
    (should (--every? (>= it min) roll-sums))
    (should (--every? (<= it max) roll-sums))))

(ert-deftest rpgtk--roll-test ()
  (let ((test-data '(((1 6) 1 6)
                     ((3 6 4) 7 22)
                     ((4 6 4 "-") 0 20))))
    (dolist (test-seq test-data)
      (destructuring-bind (dice-args lowest highest) test-seq
        (rpgtk--test-roll-series 'rpgtk--roll dice-args lowest highest)))))

;; ----------------------------------------------------------------------

(ert-deftest rpgtk--roll-dice-test ()
  "Validate the `rpgtk--roll-dice' by making sure we get a list
of all die rolls, and that each number is within the range.
We can assume that `rpgtk--roll-dice' works."
  (let ((results (rpgtk--roll-dice 4 6)))
    (should (= (length results) 4))
    (should (--every? (>= it 1) results))
    (should (--every? (<= it 6) results))))

(ert-deftest rpgtk-roll-test ()
  "We roll 4d6+3, but call `rpgtk-roll' in different ways."
  (let ((results (rpgtk-roll 4 6)))
    (should (= 4 (length (car results))))
    (should (= 0 (cdr results))))
  (let ((results (rpgtk-roll 4 6 3)))
    (should (= 4 (length (car results))))
    (should (= 3 (cdr results))))
  (let ((results (rpgtk-roll 4 6 3 "+")))
    (should (= 4 (length (car results))))
    (should (= 3 (cdr results))))
  (let ((results (rpgtk-roll 4 6 3 "-")))
    (should (= 4 (length (car results))))
    (should (= -3 (cdr results)))))

(ert-deftest rpgtk-roll-with-choice-test ()
  (let* ((high3 (lambda (rolls)
                  (seq-take (seq-sort #'> rolls) 3)))
         (rolls (rpgtk--roll-with-choice 4 6 high3)))
    (should (= 3 (length (car rolls))))
    (should (= 0 (cdr rolls))))
  (let* ((disadv (lambda (rolls)
                  (seq-take (seq-sort #'< rolls) 1)))
         (rolls (rpgtk--roll-with-choice 2 20 disadv)))
    (should (= 1 (length (car rolls))))
    (should (= 0 (cdr rolls)))))

(ert-deftest rpgtk-roll-highest-test ()
  (let ((advang (rpgtk-roll-highest 2 20 1)))
    (should (= 1 (length (car advang))))))

(ert-deftest rpgtk-roll-lowest-test ()
  (let ((advang (rpgtk-roll-lowest 2 20 1)))
    (should (= 1 (length (car advang))))))

(ert-deftest rpgtk--sum-test ()
  (should (= (rpgtk--sum '((1 6) . 3)) 10))
  (should (= (rpgtk--sum '((6) . -3)) 3))
  (should (= (rpgtk--sum '(() . 0)) 0)))

(ert-deftest rpgtk--expression-parts-test ()
  (should (equal (rpgtk--expression-parts "d20") '(1 20 0)))
  (should (equal (rpgtk--expression-parts "3d6") '(3 6 0)))
  (should (equal (rpgtk--expression-parts "d4+4") '(1 4 4)))
  (should (equal (rpgtk--expression-parts "2d4-3") '(2 4 -3)))
  (should (equal (rpgtk--expression-parts "dc") nil)))

(ert-deftest rpgtk--roll-dice-expression-test ()
  (let ((results (rpgtk-roll-dice-expression "2d6+1")))
    (should (= 2 (length (car results))))
    (should (= 1 (cdr results))))

  (let ((test-cases '(("d6" 1 6)
                      ("2d12" 2 24)
                      ("3d6+2" 5 20))))
    (dolist (test-data test-cases)
      (destructuring-bind (dice-expression lowest highest) test-data
        (rpgtk--test-roll-series 'rpgtk-roll-dice-expression
                                 (list dice-expression) lowest highest)))))

(ert-deftest rpgtk--roll-expression-test ()
  "Simple test of my random number generator.
This really tests the `rpgtk--test-rolls' function."
  (dolist (test-data '((4 1 4)
                       (6 1 6)
                       (8 1 8)
                       (20 1 20)
                       (100 1 100)))
    (destructuring-bind (die lowest highest) test-data
      (rpgtk--test-rolls #'rpgtk--roll-die (list die) lowest highest))))

(ert-deftest rpgtk--display-roll-test ()
  (should (equal (rpgtk--display-roll '((1 2 3) . 0)) "6 ... (1 2 3)"))
  (should (equal (rpgtk--display-roll '((1 2 3) . 4)) "10 ... (1 2 3) +4"))
  (should (equal (rpgtk--display-roll '((1 2 3) . -4)) "2 ... (1 2 3) -4"))
  (should (equal (rpgtk--display-roll '((2) . 4)) "6 ... 2 +4"))
  (should (equal (rpgtk--display-roll '((2) . 0)) "2"))
  (should (equal (rpgtk--display-roll '((1 2 3) . 4) "3d6+4") "10 ... (1 2 3) +4 | 3d6+4")))

(ert-deftest rpgtk-roll-em-test ()
  (let ((expr-results (rpgtk-roll-em "3d6+2")))
    (should (= (seq-length expr-results) 3))
    (should (= (seq-length (nth 1 expr-results)) 3))
    (should (> (nth 0 expr-results) 3))
    (should (= (nth 2 expr-results) 2)))
  (let ((item-results (rpgtk-roll-em 3 6 2)))
    (should (= (seq-length item-results) 3))
    (should (= (seq-length (nth 1 item-results)) 3))
    (should (> (nth 0 item-results) 3))
    (should (= (nth 2 item-results) 2))))

(ert-deftest rpgtk-roll-sum-test ()
  (let ((expr-results (rpgtk-roll-sum "3d6+2")))
    (should (>= expr-results 5))
    (should (<= expr-results 20)))
  (let ((expr-results (rpgtk-roll-sum 3 6 2)))
    (should (>= expr-results 5))
    (should (<= expr-results 20))))

(ert-deftest rpgtk-dice-format-string-test ()
  (let ((results (rpgtk-dice-format-string "There are d4+1 dragons")))
    (should (or
             (equal results "There are 2 dragons")
             (equal results "There are 3 dragons")
             (equal results "There are 4 dragons")
             (equal results "There are 5 dragons")))))

;; ----------------------------------------------------------------------
;; INTERACTIVE TESTS
;; ----------------------------------------------------------------------

(defun rpgtk-display-faces ()
  (interactive)
  (message "Critical: %s - Success: %s - Middlin: %s - Failed: %s - Fumbled: %s"
           (propertize "20" 'face 'rpgtk-critical-success-roll)
           (propertize "16" 'face 'rpgtk-successful-roll)
           (propertize "11" 'face 'rpgtk-middlin-roll)
           (propertize "4"  'face 'rpgtk-failed-roll)
           (propertize "1"  'face 'rpgtk-critical-failure-roll)))

;; (defun rpgtk--display-roll (roll-combo &optional expression)
;;   "Convert a ROLL-COMBO.results into a suitable string.
;; The format for a roll combo is described with `rpgtk--sum' function.
;; The EXPRESSION is a string that may have generated the roll combo."
;;   (let ((answer (rpgtk--sum roll-combo))
;;         (die-rolls (car roll-combo))
;;         (modifier (cdr roll-combo)))
;;     (rpgtk--display-roll-parts answer die-rolls modifier expression)))

(defun rpgtk--roll-display (which-one)
  "Display WHICH-ONE type of dice roll."
  (interactive (list (completing-read-value "Which dice roll: "
                                            '(("Critical" critical)
                                              ("Success" success)
                                              ("Failure" failure)
                                              ("Fumble" fumble)
                                              ("Partial success" middlin)
                                              ("None of the above" nil)))))
  (cl-case which-one
    ((critical) (message (rpgtk--display-roll-parts 24 '(20) 4 "d20+4"
                                                    15 nil 20 1)))
    ((success) (message (rpgtk--display-roll-parts 18 '(15) 3 "d20+3"
                                                   15  nil 20 1)))
    ((middlin) (message (rpgtk--display-roll-parts 5 '(2 5 4) 0 "BitD: 3"
                                                   6 4 6)))
    ((failure) (message (rpgtk--display-roll-parts 6 '(3) 3 "d20+3"
                                                   15 nil 20 1)))
    ((fumble) (message (rpgtk--display-roll-parts 6 '(1) 5 "d20+5"
                                                  15 nil 20 1)))
    (t (message (rpgtk--display-roll-parts 18 '(6 8 4) 0 "3d8")))))

(provide 'rpgtk-dice-tests)
;;; rpgtk-dice-tests.el ends here
