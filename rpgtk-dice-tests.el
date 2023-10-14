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

(add-to-list 'load-path
             (file-name-directory (buffer-file-name)))

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

(defun rpgtk-dice--test-rolls (fn args min max)
  "Run function FN with ARGS to validate all results.
The numbers returned should be between MIN and MAX, with an
average value of AVG, if given."
  (let ((rolls (rpgtk-repeatedly fn args 1000)))
    (should (= (seq-min rolls) min))
    (should (= (seq-max rolls) max))))

;; ----------------------------------------------------------------------

(ert-deftest rpgtk-dice-roll-dice-test ()
  "Validate the `rpgtk--roll-dice' by making sure we get a list
of all die rolls, and that each number is within the range.
We can assume that `rpgtk--roll-dice' works."
  (let ((results (rpgtk-dice-roll 4 6)))
    (should (= (length results) 4))
    (should (--every? (>= it 1) results))
    (should (--every? (<= it 6) results))))

;; ----------------------------------------------------------------------

(ert-deftest rpgtk-dice--split-roll-mods-test ()
  (should
   (equal (rpgtk-dice--split-roll-mods '(:foo 3 4 :bar 7 :baz))
          '(:foo (3 4) (:bar 7 :baz))))
  (should
   (equal (rpgtk-dice--split-roll-mods '(:foo 7))
          '(:foo (7) nil)))
  (should
   (equal (rpgtk-dice--split-roll-mods '(:foo))
          '(:foo nil nil)))
  (should
   (equal (rpgtk-dice--split-roll-mods nil)
          '(nil nil nil))))

(ert-deftest rpgtk-dice-roll-mod-test ()
  ;; D&D Style: 4d6+4
  (let ((rolls (rpgtk-dice-roll-mod '(1 4 2 3)
                           :sum
                           :add 4
                           :sum)))
    ;; we should get the original dice roll, plus 3 transforms:
    (should (= 4 (length rolls)))
    (should (= 10 (car (nth 1 rolls))))
    (should (= 14 (car (nth 3 rolls)))))

  ;; What about a Blades in the Dark roll:
  (let ((rolls (rpgtk-dice-roll-mod '(1 4 2 3) :max)))
    (should (equal '(4)  (nth 1 rolls))))

  ;; Test the top function for 4d6 with topk of 3:
  (let* ((rolls (rpgtk-dice-roll-mod '(1 4 2 3) :top 3)))
    (should (equal '(4 3 2) (nth 1 rolls))))

  ;; Test the filter function for arbitrary rolls:
  (let* ((rolls (rpgtk-dice-roll-mod '(1 4 2 3)
                                     :filter 'evenp
                                     :min)))
    (should (equal '(2)  (nth 2 rolls))))

  ;; Test the map function for arbitrary transformations
  (let* ((rolls (rpgtk-dice-roll-mod '(1 4 2 3)
                                     :map '1+)))
    (should (equal '(2 5 3 4)  (nth 1 rolls)))))

(ert-deftest rpgtk-dice-roll-dnd-test ()
  (let ((ds (rpgtk-dice-roll-dnd 3 8 2)))
    ;; A D&D dice roll will have four transformations:
    (should (= (seq-length ds) 4))
    ;; The third modification adds a 2 to the result:
    (should (= (nth 1 (nth 2 ds)) 2))))

;; ----------------------------------------------------------------------

(ert-deftest rpgtk-dice-roll-regexp-test ()
  (should (string-match rpgtk-dice-roll-regexp "d20"))
  (should (string-match rpgtk-dice-roll-regexp "4d20"))
  (should (string-match rpgtk-dice-roll-regexp "4d20+4"))
  (should (string-match rpgtk-dice-roll-regexp "4d20-4"))
  (should (not (string-match rpgtk-dice-roll-regexp "4+4")))
  (should (not (string-match rpgtk-dice-roll-regexp "foobar"))))

(ert-deftest rpgtk-dice--expression-parts-test ()
  (let ((results (rpgtk-dice--expression-parts "2d6+1")))
    (should (= 2 (nth 0 results)))
    (should (= 6 (nth 1 results)))
    (should (= 1 (nth 2 results)))))

(ert-deftest rpgtk-dice-roll-expression-test ()
  (let ((tester-func (lambda (expr) (rpgtk-dice-last
                                (rpgtk-dice-roll-expression expr))))
        (test-cases '(("d6" 1 6)
                      ("2d12" 2 24)
                      ("3d6+2" 5 20))))
    (dolist (test-data test-cases)
      (seq-let (dice-expression lowest highest) test-data
        (rpgtk-dice--test-rolls tester-func
                    (list dice-expression) lowest highest)))))

(ert-deftest rpgtk-dice-roll-die-test ()
  "Simple test of my random number generator.
This really tests the `rpgtk-dice--test-rolls' function."
  (should (rpgtk-dice--test-rolls 'rpgtk-dice-roll-die '(4) 1 4))
  (should (rpgtk-dice--test-rolls 'rpgtk-dice-roll-die '(6) 1 6))
  (should (rpgtk-dice--test-rolls 'rpgtk-dice-roll-die '(8) 1 8))
  (should (rpgtk-dice--test-rolls 'rpgtk-dice-roll-die '(12) 1 12))
  (should (rpgtk-dice--test-rolls 'rpgtk-dice-roll-die '(20) 1 20))
  (should (rpgtk-dice--test-rolls 'rpgtk-dice-roll-die '(100) 1 100)))

(ert-deftest rpgtk-dice-display-roll-test ()
  (should (equal (rpgtk-dice-format-roll
                  '((1 2 3) (6)))
                 "6 … 「1 2 3」→「6」"))
  (should (equal (rpgtk-dice-format-roll
                  '((1 2 3) (4)) "3d6" 6 4)
                 "4 … 「1 2 3」→「4」 | 3d6")))

(ert-deftest rpgtk-dice-roll-expression-sum-test ()
  (let ((expr-results (rpgtk-dice-roll-expression-sum "3d6+2")))
    (should (>= expr-results 5))
    (should (<= expr-results 20))))

(ert-deftest rpgtk-dice-format-string-test ()
  (let ((results (rpgtk-dice-format-string "There are d4+1 dragons")))
    (should (or
             (equal results "There are 2 dragons")
             (equal results "There are 3 dragons")
             (equal results "There are 4 dragons")
             (equal results "There are 5 dragons")))))

(ert-deftest rpgtk-dice-format-dice-roll-test ()
  (should (equal "「1 2 3 4」"
                 (rpgtk-dice-format-dice-roll '(1 2 3 4))))
 (should (equal "「1」"
                 (rpgtk-dice-format-dice-roll '(1))))
  (should (equal " "
                 (rpgtk-dice-format-dice-roll nil))))

(ert-deftest rpgtk-dice-format-dice-rolls-test ()
  (should (equal "「1 2 3 4」→「5」"
                 (rpgtk-dice-format-dice-rolls '((1 2 3 4) (5)))))
 (should (equal "「1」"
                (rpgtk-dice-format-dice-rolls '((1)))))
  (should (equal ""
                 (rpgtk-dice-format-dice-rolls nil))))

(ert-deftest rpgtk-dice-last-test ()
  (should (= (rpgtk-dice-last '((1 2 3 4) (5)))
             5)))

(ert-deftest rpgtk-dice-format-total-test ()
  (should (eq (rpgtk-dice-format-total 24 '(20) 15 nil 20)
             'rpgtk-critical-success-roll))
  (should (eq (rpgtk-dice-format-total 12 '(8 4))
             'rpgtk-other-roll)))


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

(defun rpgtk-dice--roll-display (which-one)
  "Display WHICH-ONE type of dice roll."
  (interactive (list (completing-read "Which dice roll: "
                                      '("Critical"
                                        "Success"
                                        "Failure"
                                        "Fumble"
                                        "Partial success"
                                        "None of the above"))))
  (cond
    ((equal which-one "Critical")
     (message (rpgtk-dice-format-roll '((20) (20 4) (24)) "d20+4"
                                      15 nil 20 1)))
    ((equal which-one "Success")
     (message (rpgtk-dice-format-roll '((15) (15 3) (18)) "d20+3"
                                         15  nil 20 1)))
    ((equal which-one "Partial success")
     (message (rpgtk-dice-format-roll '((2 5 4) (5)) "BitD: 3"
                                      6 4 6)))
    ((equal which-one "Failure")
     (message (rpgtk-dice-format-roll '((4) (4 3) (7)) "d20+3"
                                      15 nil 20 1)))
    ((equal which-one "Fumble")
     (message (rpgtk-dice-format-roll '((1) (1 5) (6)) "d20+5"
                                      5 nil 20 1)))
    (t (message (rpgtk-dice-format-roll '((6 8 4) (18)) "3d8")))))

(provide 'rpgtk-dice-tests)
;;; rpgtk-dice-tests.el ends here
