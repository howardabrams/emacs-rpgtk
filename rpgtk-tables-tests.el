;;; rpgtk-tables-test.el --- Tests for rpgtk-tables  -*- lexical-binding: t; -*-
;;
;; © 2023 Howard X. Abrams
;;   Licensed under a Creative Commons Attribution 4.0 International License.
;;   See http://creativecommons.org/licenses/by/4.0/
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams
;; Created: 14 October 2023
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
;; The functions in this file are unit tests using ERT.

;;; Code:

(add-to-list 'load-path
             (file-name-directory (buffer-file-name)))

(require 'rpgtk-tables)

(ert-deftest rpgtk-tables--line-parse-test ()
  "Why yes, we should test our regular expression."
  (dolist (data '((" - normal list" "normal list" nil)
                  (" + normal list" "normal list" nil)
                  (" * normal list" "normal list" nil)
                  ("normal list" "normal list" nil)
                  (" - list with tag :tag:" "list with tag" "tag")
                  (" - list with tag :tag" "list with tag" "tag")
                  (" - list with tag : tag" "list with tag" "tag")
                  (" | table cell | freq |" "table cell" "freq")))
    (seq-let (line contents tag) data
      (should (string-match rpgtk-tables--line-parse line))
      (should (equal (match-string 1 line) contents))
      (when tag
        (should (equal (match-string 2 line) tag))))))

(ert-deftest rpgtk-tables--choose-string-list ()
  (let ((empty-string "")
        (no-op-string "This is just a phrase.")
        (two-choices  "We can have [this/that]")
        (two-choices1 "We can have this")
        (two-choices2 "We can have that")
        (tri-choices  "We can have [this / that / the other].")
        (tri-choices1 "We can have this.")
        (tri-choices2 "We can have that.")
        (tri-choices3 "We can have the other."))

    (should (string-equal (rpgtk-tables--choose-string-list empty-string)
                          empty-string))
    (should (string-equal (rpgtk-tables--choose-string-list no-op-string)
                          no-op-string))
    (let ((chosen (rpgtk-tables--choose-string-list two-choices)))
      (should (or (string-equal chosen two-choices1)
                  (string-equal chosen two-choices2))))
    (let ((chosen (rpgtk-tables--choose-string-list tri-choices)))
      (should (or (string-equal chosen tri-choices1)
                  (string-equal chosen tri-choices2)
                  (string-equal chosen tri-choices3))))))

(ert-deftest rpgtk-tables-choose-string-from-table-test ()
  (flet ((rpgtk-tables-choose-str (table-name) "wonderful"))
    (should (string-equal
             "The weather suddenly turns wonderful."
             (rpgtk-tables--choose-string-from-table
              "The weather suddenly turns <<weather/summer>>.")))))

(ert-deftest rpgtk-tables-relevel-table-test ()
  ;; Need to make a fake table, so we will just have a single entry in this
  ;; table, with a tag of "often". We'll specify that the weight for this should
  ;; be 4, and we'll store 10 items under that tag:
  (let* ((table (make-hash-table :test 'equal))
         (tag "often")
         (tag-weight-tuple (list 4 tag)))
    (puthash tag (number-sequence 1 10) table)
    (should (equal (list 40 tag)
                   (rpgtk-tables--relevel-table table tag-weight-tuple)))))

(ert-deftest rpgtk-tables--sum--tag-weights-test ()
  (let ((weighted-tags
         '((44 "often") (27 "seldom") (22 "scarcely") (7 "rarely"))))
    (should (= 100 (rpgtk-tables--sum-tag-weights weighted-tags)))))

(ert-deftest rpgtk-tables--find-tag-test ()
  (let ((weighted-tags
         '((44 "often") (27 "seldom") (22 "scarcely") (7 "rarely"))))
    (should (equal "often"    (rpgtk-tables--find-tag 1 weighted-tags)))
    (should (equal "often"    (rpgtk-tables--find-tag 44 weighted-tags)))
    (should (equal "seldom"   (rpgtk-tables--find-tag 45 weighted-tags)))
    (should (equal "seldom"   (rpgtk-tables--find-tag 71 weighted-tags)))
    (should (equal "scarcely" (rpgtk-tables--find-tag 72 weighted-tags)))
    (should (equal "scarcely" (rpgtk-tables--find-tag 93 weighted-tags)))
    (should (equal "rarely"   (rpgtk-tables--find-tag 94 weighted-tags)))
    (should (equal "rarely"   (rpgtk-tables--find-tag 100 weighted-tags)))))

;; Let's attempt to test our code and its theories.
;;
;; The function repeatedly selects items from a table randomly, and
;; returns a hash of the number of times each element was selected ...

(defun rpgtk-tables-validate (&optional table-name iterations)
  "Return results randomly choosing many items from TABLE-NAME.
Calls `rpgtk-tables-choose' a number of ITERATIONS (defaults to 500)."
  (unless iterations (setq iterations 500))
  (unless table-name
    (setq table-name "test-subject")
    (puthash table-name (make-hash-table :test 'equal) rpgtk-tables)
    (setf (gethash "often" (gethash table-name rpgtk-tables))
          '(o1 o2 o3 o4 o5 o6 o7 o8 o9 o0))
    (setf (gethash "seldom" (gethash table-name rpgtk-tables))
          '(s1 s2 s3 s4 s5 s6 s7 s8 s9 s0))
    (setf (gethash "scarcely" (gethash table-name rpgtk-tables))
          '(l1 l2 l3 l4 l5 l6 l7 l8 l9 l0))
    (setf (gethash "rarely" (gethash table-name rpgtk-tables))
          '(r1 r2 r3 r4 r5 r6 r7 r8 r9 r0)))

  (let ((accumulator (make-hash-table :test 'equal)))
    (dotimes (i iterations accumulator)
      (let* ((item (rpgtk-tables-choose table-name))
             (item-name (first (split-string item " :: "))))
        (cl-incf (gethash item-name accumulator 0))))
    accumulator))

(provide 'rpgtk-tables-test)
;;; rpgtk-tables-test.el ends here
