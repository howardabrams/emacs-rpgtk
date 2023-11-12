;;; rpgtk-org-tests.el --- unit tests for rpgtk-org  -*- lexical-binding: t; -*-
;;
;; © 2023 Howard X. Abrams
;;   Licensed under a Creative Commons Attribution 4.0 International License.
;;   See http://creativecommons.org/licenses/by/4.0/
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams
;; Created: 11 November 2023
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
;; Unit tests for functions in the `rpgtk-org.el' using ERT.

;;; Code:

(require 'rpgtk-org)

(ert-deftest rpgtk-org--property-key-test ()
  (should (eq :FOO-BAR (rpgtk-org--property-key 'FOO 'BAR)))
  (should (eq :FOO-BAR (rpgtk-org--property-key 'foo 'bar)))
  (should (eq :FOO-BAR (rpgtk-org--property-key "foo" "bar")))
  (should (eq :RPG-BAR (rpgtk-org--property-key nil "bar"))))

(ert-deftest rpgtk-org--property-value-test ()
  (should (= 42 (rpgtk-org--property-value "42")))
  (should (equal '("foo" "bar") (rpgtk-org--property-value "foo bar")))
  (should (equal "foobar" (rpgtk-org--property-value "foobar"))))

(ert-deftest rpgtk-org--string-to-list-test ()
  (should (null (rpgtk-org--string-to-list nil)))
  (should (null (rpgtk-org--string-to-list "")))

  ;; Single elements:
  (should (equal '("foo bar") (rpgtk-org--string-to-list "\"foo bar\"")))
  (should (equal '(42) (rpgtk-org--string-to-list "42")))
  (should (equal '(bob) (rpgtk-org--string-to-list ":bob")))
  (should (equal '("bob") (rpgtk-org--string-to-list "bob")))

  ;; Multiple elements:
  (should (equal (list "foo" "bar")
                 (rpgtk-org--string-to-list "foo bar")))
  (should (equal (list "foo" "the second" "bar")
                 (rpgtk-org--string-to-list "foo \"the second\" bar")))
  (should (equal (list "foo" 42)
                 (rpgtk-org--string-to-list "foo 42")))
  (should (equal (list "one" 'two "buckle my shoe" 3 'four)
                 (rpgtk-org--string-to-list
                  "one :two \"buckle my shoe\" 3 :four"))))

(ert-deftest rpgtk-org-read--property-test ()
  (flet ((org-element--get-node-properties
          ()
          (list :RPG-FOOBAR "42"
                :RPG-FOOLIST "one :two \"buckle my shoe\" 3 :four"
                :IGNORED "this is ignored")))
    (should (= 42 (rpgtk-org--read-property :RPG-FOOBAR)))
    (should (equal '("one" two "buckle my shoe" 3 four)
                   (rpgtk-org--read-property :RPG-FOOLIST)))))

(ert-deftest rpgtk-org-read-property-test ()
  (flet ((org-at-heading-p ()  t)
         (org-up-heading () t)
         (org-heading-level () 1)
         (org-element--get-node-properties
          ()
          (list :RPG-FOOBAR "42"
                :RPG-FOOLIST "one :two \"buckle my shoe\" 3 :four"
                :IGNORED "this is ignored")))
    (should (= 42 (rpgtk-org-read-property 'foobar 'rpg)))
    (should (= 42 (rpgtk-org-read-property "foobar" "rpg")))
    (should (= 42 (rpgtk-org-read-property "foobar")))))

(ert-deftest rpgtk-org--property-value-string-test ()
  (should (equal "42" (rpgtk-org--property-value-string 42)))
  (should (equal "\"foobar\"" (rpgtk-org--property-value-string "foobar")))
  (should (equal "'Dwayne \"The Rock\" Johnson'"
                 (rpgtk-org--property-value-string "Dwayne \"The Rock\" Johnson")))
  (should (equal ":foobar" (rpgtk-org--property-value-string 'foobar)))
  (should (equal ":foobar 42 \"just notes\""
                 (rpgtk-org--property-value-string
                  '(foobar 42 "just notes")))))

(ert-deftest rpgtk-org-property-value-identity-test ()
  (let ((v '(strength 19 "very strong")))
    (should (equal v (rpgtk-org--property-value
                      (rpgtk-org--property-value-string v))))))
(provide 'rpgtk-org-tests)
;;; rpgtk-org-tests.el ends here
