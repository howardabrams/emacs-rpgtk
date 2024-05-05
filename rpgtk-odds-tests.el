;;; rpgtk-odds-tests.el --- ERT Unit tests for Odds  -*- lexical-binding: t; -*-
;;
;; © 2024 Howard X. Abrams
;;   Licensed under a Creative Commons Attribution 4.0 International License.
;;   See http://creativecommons.org/licenses/by/4.0/
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams
;; Created:  4 May 2024
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

(require 'rpgtk-odds)

  (ert-deftest rpgtk-odds-markers-test ()
    (should (equal (odds-markers 50) '(12.5 33.5 50 66.5 87.5)))
    (should (equal (odds-markers 30) '( 7.5 20.1 30 53.1 82.5)))
    (should (equal (odds-markers 70) '(17.5 46.9 70 79.9 92.5))))

(ert-deftest rpgtk-odds-likelihood-offset-test ()
    ;; 50/50 odds with no chaos should keep to 50%:
    (should (= (rpgtk-odds-likelihood-offset 50  0) 50))

    ;; 50/50 odds with 2 levels of chaotic energy making it more likely
    ;; for something to be true, should bump us up around 75%:
    (should (= (rpgtk-odds-likelihood-offset 50 +2) 70))

    ;; And 50/50 odds with 2 levels of stabilty should do the opposite:
    (should (= (rpgtk-odds-likelihood-offset 50 -2) 30))

    ;; An unlikely question (around 37%) with 1 level of chaos, should
    ;; bump us close to the 50/50 realm:
    (should (= (rpgtk-odds-likelihood-offset 37 +1) 49))

    ;; But an unlikely question with 2 levels of chaos, should put the
    ;; question _likely_ (around 62%):
    (should (= (rpgtk-odds-likelihood-offset 37 +2) 61))

    ;; An absolutely sure thing (around 87-90% chance) with one level of
    ;; stability, should bring it down to just likely (around 62%):
    (should (= (rpgtk-odds-likelihood-offset 87 -1) 70))

    ;; But an absolutely sure things with more chaos, shouldn't affect
    ;; the outcome much:
    (should (= (rpgtk-odds-likelihood-offset 87 +1) 89))

    (should (= (rpgtk-odds-likelihood-offset 50 +5) 89)))


;;; Code:



(provide 'rpgtk-odds-tests)
;;; rpgtk-odds-tests.el ends here
