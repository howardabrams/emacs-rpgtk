;;; rpgtk-messages-tests.el --- unit tests for rpgtk-messages  -*- lexical-binding: t; -*-
;;
;; © 2023 Howard X. Abrams
;;   Licensed under a Creative Commons Attribution 4.0 International License.
;;   See http://creativecommons.org/licenses/by/4.0/
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams
;; Created: 31 October 2023
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
;; Simple unit tests

;;; Code:

(ert-deftest rpgtk-last-results-empty-test ()
  ;; Clear the results queue:
  (setq rpgtk-last-results nil)
  ;; Override our
  (let (msg clipboard)
    (flet ((message (fstr &rest opts) (setq msg (apply 'format fstr opts)))
           (kill-new (str) (setq clipboard str)))
      ;;
      (rpgtk-last-results)
      (should (equal "No previous results to show." msg))
      (should (null clipboard)))))

(ert-deftest rpgtk-last-results-single-test ()
  ;; Clear the results queue:
  (setq rpgtk-last-results nil)
  (rpgtk-message "Here's %s!" "Johnny")

  (let (msg clipboard)
    ;; Override the standard `message' and `kill-new' functions.
    ;; This is my way of doing mocks:
    (flet ((message (fstr &rest opts) (setq msg (apply 'format fstr opts)))
           (kill-new (str &optional replace) (setq clipboard str)))

      (rpgtk-last-results)
      (should (equal "Here's Johnny!" msg))
      (should (equal "Here's Johnny!" clipboard))

      (rpgtk-last-results-previous)
      (should (equal "0> Here's Johnny!" msg))
      (should (equal "Here's Johnny!" clipboard)))))

(ert-deftest rpgtk-last-results-multiple-test ()
  (progn
    ;; Clear the results queue:
    (setq rpgtk-last-results nil)

    ;; Fill 'er up with rolls:
    (rpgtk-message2 "First roll: 20" 20)
    (rpgtk-message2 "Second roll: 23" 23)
    (rpgtk-message2 "Third roll: 11" 11)

    (let (msg clipboard)
      ;; Override the standard `message' and `kill-new' functions.
      ;; This is my way of doing mocks:
      (flet ((message (fstr &rest opts) (setq msg (apply 'format fstr opts)))
             (kill-new (str &optional replace) (setq clipboard str)))

        (rpgtk-last-results)
        (should (equal "Third roll: 11" msg))
        (should (equal "11" clipboard))

        ;; If the pointer is at the top of the queue, and we ask to
        ;; advance it earlier than the front, we just repeat the top
        ;; of the queue:
        (rpgtk-last-results-next)
        (should (equal "0> Third roll: 11" msg))
        (should (equal "11" clipboard))

        (rpgtk-last-results-previous)
        (should (equal "1> Second roll: 23" msg))
        (should (equal "23" clipboard))

        (rpgtk-last-results-previous)
        (should (equal "2> First roll: 20" msg))
        (should (equal "20" clipboard))

        ;; Make sure that if we ask for a previous roll at the end of
        ;; the queue, we just get a repeat of the same:
        (rpgtk-last-results-previous)
        (should (equal "2> First roll: 20" msg))
        (should (equal "20" clipboard))))))

(provide 'rpgtk-messages-tests)
;;; rpgtk-messages-tests.el ends here
