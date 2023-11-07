;;; rpgtk-tests.el --- Unit Tests for rpgtk  -*- lexical-binding: t; -*-
;;
;; © 2023 Howard X. Abrams
;;   Licensed under a Creative Commons Attribution 4.0 International License.
;;   See http://creativecommons.org/licenses/by/4.0/
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams
;; Created: 24 October 2023
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
;; Simple tests for the ring-specific functions.

;;; Code:

(require 'rpgtk (expand-file-name "rpgtk.el"
                           (file-name-directory (buffer-file-name))))

(ert-deftest rpgtk-last-results-test ()
  (progn
    (setq rpgtk-last-results (make-ring 10))
    (rpgtk-message "First in, so this is the oldest")
    (rpgtk-message "Something or other")
    (rpgtk-message "Almost the newest")
    (rpgtk-message "Newest"))

  (should (equal "Newest" (rpgtk-last-results)))
  (should (equal "1> Almost the newest" (rpgtk-last-results-previous)))
  (should (equal "2> Something other" (rpgtk-last-results-previous)))
  (should (equal "1> Almost the newest" (rpgtk-last-results-next)))
  (should (equal "0> Almost the newest" (rpgtk-last-results-next)))
  (should (equal "0> Almost the newest" (rpgtk-last-results-next))))

(provide 'rpgtk-tests)
;;; rpgtk-tests.el ends here
