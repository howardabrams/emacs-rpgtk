;;; rpgtk-messages.el --- display message results  -*- lexical-binding: t; -*-
;;
;; © 2023 Howard X. Abrams
;;   Licensed under a Creative Commons Attribution 4.0 International License.
;;   See http://creativecommons.org/licenses/by/4.0/
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams
;; Created: 26 October 2023
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
;; The functions in this file display messages in the mini-buffer.
;; They are used by other files in this project to display their
;; results, allowing the user to re-display (with a ring) past
;; results.
;;
;; The interactive functions include:
;;
;; Program utilizing this system would call, `rpgtk-message' that
;; _acts_ like the standard Emacs `message' function, but the user can
;; call the following interactive functions, with messages displayed
;; with it:
;;
;;  - `rpgtk-last-results' :: re-displays the last message sent to
;;    the `rpgtk-message' function.
;;
;;  - `rpgtk-last-results-previous' :: would be called after first
;;    calling `rpgtk-last-results' to displays the result earlier in
;;    the ring.
;;
;;  - `rpgtk-last-results-next' :: called to re-display a later message.
;;
;;  - `rpgtk-paste-last-message' :: inserts the last message
;;    _displayed_ into the current buffer.
;;
;;  Most functions would actually call `rpgtk-message2' which makes a
;;  distinction between what is shown to the user in the mini-buffer,
;;  and what would be inserted into a buffer with the
;;  `rpgtk-paste-last-message'.
;;
;;; Code:

(require 'ring)

(defgroup rpgtk nil
  "Customization for the Role Playing Game Toolkit.")

(defvar rpgtk-last-results (make-ring 10)
  "The results from calls to `rpgtk-screen-' functions are stored here.")

(defvar rpgtk-last-results-ptr 0
  "Keeps track of where we are in the message display ring.
Each call to `rpgtk-last-results' resets this to 0.")

(defun rpgtk-message (format-string &rest args)
  "Replace `messasge' function allowing it to be re-displayed.
The FORMAT-STRING is a standard string for the `format' function,
and ARGS are substitued values."
  (let ((message (apply 'format format-string args)))
    (ring-insert rpgtk-last-results message)
    (kill-new message)
    (rpgtk-last-results)))

(defun rpgtk-last-results ()
  "Display results from the last call to a `rpgtk-message' function."
  (interactive)
  (setq rpgtk-last-results-ptr 0)
  (message (ring-ref rpgtk-last-results rpgtk-last-results-ptr)))

(defun rpgtk-last-results-previous ()
  "Display results from an earlier call to `rpgtk-message'."
  (interactive)
  (cl-incf rpgtk-last-results-ptr)
  (when (>= rpgtk-last-results-ptr (ring-length rpgtk-last-results))
    (setq rpgtk-last-results-ptr 0))
  (message "%d> %s" rpgtk-last-results-ptr
           (ring-ref rpgtk-last-results rpgtk-last-results-ptr)))

(defun rpgtk-last-results-next ()
  "Display results from an later call to `rpgtk-message'.
Meant to be used with `rpgtk-last-results-previous'."
  (interactive)
  (when (> rpgtk-last-results-ptr 0)
    (cl-decf rpgtk-last-results-ptr))
  (message "%d> %s" rpgtk-last-results-ptr
           (ring-ref rpgtk-last-results rpgtk-last-results-ptr)))

(defun rpgtk-paste-last-message ()
  "Yank, e.g. paste, the last displayed message."
  (interactive)
  (insert (ring-ref rpgtk-last-results rpgtk-last-results-ptr)))

(provide 'rpgtk-messages)
;;; rpgtk-messages.el ends here
