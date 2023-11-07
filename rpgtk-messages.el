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

(defgroup rpgtk nil
  "Customization for the Role Playing Game Toolkit.")

(defvar rpgtk-last-results nil
  "The results from calls to `rpgtk-screen-' functions are stored here.")

(defvar rpgtk-last-results-ptr 0
  "Keeps track of where we are in the message display ring.
Each call to `rpgtk-last-results' resets this to 0.")

(defcustom rpgtk-last-results-size 25
  "Number of back results for RPGTK rolls and messages.
See `rpgtk-last-results' for details."
  :group 'rpgtk
  :type '(number))

(defun rpgtk-message (format-string &rest args)
  "Replace `messasge' function allowing it to be re-displayed.
The FORMAT-STRING is a standard string for the `format' function,
and ARGS are substituted values."
  (let ((message (apply 'format format-string args)))
    (rpgtk-message2 message)))

(defun rpgtk-message2 (display-msg &optional paste-msg)
  "Display DISPLAY-MSG in the mini-buffer.
Place PASTE-MSG on `kill-ring'."
  (push (list display-msg paste-msg)
        rpgtk-last-results)
  (setq rpgtk-last-results-ptr 0)

  ;; If the results list is too big, truncate it with this magic:
  (when (< rpgtk-last-results-size (seq-length rpgtk-last-results))
    (setf (cdr (nthcdr rpgtk-last-results-size rpgtk-last-results)) nil))

  (rpgtk-last-results))

(defun rpgtk-last-results (&optional replace)
  "Display results from the last call to a `rpgtk-message' function.
With PREFIX, display the number on the queue ring.
With REPLACE, replace the last element on `kill-ring'
instead of appending. See `kill-new' for details."
  (interactive)
  (if (= 0 (seq-length rpgtk-last-results))
      (message "No previous results to show.")
    (seq-let (display-msg paste-msg)
        (nth rpgtk-last-results-ptr rpgtk-last-results)
      (unless paste-msg
        (setq paste-msg display-msg))
      (kill-new (format "%s" paste-msg) replace)
      (if replace
          (message "%d> %s" rpgtk-last-results-ptr display-msg)
        (message display-msg)))))

(defun rpgtk-last-results-previous ()
  "Display results from an earlier call to `rpgtk-message'."
  (interactive)
  (when (< rpgtk-last-results-ptr
           (1- (seq-length rpgtk-last-results)))
    (cl-incf rpgtk-last-results-ptr))
  (rpgtk-last-results t))

(defun rpgtk-last-results-next ()
  "Display results from an later call to `rpgtk-message'.
Meant to be used with `rpgtk-last-results-previous'."
  (interactive)
  (when (> rpgtk-last-results-ptr 0)
    (cl-decf rpgtk-last-results-ptr))
  (rpgtk-last-results t))

(if (not (fboundp 'defhydra))
    (defun rpgtk-last-message ()
      "Display the last RPG Toolkit message."
      (interactive)
      (rpgtk-last-results))

  (if (fboundp 'evil-mode)
      (defhydra rpgtk-messages (:color pink :hint nil)
        ("k" rpgtk-last-results-previous "previous message")
        ("j" rpgtk-last-results-next "next message")
        ("p" yank "paste message")
        ("q" nil "quit"))

    (defhydra rpgtk-messages (:color pink :hint nil)
      ("p" rpgtk-last-results-previous "previous message")
      ("n" rpgtk-last-results-next "next message")
      ("y" yank "yank message")
      ("q" nil "quit")))

  (defun rpgtk-last-message ()
    "Display the last RPG Toolkit message.
  Present a hydra allowing user to see previous messages."
    (interactive)
    (rpgtk-last-results)
    (rpgtk-messages/body)))

(provide 'rpgtk-messages)
;;; rpgtk-messages.el ends here
