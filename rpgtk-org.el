;;; rpgtk-org.el --- embed PC stats in org files  -*- lexical-binding: t; -*-
;;
;; © 2023 Howard X. Abrams
;;   Licensed under a Creative Commons Attribution 4.0 International License.
;;   See http://creativecommons.org/licenses/by/4.0/
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams
;; Created:  6 November 2023
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
;; Helper functions for parsing property values in an org-formatted
;; file. These functions are specifically _designed_ for storing
;; character statistics, attributes and other meta-data, in a property
;; drawers in an org file.
;;
;;   - `rpgtk-org-create-property' : creates a property value at the
;;     top-level heading
;;
;;   - `rpgtk-org-set-property' : sets a property value at the current
;;     heading level
;;
;;   - `rpgtk-org-read-property' : reads a property searching heading
;;     levels until it finds it
;;
;;   - `rpgtk-org-update-property' : changes property at whatever
;;     property level it is
;;
;;   - `rpgtk-org-adjust-property' : changes property value by a
;;     relative, numeric value
;;
;;   - `rpgtk-org-delete-property' : removes the property at current
;;     level, or in higher levels.
;;
;;; Code:

(require 'cl)
(require 'org)
(require 'loop)
(require 'org-element)

;;As I've mentioned before, the code needs to walk "up" an Org Tree
;;looking for properties. The crux is using the /internal/
;;`org-element-get-node-properties' function, which returns a property
;;list iff the point is on a header.
;;
;;So the general idea is:
;;  - Move to the previous header
;;  - Collect the properties
;;  - Move up to that header's parent
;;  - Collect its properties, etc.
;;  - Stop if the point is at the /top-level/ header
;;
;;Since we need to know if we are at the top-level, we could have a
;;function, =org-heading-level= that returns =1= if we are at the
;;top-level, and =0= if we aren't at any level:

(defun org-heading-level ()
  "Return heading level of the element at the point.
Return 0 if not at a heading, or above first headline."
  (if-let ((level-str (org-element-property :level (org-element-at-point))))
      level-str
    0))

;;Since `org-up-element' behavior has changed, and
;;`outline-up-heading' doesn’t go to the next heading if it is already
;;on a heading, we need to make a little helper:

(defun org-up-heading ()
  "Move the point to next parent heading, unless already at the top-level."
  (interactive)
  (cl-case (org-heading-level)
    (0 (org-previous-visible-heading 1))
    (1 nil)
    (t (outline-up-heading 1))))

;;And a function to move the point to the top-most heading (where we
;;store the character’s details:

(defun org-top-heading ()
  "Move the point to the top-most heading in the org document.
Note that this is based on the current point position."
  (interactive)
  (org-up-heading)
  (while (> (org-heading-level) 1)
    (org-up-heading)))

;; --------------------

(defvar rpgtk-org-default-property-prefix "rpg"
  "A string that will be prefixed to all properties.")

(defun rpgtk-org--property-key (prefix key)
  "Create symbol by combining PREFIX and KEY for org properties.
The key can be a string or symbol. If PREFIX is nil, use value of
`rpgtk-org-default-property-prefix'."
  (let ((pre (or prefix rpgtk-org-default-property-prefix)))
    (thread-last key
                 (format ":%s-%s" pre)
                 (upcase)
                 (intern))))

(defun rpgtk-org--property-key-string (prefix key)
  "Create symbol by combining PREFIX and KEY for org properties.
The key can be a string or symbol. If PREFIX is nil, use value of
`rpgtk-org-default-property-prefix'."
  (let ((pre (or prefix rpgtk-org-default-property-prefix)))
    (thread-last key
                 (format "%s-%s" pre)
                 (upcase))))

(defun rpgtk-org--string-to-list (str)
  "Convert STR to a list of elements, split on space characters.
This recursive function looks for the following tokens:
  * A word of digits is a number
  * A word starting with a : is a symbol
  * Everything else is a string

Note that if you want to have a string with space, surround the
string with single or double quotes."
  (unless (or (null str) (string-empty-p str))
    (let* ((dquoted? (rx bos "\""
                        (group (1+ (not "\"")))
                        "\""
                        (zero-or-more space)))
           (squoted? (rx bos "'"
                        (group (1+ (not "'")))
                        "'"
                        (zero-or-more space)))
           (number? (rx bos
                        (group (1+ digit))
                        (zero-or-more space)))
           (symbol? (rx bos ":"
                        (group (1+ (not space)))
                        (zero-or-more space)))
           (string? (rx bos
                        (group (1+ (not space)))
                        (zero-or-more space)))
           (head   (cond
                    ((or (string-match dquoted? str)
                         (string-match squoted? str))
                     (match-string 1 str))
                    ((string-match number? str)
                     (string-to-number (match-string 1 str)))
                    ((string-match symbol? str)
                     (intern (match-string 1 str)))
                    ((string-match string? str)
                     (match-string 1 str)))))
      (cons head
            (rpgtk-org--string-to-list (substring str (match-end 0)))))))

(defun rpgtk-org--property-value (value)
  "If VALUE, a string, can be a number, return VALUE as number.
If VALUE looks like a list of items, return VALUE as list.
Otherwise, return VALUE as a string."
  (cond
   ((string-match (rx bos (one-or-more digit) eos) value)
    (string-to-number value))
   ((string-match (rx space) value)
    (rpgtk-org--string-to-list value))
   (t value)))

(defun rpgtk-org--property-value-string (value)
  "Return VALUE as a string suitable for Org properties."
  (let ((convert-value (lambda (v)
                         (cond
                          ((and (stringp v) (string-match-p (rx "\"") v))
                           (format "'%s'" v))
                          ((stringp v) (format "\"%s\"" v))
                          ((numberp v) (number-to-string v))
                          ((symbolp v) (format ":%s" (symbol-name v)))))))
    (if (listp value)
        (string-join (seq-map convert-value value) " ")
      (funcall convert-value value))))

;; --------------------

(defun rpgtk-org--read-property (prop-key)
  "Helper function for `rpgtk-org-read-property'.
Read current header properties for PROP-KEY, and return
the value of that property."
  (let ((props (org-element--get-node-properties)))
    (when-let ((value (plist-get props prop-key 'equal)))
      (rpgtk-org--property-value value))))

(defun rpgtk-org-read-property (prop &optional prefix)
  "Return first occurrence of property, PROP in org buffer.
Note that PROP (or PREFIX) can be a string or symbol.
If PREFIX is specified, pre-pend it to PROP, otherwise, pre-pend
value of `rpgtk-org-default-property-prefix'."
  (let ((key (rpgtk-org--property-key prefix prop))
        (results nil))
    (save-excursion
      (unless (org-at-heading-p)
        (org-up-heading))

      (loop-until (or results (< (org-heading-level) 1))
        (setq results (rpgtk-org--read-property key))
        (unless results (org-up-heading)))
      results)))


(defun rpgtk-org-create-property (prop value &optional prefix)
  "Set PROP as a Org Property at the first heading level in document.
The VALUE can be a string, number, or a list of symbols, numbers, and text.
The symbol or string, PREFIX, if not-nil is pre-pended to the PROP key,
and if nil, it defaults to `rpgtk-org-default-property-prefix'."
  (save-excursion
    (org-top-heading)
    (rpgtk-org--set-property prop value prefix)))

(defun rpgtk-org-set-property (prop value &optional prefix)
  "Set PROP as a Org Property at current heading level in document.
The VALUE can be a string, number, or a list of symbols, numbers, and text.
The symbol or string, PREFIX, if not-nil is pre-pended to the PROP key,
and if nil, it defaults to `rpgtk-org-default-property-prefix'."
  (save-excursion
    (unless (org-at-heading-p)
      (org-up-heading))
    (rpgtk-org--set-property prop value prefix)))

(defun rpgtk-org--set-property (property value prefix)
  "Helper function for storing PREFIX + PROPERTY with VALUE in org buffer.
This assumes that point is currently at the header where the property
should be stored."
 (let ((key (rpgtk-org--property-key-string prefix property))
       (val (rpgtk-org--property-value-string value)))
   (org-set-property key val)))

(provide 'rpgtk-org)
;;; rpgtk-org.el ends here
