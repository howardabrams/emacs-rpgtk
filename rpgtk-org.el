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
  (if (string-match (rx bos (one-or-more digit) eos) value)
      (string-to-number value)
    (let ((l (rpgtk-org--string-to-list value)))
      (if (= 1 (seq-length l))
          (car l)
        l))))

(defun rpgtk-org--property-value-string (value)
  "Return VALUE as a string suitable for Org properties."
  (let ((convert-value (lambda (v)
                         (cond
                          ((and (stringp v) (string-match-p (rx "\"") v))
                           (format "'%s'" v))
                          ((and (stringp v) (string-match-p (rx space) v))
                           (format "\"%s\"" v))
                          ((stringp v) (format "%s" v))
                          ((numberp v) (number-to-string v))
                          ((keywordp v) (symbol-name :owl))
                          ((symbolp v) (format ":%s" (symbol-name v)))))))
    (if (listp value)
        (string-join (seq-map convert-value value) " ")
      (funcall convert-value value))))

;; --------------------

(defun rpgtk-org-read-property (prop &optional prefix)
  "Return first occurrence of property, PROP in org buffer.
Note that PROP (or PREFIX) can be a string or symbol.
If PREFIX is specified, pre-pend it to PROP, otherwise, pre-pend
value of `rpgtk-org-default-property-prefix'."
  (let ((key (if prefix
                 (rpgtk-org--property-key prefix prop)
               (rpgtk-org--property-key rpgtk-org-default-property-prefix prop)))
        (results nil))
    (save-excursion
      (unless (org-at-heading-p)
        (org-up-heading))

      (loop-until (or results (< (org-heading-level) 1))
        (setq results (rpgtk-org--read-property key))
        (unless results (org-up-heading)))
      results)))

(defun rpgtk-org--read-property (prop-key)
  "Helper function for `rpgtk-org-read-property'.
Read current header properties for PROP-KEY, and return
the value of that property."
  (let ((props (org-element--get-node-properties)))
    (when-let ((value (plist-get props prop-key 'equal)))
      (rpgtk-org--property-value value))))

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
  ;; Convert all values given to strings:
  (let ((key (rpgtk-org--property-key-string prefix property))
        (val (rpgtk-org--property-value-string value)))
    (org-set-property key val)))


(defun rpgtk-org-update-property (property value &optional
                                           prefix error-if-missing)
  "Search org document, updating PROPERTY with VALUE when found.
If not found, throw `rpgtk-missing-property' if ERROR-IF-MISSING
is non-nil. PREFIX, if set, is pre-pended to PROPERTY, and if not
set, `rpgtk-org-default-property-prefix' is pre-pended."
  (let ((key (rpgtk-org--property-key prefix property)))
    (save-excursion
      (unless (org-at-heading-p)
        (org-up-heading))
      (rpgtk-org--update-property key value error-if-missing))))

(defun rpgtk-org--reset-property (prop curr-value adjustment)
  "Called by `rpgtk-org--update-property' to set Org property, PROP.
CURR-VALUE is the current value of the property.

If ADJUSTMENT is nil, we assume we are to delete the PROP
property. If ADJUSTMENT is a function (lambda expression), then
we call the function with CURR-VALUE, and the results are used as
a new value (after being converted to a string with
`rpgtk-org--property-value-string'.

Otherwise, ADJUSTMENT is used as the new value, replacing CURR-VALUE."
  (let ((prop-str (thread-first prop
                                (symbol-name)
                                (substring 1))))
    (cond
     ((null adjustment)
      (org-delete-property prop-str))
     ((functionp adjustment)
      (org-set-property prop-str
                        (rpgtk-org--property-value-string
                         (funcall adjustment curr-value))))
     (t
      (org-set-property prop-str
                        (rpgtk-org--property-value-string
                         adjustment))))))

(defun rpgtk-org--update-property (prop value error-if-missing)
  "Helper function for `rpgtk-org-update-property'.
See that function for PROP, VALUE and ERROR-IF-MISSING
parameters."
  (let ((props (org-element--get-node-properties)))

    ;; if current heading contains our property, set or delete it:
    (if-let ((curr-value (rpgtk-org--property-value
                          (plist-get props prop 'equal))))
        (rpgtk-org--reset-property prop curr-value value)

      ;; otherwise, if we are at the top, we throw an error,
      ;; or set it depending:
      (if (= 1 (org-heading-level))
          (if error-if-missing
              (throw rpgtk-missing-property
                     (format "Property '%s' not found" prop))
            (org-set-property prop value))

        ;; Didn't find the property and have higher headings to
        ;; search? Go up a heading level, and search again:
        (org-up-heading)
        (rpgtk-org--update-property prop value error-if-missing)))))


(defun rpgtk-org-adjust-property (property value-adj &optional prefix)
  "Search ord document, adjusting PROPERTY with VALUE-ADJ when found.
If VALUE-ADJ is a number, it is added to the current property's
value.

If VALUE-ADJ is a function that takes a single argument,
then the property's value is replaced by applying the function to
the current value.

PREFIX, if set, is pre-pended to PROPERTY, and if not set,
`rpgtk-org-default-property-prefix' is pre-pended."
  (let ((key (rpgtk-org--property-key prefix property))
        (value-func (if (numberp value-adj)
                        (lambda (x) (+ x value-adj))
                      ;; We assume that `value-adj' is a function:
                      value-adj)))
    (save-excursion
      (unless (org-at-heading-p)
        (org-up-heading))
      (rpgtk-org--update-property key value-func nil))))

(defun rpgtk-org-delete-property (property &optional prefix)
  "Search org document, deleting PROPERTY if found.
PREFIX, if set, is pre-pended to PROPERTY, and if not set,
`rpgtk-org-default-property-prefix' is pre-pended."
  (let ((key (rpgtk-org--property-key prefix property)))
    (save-excursion
      (unless (org-at-heading-p)
        (org-up-heading))
      (rpgtk-org--update-property key nil nil))))

;; ------------------------------------------------------------
;;  Collections are properties that are treated as a "group".

(defun rpgtk-org-read-collection (prop &optional prefix)
  "Read a list of properties that match PREFIX and PROP."
  (let ((key (if prefix
                 (rpgtk-org--property-key prefix prop)
               (rpgtk-org--property-key rpgtk-org-default-property-prefix prop))))
    (rpgtk-org-read-properties (rx bol (literal (format "%s" key))) nil)))

;; ------------------------------------

(defun rpgtk-org-property-id ()
  "Generate and return a unique, but short ID."
  (substring (sha1 (format-time-string "%y%m%d%H%M%S")) 0 6))

(defun rpgtk-org-add-collection (prop value &optional prefix)
  "Adds a property collection, PROP (with optional PREFIX).
The VALUE can be a string, number or a list.
See `rpgtk-org-set-property' for details."
  (rpgtk-org-set-property (format "%s-%s" prop (rpgtk-org-property-id))
                          value prefix))

(defun rpgtk-org-get-collection (prop &optional prefix)
  "Return a list of all properties that match property, PROP.
The PREFIX, if not given, defaults to 'rpg'."
  (let ((property (rpgtk-org--property-key-string prefix prop)))
    (rpgtk-org-read-properties (rx string-start (literal property)))))

(defun rpgtk-org-get-collection-keys (key prop &optional prefix)
  "Return list of keys, KEY, of properties matching PROP.
The PREFIX, if not given, defaults to 'rpg'. For instance, if the
org document contained the following matching properties:

    :RPG-ITEM-78C4D8: :name bedroll :weight 2
    :RPG-ITEM-52847E: :name dagger :weight 1
    :RPG-ITEM-E74DDA: :name \"component pouch\" :weight 2

Then setting KEY to 'name' (and PROP to 'item'), would return the
list:

    (\"bedroll\" \"dagger\" \"component pouch\")"
  (seq-map (lambda (item) (plist-get item key))
           (rpgtk-org-get-collection prop prefix)))

(defun rpgtk-org-get-collection-names (prop &optional prefix)
  "Return a list of names of properties matching PROP.
The PREFIX, if not given, defaults to 'rpg'. See the doc string
for `rpgtk-org-get-collection-keys' for details."
  (rpgtk-org-get-collection-keys 'name prop prefix))

(defun rpgtk-org-get-collection-item (key value prop &optional prefix)
  "Return first available org property of collection PROP.
Filter the list to one of a symbol, KEY that matches VALUE."
  (let ((props (rpgtk-org-get-collection prop prefix)))
    (first
     (seq-filter (lambda (item)
                   (equal value (plist-get item key)))
                 props))))

(defun rpgtk-org-get-collection-item-by-name (name prop &optional prefix)
  "Return org property matching PROP (with PREFIX defaulting to 'rpg').
Filter to first match with a `plist' of `name' matching NAME.

For instance, if the org document contained the following
matching properties:

    :RPG-ITEM-78C4D8: :name bedroll :weight 2
    :RPG-ITEM-52847E: :name dagger :weight 1
    :RPG-ITEM-E74DDA: :name \"component pouch\" :weight 2

Calling this function with NAME set to \"dagger\" (and PROP to
\"item\"), would return the list:

    '(name \"dagger\" weight 1)"
  (rpgtk-org-get-collection-item 'name name prop prefix))

(defun rpgtk-org-read-properties (property-pattern &optional include-key?)
  "Return all properties matching PROPERTY-PATTERN.
Similar to `rpgtk-org-read-property', but returns a list of all
matching properties. If INCLUDE-KEY? is non-nil, return both
property and value of the property otherwise, this just returns a
list of matching values."
  (save-excursion
    (unless (org-at-heading-p)
      (org-up-heading))

    ;; Search and collect matching properties, and keep doing that as
    ;; long as `org-up-heading' returns non-nil ... in other words,
    ;; it can still go up a level:
    (let (results)
      (loop-do-while (org-up-heading)
        (setq results
              (append (rpgtk-org--read-properties property-pattern include-key?)
                      results)))
      results)))

(defun rpgtk-org--read-properties (prop-pattern &optional include-key?)
  "Recursive helper function for `rpgtk-org-read-properties'.
Where PROP-PATTERN is a regular expression to filter property symbols.
If INCLUDE-KEY? is non-nil, return both property and value of the property
otherwise, this just returns a list of matching values."
  (let ((match? (lambda (pair)
                  (when (string-match prop-pattern
                                      ;; Convert symbol to matching string:
                                      (thread-first pair
                                              (first)
                                              (symbol-name)
                                              (substring 1)))
                    (if include-key?
                        pair
                      (rpgtk-org--property-value (second pair))))))
        (props (seq-split (org-element--get-node-properties) 2)))
    (thread-last props
                 (seq-map match?)
                 (seq-remove 'null))))

(provide 'rpgtk-org)
;;; rpgtk-org.el ends here
