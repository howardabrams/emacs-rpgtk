;;; RPGTK-DICE -- Yet Another Dice Simulator
;;
;; Author: Howard Abrams <howard@howardabrams.com>
;; Copyright © 2018, Howard Abrams, all rights reserved.
;; Created: 15 August 2018
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;   Functions to simulate rolling dice in RPG games.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defvar rpgtk-previous-roll-expression nil
  "Whenever we roll a dice from an expression, we remember it
here, so that we can re-roll it again.")

(defface rpgtk-critical-success-roll
  '((t :foreground "green yellow"
       :weight ultra-bold))
  "Face for critical success dice rolls."
  :group 'rpgtk)

(defface rpgtk-successful-roll
  '((t :foreground "chartreuse"
       :weight bold))
  "Face for successful dice rolls."
  :group 'rpgtk)

(defface rpgtk-failed-roll
  '((t :foreground "tomato"
       :weight bold))
  "Face for failed dice rolls."
  :group 'rpgtk)

(defface rpgtk-critical-failure-roll
  '((t :foreground "magenta"
       :weight extra-bold))
  "Face for critical failures and fumbled dice rolls."
  :group 'rpgtk)

(defface rpgtk-middlin-roll
  '((t :foreground "yellow"
       :weight bold))
  "Face for partial success or weak hit dice rolls."
  :group 'rpgtk)

(defface rpgtk-other-roll
  '((t :foreground "gold"
       :weight bold))
  "Face for dice rolls where we don't know if it was successful."
  :group 'rpgtk)

(defface rpgtk-roll-expression
  '((t :foreground "khaki"))
  "Face for dice expression (if given)."
  :group 'rpgtk)

(defface rpgtk-dimmed-display
  '((t :foreground "gray"))
  "Face for unimportant parts of a displayed message."
  :group 'rpgtk)

(defface rpgtk-bright-display
  '((t :foreground "white"
       :weight bold))
  "Face for important parts of a message."
  :group 'rpgtk)

;; The basics of a dice roll is a random number from a given range.
;; Note that if we give a 6-sided die to the random number, we will
;; end up with a range of 0 to 5, so we need to increase this value by
;; 1.

(defun rpgtk-roll-die (sides)
  "Rolls a die of with SIDES."
  (interactive "nDice Type (sides): ")
  (1+ (random sides)))

;; Now that we have a `rpgtk--roll-die' function that rolls a single
;; die. How do we want multiple dice rolls. Perhaps the results should
;; be a list, so that we can easily sum them, but still have the
;; original results of each die roll.

(defun rpgtk-roll-dice (count sides)
  "Return a list of COUNT dice rolls where each die has SIDES."
  (interactive "nNumber of Dice to Roll: \nnDice Type (sides): ")
  (let (value)
    (dotimes (_ count value)
      (setq value (cons (rpgtk-roll-die sides) value)))))

;; An RPG has checks that have multiple dice, plus a modifier
;; (positive or negative). When displaying the results, I want all the
;; dice rolls displayed differently from the modifier. Should we just
;; assume the modifier is the first or last number is the returned
;; list?
;;
;; What if we have this function return a cons'd with the `car' be a
;; list of the rolls, and the `cdr' the modifier amount?

(defun rpgtk--roll (num-dice dice-type &optional modifier plus-minus)
  "Generate a random dice roll.
Return tuple where `car' is a list of rolled
results, and the `cdr' is the modifier, see `rpgtk--sum'.

The NUM-DICE is the number of DICE-TYPE to roll. The PLUS-MINUS
is a string of either '+' or '-' to affect the results with the
MODIFIER amount. If PLUS-MINUS is nil, assume MODIFIER should
be added."
  (let* ((base-rolls (rpgtk-roll-dice num-dice dice-type)))
    (cond ((string= "-" plus-minus) (cons base-rolls (- modifier)))
          ((numberp modifier)       (cons base-rolls modifier))
          (t                        (cons base-rolls 0)))))

(defun rpgtk--roll-with-choice (num-dice dice-type choose-fn
                                         &optional modifier plus-minus)
  "Return dice roll where results are filtered by CHOOSE-FN.
Like `rpgtk--roll', NUM-DICE are the number of dice to roll of
sides of DICE-TYPE, e.g. 6 for normal cubed dice.

The PLUS-MINUS is a string of either '+' or '-' to affect the
results with the MODIFIER amount. If PLUS-MINUS is nil, assume
MODIFIER should be added.

For instance, to keep the highest of 3 of rolling 4 six-sided
the CHOOSE-FN can be a lambda like:

    (lambda (rolls)
      (seq-take (seq-sort #'> rolls) 3))

Return tuple where `car' is a list of rolled results, and the
`cdr' is the modifier."
  (let* ((rolls (rpgtk--roll num-dice dice-type modifier plus-minus)))
    (cons
     (funcall choose-fn (car rolls))
     (cdr rolls))))


(defun rpgtk-roll-highest (num-dice dice-type
                                    &optional num-keep modifier plus-minus)
  "Return dice roll where on NUM-KEEP highest rolls are returned.
Like `rpgtk--roll', NUM-DICE are the number of dice to roll of sides
of DICE-TYPE, e.g. 6 for normal cubed dice.

The PLUS-MINUS is a string of either '+' or '-' to affect the
results with the MODIFIER amount. If PLUS-MINUS is nil, assume
MODIFIER should be added.

Return tuple where `car' is a list of rolled results, and the
`cdr' is the modifier."
  (let* ((highest (lambda (rolls)
                    (seq-take (seq-sort #'> rolls) num-keep))))
    (rpgtk--roll-with-choice 4 6 highest)))

(defun rpgtk-roll-lowest (num-dice dice-type
                                   &optional num-keep modifier plus-minus)
  "Return dice roll where on NUM-KEEP lowest rolls are returned.
Like `rpgtk--roll', NUM-DICE are the number of dice to roll of sides
of DICE-TYPE, e.g. 6 for normal cubed dice.

The PLUS-MINUS is a string of either '+' or '-' to affect the
results with the MODIFIER amount. If PLUS-MINUS is nil, assume
MODIFIER should be added.

Return tuple where `car' is a list of rolled results, and the
`cdr' is the modifier."
  (let* ((lowest (lambda (rolls)
                   (seq-take (seq-sort #'< rolls) num-keep))))
    (rpgtk--roll-with-choice 4 6 lowest)))


(defun rpgtk--sum (roll-combo)
  "Return a summation of the dice rolls in ROLL-COMBO tuple.
The tuple is a `cons' structure where the `car' is a list of rolls,
and the `cdr' is a modifier, e.g. ((5 3 2 1 6) . 3)."
  (let ((rolls    (car roll-combo))
        (modifier (cdr roll-combo)))
    (seq-reduce #'+ rolls modifier)))

;; ------------------------------------------------------------
;; DICE EXPRESSION- Strings like 2d6+1 could be split into integer
;; values, and passed to functions defined above, as the first digit
;; is the number of dice to roll, and the digit following the "d" is
;; the number of sides.
;; ------------------------------------------------------------

(defvar rpgtk-roll-regexp
  (rx word-start
      (optional (group (one-or-more digit)))
      "d"
      (group (one-or-more digit))
      (optional
       (group (or "+" "-"))
       (group (one-or-more digit)))
      (optional ":"
                (group (one-or-more digit)))
      word-end)
  "A regular expression that matches a dice roll.")

(defun rpgtk-forward-roll (count)
  "Move the point to the next COUNT of a dice roll expression.

Note: This moves the point to the _beginning_ of what is
considered the dice roll description, which could include any of
the following:

  - d8
  - 2d6
  - 1d12+5
  - d20-4"
  (interactive "p")
  (when (looking-at-p rpgtk-roll-regexp)
    (re-search-forward rpgtk-roll-regexp))
  (dotimes (repeat count)
    (re-search-forward rpgtk-roll-regexp))
  (goto-char (match-beginning 0)))

(defun rpgtk--expression-parts (expression)
  "Given a dice EXPRESSION, e.g. 2d6+3, return a list of the numbers.
For instance, 3d6+2 would return (3 6 2) and 2d10-1 would return
the list (2 10 -1)."
  (when (string-match rpgtk-roll-regexp expression)
    (let* ((num-dice-s  (or (match-string 1 expression) "1"))
           (num-dice    (string-to-number num-dice-s))
           (dice-type-s (or (match-string 2 expression) "20"))
           (dice-type   (string-to-number dice-type-s))
           (plus-minus  (or (match-string 3 expression) "+"))
           (modifier-s  (or (match-string 4 expression) "0"))
           (modifier    (string-to-number modifier-s)))
      (list num-dice dice-type
            (if (equal plus-minus "-")
                (- 0 modifier)
              modifier)))))

(defun rpgtk-roll-dice-expression (expression)
  "Return dice roll of EXPRESSION as a string, e.g. 2d6+3."
  (seq-let (num-dice dice-type modifier)
      (rpgtk--expression-parts expression)
    (rpgtk--roll num-dice dice-type modifier)))

;; For programmatic reasons, we need a quick way to roll dice and get a
;; numeric value.

(defun rpgtk-roll-em (num-or-expr &optional dice-type modifier plus-minus)
  "Return results of rolling some dice values as a list.

The first element is the sum total of rolling the dice. The
second is a list of the dice rolls, and the third is an integer
value of the modifier included in the total.

NUM-OR-EXPR can either be a dice expression as a string, e.g.
2d4+2 Or NUM-OR-EXPR is the number of dice to roll of type,
DICE-TYPE, and a MODIFIER is included in the sum. PLUS-MINUS can
be a string '+' or '-'.

 - A roll-combo tuple list
 - A single number of dice to roll (but this requires more values)
a cons-cell, where car is a sum total of a dice roll."
  (let* ((rolls (if (stringp num-or-expr)
                    (rpgtk-roll-dice-expression num-or-expr)
                  (rpgtk--roll num-or-expr dice-type modifier plus-minus)))
         (total (rpgtk--sum rolls)))
    (list total (car rolls) (cdr rolls))))



(defun rpgtk-roll-sum (num-or-expr &optional dice-type modifier)
  "Return only sum value from rolling some dice.
The NUM-OR-EXPR can be one of the following values:
 - A dice expression as a string, e.g. 2d4+2
 - A single number of dice to roll (but this requires more values)

If NUM-OR-EXPR is an integer, then DICE-TYPE is the number of dice sides.
MODIFIER, if given, is added to roll."
  (car (rpgtk-roll-em num-or-expr dice-type modifier)))

(defun rpgtk-dice-format-string (str)
  "Replace all dice expressions in STR with a dice roll results."
  (replace-regexp-in-string rpgtk-roll-regexp
                            (lambda (s) (number-to-string
                                    (rpgtk-roll-sum s)))
                            str))

(defun rpgtk--display-roll-parts (answer die-rolls modifier
                                         &optional expression
                                         success middlin
                                         critical fumble)
  "Render parameters into a suitable string.
The ANSWER is probably the sum expression of DIE-ROLLS (a list of
integers), rendered brightly. MODIFIER is positive or negative
number. The EXPRESSION is a string that may have generated the
roll combo.

If SUCCESS is given, ANSWER is displayed with the face,
`rgptk-successful-roll' if ANSWER is greater than or equal to
this value, or `rpgtk-failed-roll' if less than.

If MIDDLIN is given, ANSWER is displayed with the face,
`rgptk-middlin-roll' if ANSWER is greater than or equal to
this value (e.g. a partial success or weak hit).

If CRITICAL is given, ANSWER is displayed with the face,
`rgptk-critical-success-roll' if any DIE-ROLLS match this value.

If FUMBLE is given, ANSWER is displayed with the face,
`rgptk-critical-failure-roll' if any DIE-ROLLS match this value.

Otherwise, ANSWER is displayed with the face, `rgptk-other-roll'."
  (let* ((sum-prop (cond
                    ((and critical (seq-contains-p die-rolls critical))
                     'rpgtk-critical-success-roll)
                    ((and fumble (seq-contains-p die-rolls fumble))
                     'rpgtk-critical-failure-roll)
                    ((and success (>= answer success))
                     'rpgtk-successful-roll)
                    ((and middlin (>= answer middlin))
                     'rpgtk-middlin-roll)
                    (success 'rpgtk-failed-roll)
                    (t 'rpgtk-other-roll)))
         (sum-str (propertize (number-to-string answer)
                              'face sum-prop))
         (die-str (cond ((and (= (length die-rolls) 1)
                              (= modifier 0))
                         "")
                        ((= (length die-rolls) 1)
                         (format " ... %d" (car die-rolls)))
                        (t
                         (format " ... %s" die-rolls))))
         (mod-str (cond ((> modifier 0)  (format " +%d" modifier))
                        ((< modifier 0)  (format " %d" modifier))
                        (t               "")))
         (exp-str (if expression
                      (format " | %s"
                              (propertize expression
                                          'face 'rpgtk-roll-expression))
                    "")))
    (format "%s%s%s%s" sum-str die-str mod-str exp-str)))

(defun rpgtk-roll (expression)
  "Generate a random number based on a given dice roll EXPRESSION.
Unless the point is on a dice roll description, e.g 2d12+3."
  (interactive (list (if (looking-at rpgtk-roll-regexp)
                         (match-string-no-properties 0)
                       (read-string "Dice Expression: "))))
  (setq rpgtk-previous-roll-expression expression)
  (seq-let (answer die-rolls modifier) (rpgtk-roll-em expression)
    (message "Rolled: %s"
             (rpgtk--display-roll-parts answer die-rolls
                                        modifier expression))))

(defun rpgtk-roll-again ()
  "Roll the previous expression ... again.
Never rolled before? No problem, we'll query for the expression
if we need too."
  (interactive)
  (if rpgtk-previous-roll-expression
      (rpgtk-roll rpgtk-previous-roll-expression)
    (call-interactively 'rpgtk-roll)))

(provide 'rpgtk-dice)
;;; rpgtk-dice.el ends here
