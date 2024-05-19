;;; rpgtk-dice.el -- Yet another dice simulator -*- lexical-binding: t; -*-
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

(require 'subr-x)
(require 'rpgtk-messages)

(defvar rpgtk-dice-previous-roll-expression nil
  "Store last dice expression, aa a string.
This way, we can re-roll it again.")

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

(defface rpgtk-display-dice-sequence
  '((t :foreground "dark gray"))
  "Face for unimportant parts of a displayed message."
  :group 'rpgtk)

(defface rpgtk-display-dice-sequence-separator
  '((t :foreground "dim gray"))
  "Face for unimportant parts of a displayed message."
  :group 'rpgtk)

(defface rpgtk-display-dice-sequence-border
  '((t :foreground "gray"))
  "Face for unimportant parts of a displayed message."
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

;; -------------------------------------------------------------------
;;  DICE ROLLING BASICS
;; -------------------------------------------------------------------

;; The basics of a dice roll is a random number from a given range.
;; Note that if we give a 6-sided die to the random number, we will
;; end up with a range of 0 to 5, so we need to increase this value by
;; 1.

(defun rpgtk-dice-roll-die (sides)
  "Rolls a die of with SIDES."
  (interactive "nDice Type (sides): ")
  (1+ (random sides)))

;; Now that we have a `rpgtk--roll-die' function that rolls a single
;; die. How do we want multiple dice rolls. Perhaps the results should
;; be a list, so that we can easily sum them, but still have the
;; original results of each die roll.

(defun rpgtk-dice-roll (count sides)
  "Return a list of COUNT dice rolls where each die has SIDES."
  (interactive "nNumber of Dice to Roll: \nnDice Type (sides): ")
  (let (value)
    (dotimes (_ count value)
      (setq value (cons (rpgtk-dice-roll-die sides) value)))))

;; -------------------------------------------------------------------
;; DICE MODIFICATIONS FUNCTIONS
;;
;; With a pool of dice, we want to have functions that emulate various
;; RPG games, for instance D&D takes a dice pool and then adds one or
;; more modifiers, where Blades in the Dark, rolls a pool of d6's and
;; takes the highest value.
;;
;; To make this easy, we create a mini-DSL
;; -------------------------------------------------------------------

(defun rpgtk-dice--split-roll-mods (modifiers)
  "Return three-element list of MODIFIERS.
For instance, if MODIFIERS is the list consisting of:
   (:foo 3 4 :bar 7 :baz)

Would return:  (:foo (3 4) (:bar 7 :baz)

Where the first element is a keyword, the second element is a
list of all non-keyword elements, and the third is a list of the
rest, starting with a keyword."
  (let* ((nonkey (lambda (k) (not (keywordp k)))))
    (list
     (car modifiers)
     (seq-take-while nonkey (cdr modifiers))
     (seq-drop-while nonkey (cdr modifiers)))))

(defun rpgtk-dice--roll-mod (dice-pool modifiers)
  "Return a list of DICE-POOL and a list dice sequences.
Each new dice sequence is modified based on MODIFIERS.
See `rpgtk-dice-roll-mod' for DSL details implemented here."
  (seq-let (modifier parameters rest)
      (rpgtk-dice--split-roll-mods modifiers)
    (cons dice-pool
          (when modifier
            (let ((modded-dice-pool
                   (cl-case modifier
                     (:add    (append dice-pool parameters))
                     (:sum    (list (apply '+ dice-pool)))
                     (:count  (list (length dice-pool)))
                     (:max    (list (apply 'max dice-pool)))
                     (:min    (list (apply 'min dice-pool)))
                     (:top    (seq-take (seq-sort '> dice-pool)
                                        (or (car parameters) 1)))
                     (:bottom (seq-take (seq-sort '< dice-pool)
                                        (or (car parameters) 1)))
                     (:filter (seq-filter (car parameters) dice-pool))
                     (:map    (seq-map (car parameters) dice-pool))
                     (t (error (format "%s not keyword" modifier))))))
              (rpgtk-dice--roll-mod modded-dice-pool rest))))))

(defun rpgtk-dice-roll-mod (dice-pool &rest modifiers)
  "Return a sequence of modifications of DICE-POOL.
Use this to simulate special RPG-specific dice-roll
interpretations.

Where DICE-POOL is a list of numbers generated by rolling a
number of dice, and MODIFIERS is a sequence of modifications
to perform on DICE-POOL.

For instance, to simulate a D&D style, like 2d8+4, we would:

    (rpgtk-dice-roll-mod (rpgtk-dice-roll 2 8)
                         :add 4
                         :sum)

Could return: ((1 7) (1 7 4) (12))

A Blades in the Dark game rolls a pool of d6, and takes the highest:

    (rpgtk-dice-roll-mod (rpgtk-dice-roll num-dice 6)
                         :max)

Which could return, if num-dice is 4:  ((1 3 5 2) (5))

Modifiers include:

    * :add    appends one or more numbers to the dice pool
    * :sum    adds all numbers in dice pool
    * :count  return number of dice in pool
    * :max    returns the highest number is pool
    * :min    returns the lowest number
    * :top    returns the highest number(s) based on parameter
              given, and if no number given, defaults to 1
    * :top    returns the lowest number(s) based on parameter
              given, and if no number given, defaults to 1
    * :filter only keeps numbers that pass a predicate lambda
    * :map    changes dice pool based on a given lamda expression"
  (rpgtk-dice--roll-mod dice-pool modifiers))

(defun rpgtk-dice-last (dice-sequence)
  "Return the last digit of DICE-SEQUENCE.
Where DICE-SEQUENCE is a list of numeric lists, e.g.
`((1 8 3 4) (8 4) (12))' would return 12."
  (thread-first dice-sequence
                (flatten-list)
                (last)
                (car)))

;; ------------------------------------------------------------
;; DICE EXPRESSION- Strings like 2d6+1 could be split into integer
;; values, and passed to functions defined above, as the first digit
;; is the number of dice to roll, and the digit following the "d" is
;; the number of sides.
;;
;; Yeah, this is primary a style we associate with D&D, but is quite
;; common in many RPGs.
;; ------------------------------------------------------------

(defvar rpgtk-dice-roll-regexp
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

(defun rpgtk-dice-forward-roll (count)
  "Move the point to the next COUNT of a dice roll expression.

Note: This moves the point to the _beginning_ of what is
considered the dice roll description, which could include any of
the following:

  - d8
  - 2d6
  - 1d12+5
  - d20-4"
  (interactive "p")
  (when (looking-at-p rpgtk-dice-roll-regexp)
    (re-search-forward rpgtk-dice-roll-regexp))
  (dotimes (repeat count)
    (re-search-forward rpgtk-dice-roll-regexp))
  (goto-char (match-beginning 0)))

(defun rpgtk-dice--expression-parts (expression)
  "Given a dice EXPRESSION, e.g. 2d6+3, return a list of the numbers.
For instance, 3d6+2 would return (3 6 2) and 2d10-1 would return
the list (2 10 -1)."
  (when (string-match rpgtk-dice-roll-regexp expression)
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

(defun rpgtk-dice-roll-expression (dice-expression)
  "Return dice roll of DICE-EXPRESSION as a string, e.g. 2d6+3."
  (seq-let (num-dice dice-type modifier)
           (rpgtk-dice--expression-parts dice-expression)
    (if (= modifier 0)
        (rpgtk-dice-roll-mod (rpgtk-dice-roll num-dice dice-type)
                             :sum)
      (rpgtk-dice-roll-mod (rpgtk-dice-roll num-dice dice-type)
                           :sum
                           :add modifier
                           :sum))))

(defun rpgtk-dice-roll-expression-sum (dice-expression)
  "Return sum value from rolling some dice, via DICE-EXPRESSION.
For instance, if given a string, e.g. 2d4+2, ruturn a single
integer value from rolling 2 four-sided die and adding 2."
  (rpgtk-dice-last
   (rpgtk-dice-roll-expression dice-expression)))

(defun rpgtk-dice-format-string (str)
  "Replace all dice expressions in STR with a dice roll results."
  (replace-regexp-in-string rpgtk-dice-roll-regexp
                            (lambda (s) (number-to-string
                                    (rpgtk-dice-roll-expression-sum s)))
                            str))

;; ----------------------------------------------------------------------
;;  DICE DISPLAY
;;
;;  These functions colorize the display of dice rolls and expressions.
;;  The main function is `rpgtk-dice-format-roll'.
;; ----------------------------------------------------------------------

(defun rpgtk-dice-format-dice-roll (roll)
  "Convert ROLL from a list of dice rolled integers to a string."
  (if roll
      (let ((roll-of-strs (seq-map (lambda (e) (format "%s" e)) roll)))
        (concat (propertize "「" 'face 'rpgtk-display-dice-sequence-border)
                (propertize (string-join roll-of-strs " ")
                            'face 'rpgtk-display-dice-sequence)
                (propertize "」" 'face 'rpgtk-display-dice-sequence-border)))
    " "))

(defun rpgtk-dice-format-dice-rolls (rolls)
  "Convert ROLLS from a list of lists of integers to a string.
Given:   ((3 4 1 2) (4 2) (6) (-3))
Return: 「3 4 1 2」→「4 2」→「6」→「-3」

Perhaps this could be called with `propertize'."
  (let ((s-rolls (seq-map 'rpgtk-dice-format-dice-roll rolls)))
    (string-join s-rolls
                 (propertize "→" 'face
                             'rpgtk-display-dice-sequence-separator))))

(defun rpgtk-dice-format-total (total dice-rolls
                                      &optional success middlin
                                      critical fumble)
  "Return a property symbols based on dice roll logic.
See `rpgtk-dice-format-roll' for details.
TOTAL is a number, and DICE-ROLLS is a list of numbers."
  (cond
   ((and critical (seq-contains-p dice-rolls critical))
    'rpgtk-critical-success-roll)
   ((and fumble (seq-contains-p dice-rolls fumble))
    'rpgtk-critical-failure-roll)
   ((and success (>= total success))
    'rpgtk-successful-roll)
   ((and middlin (>= total middlin))
    'rpgtk-middlin-roll)
   (success 'rpgtk-failed-roll)
   (t 'rpgtk-other-roll)))

(defun rpgtk-dice-format-roll-uniq (dice-rolls)
  "Return DICE-ROLLS without repeating sequences."
  ;; shame I can't use `seq-uniq', but we should only remove
  ;; duplicates that are next to each other.
  (let (acc)
    (dolist (roll dice-rolls acc)
      (unless (equal (car (last acc)) roll)
        (setq acc (append acc (list roll)))))))

(defun rpgtk-dice-format-roll (dice-rolls
                               &optional expression
                               success middlin
                               critical fumble)
  "Render parameters into a suitable string.
The total is probably the last element of DICE-ROLLS (a list
of list of integers ... of rolled dice), rendered brightly. The
EXPRESSION is a string that may have generated the roll combo.

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
  (let* ((total (rpgtk-dice-last dice-rolls))
         (total-prop (rpgtk-dice-format-total total (car dice-rolls)
                                              success middlin
                                              critical fumble))
         (total-str (propertize (number-to-string total)
                                'face total-prop))
         (total-sep (propertize " … " 'face 'rpgtk-dimmed-display))
         (rolls-str (thread-last dice-rolls
                                 rpgtk-dice-format-roll-uniq
                                 rpgtk-dice-format-dice-rolls))
         (exp-str (if expression
                      (format " | %s"
                              (propertize expression
                                          'face 'rpgtk-roll-expression))
                    "")))
    (format "%s%s%s%s" total-str total-sep rolls-str exp-str)))

(defun rpgtk-roll (expression)
  "Generate a random number based on a given dice roll EXPRESSION.
Unless the point is on a dice roll description, e.g 2d12+3."
  (interactive (list (if (looking-at rpgtk-dice-roll-regexp)
                         (match-string-no-properties 0)
                       (read-string "Dice Expression: "))))
  (setq rpgtk-dice-previous-roll-expression expression)
  (let* ((rolls (rpgtk-dice-roll-expression expression)))
    (message "Rolled: %s"
             (rpgtk-dice-format-roll rolls expression))))

(defun rpgtk-roll-again ()
  "Roll the previous expression ... again.
Never rolled before? No problem, we'll query for the expression
if we need too."
  (interactive)
  (if rpgtk-dice-previous-roll-expression
      (rpgtk-roll rpgtk-dice-previous-roll-expression)
    (call-interactively 'rpgtk-roll)))


;; ------------------------------------------------------------------
;;  SOME GAME-SPECIFIC DICE ROUTINES
;; -------------------------------------------------------------------

(defun rpgtk-dice-roll-poly (sides)
  "Return value from rolling a polyhedral die of SIDES."
  (interactive "nNumber of Polyhedral Die: ")
  (thread-first (rpgtk-dice-roll 1 sides)
                (rpgtk-dice-roll-mod)
                (rpgtk-dice-format-roll (format "d%d" sides))
                (rpgtk-message)))

(defun rpgtk-dice-roll-d3 ()
  "Return results from rolling a 3-sided die."
  (interactive)
  (rpgtk-dice-roll-poly 3))

(defun rpgtk-dice-roll-d4 ()
  "Return results from rolling a 4-sided die."
  (interactive)
  (rpgtk-dice-roll-poly 4))

(defun rpgtk-dice-roll-d6 ()
  "Return results from rolling a 6-sided die."
  (interactive)
  (rpgtk-dice-roll-poly 6))

(defun rpgtk-dice-roll-d8 ()
  "Return results from rolling a 8-sided die."
  (interactive)
  (rpgtk-dice-roll-poly 8))

(defun rpgtk-dice-roll-d10 ()
  "Return results from rolling a 10-sided die."
  (interactive)
  (rpgtk-dice-roll-poly 10))

(defun rpgtk-dice-roll-d12 ()
  "Return results from rolling a 12-sided die."
  (interactive)
  (rpgtk-dice-roll-poly 12))

(defun rpgtk-dice-roll-d20 ()
  "Return results from rolling a 20-sided die."
  (interactive)
  (thread-first (rpgtk-dice-roll 1 20)
                (rpgtk-dice-roll-mod)
                (rpgtk-dice-format-roll "d20" nil nil 20 1)
                (rpgtk-message)))

(defun rpgtk-dice-roll-d100 ()
  "Return results from rolling a 100-sided die."
  (interactive)
  (rpgtk-dice-roll-poly 100))

(defun rpgtk-dice-roll-2d6 ()
  "Return results from rolling two 6-sided dice."
  (interactive)
  (thread-first (rpgtk-dice-roll 2 6)
                (rpgtk-dice-roll-mod :sum)
                (rpgtk-dice-format-roll "2d6")
                (rpgtk-message)))

(defun rpgtk-dice-roll-3d6 ()
  "Return results from rolling three 6-sided dice."
  (interactive)
  (thread-first (rpgtk-dice-roll 3 6)
                (rpgtk-dice-roll-mod :sum)
                (rpgtk-dice-format-roll "3d6")
                (rpgtk-message)))

(defun rpgtk-dice-roll-dnd (num-dice dice-type modifier)
  "Return a dice sequence that simulates a D&D-style roll.
Given a dice expression of 3d8+2, this function would be called
where NUM-DICE is the number of dice, e.g. 3, and
DICE-TYPE is the size of number of sides of dice, e.g. 8,
and MODIFIER is the value to add/subtract to the sum, e.g. 2."
  (rpgtk-dice-roll-mod (rpgtk-dice-roll num-dice dice-type)
                       :sum
                       :add modifier
                       :sum))

(defun rpgtk-dice-roll-dnd-advantage (&optional modifier)
  "Return a dice sequence of two d20s, and the highest is kept.
Note that MODIFIER is added to the results."
  (interactive "nRoll d20 modifier: ")
  (thread-first (rpgtk-dice-roll 2 20)
                (rpgtk-dice-roll-mod
                 :max
                 :add (or modifier 0)
                 :sum)
                (rpgtk-dice-format-roll "d20/a")
                (rpgtk-message)))

(defun rpgtk-dice-roll-dnd-disadvantage (&optional modifier)
  "Return a dice sequence of two d20s, and the lowest is kept.
Note that MODIFIER is added to the results."
  (interactive "nRoll d20 modifier: ")
  (thread-first (rpgtk-dice-roll 2 20)
                (rpgtk-dice-roll-mod
                 :min
                 :add (or modifier 0)
                 :sum)
                (rpgtk-dice-format-roll "d20/d")
                (rpgtk-message)))

(defun rpgtk-dice-roll-bitd (num-dice)
  "Displays a formatted dice expression for Blades in the Dark.
Where NUM-DICE are the number six-sided dice to roll."
  (interactive "nNumber of Dice: ")
  (thread-first num-dice
                (rpgtk-dice-roll 6)
                (rpgtk-dice-roll-mod :max)
                (rpgtk-dice-format-roll "BitD" 6 4)
                (rpgtk-message)))

(defun rpgtk-dice-roll-fate ()
  "Displays a formatted dice expression for four Fate Dice.
Each die ranges fro -1 to 1."
  (interactive)
  (let ((die-rolls (seq-map (lambda (_) (1- (random 3)))
                            '(0 0 0 0))))
    (thread-first die-rolls
                  (rpgtk-dice-roll-mod :sum)
                  (rpgtk-dice-format-roll "4dF" nil nil 4 -4)
                  (rpgtk-message))))

(defun rpgtk-dice-roll-yearend (num-dice)
  "Displays a formatted dice expression for Year End rolls.
Where NUM-DICE are the number six-sided dice to roll.
The results are the number of 6's rolled."
  (interactive "nNumber of Dice: ")
  (rpgtk-message  (thread-first num-dice
                          (rpgtk-dice-roll 6)
                          (rpgtk-dice-roll-mod
                           :filter (lambda (d) (>= d 6))
                           :count)
                          (rpgtk-dice-format-roll nil 1))))

(provide 'rpgtk-dice)
;;; rpgtk-dice.el ends here
