;;; rpgtk-odds.el --- Twisted 50/50 coin toss. -*- lexical-binding: t; -*-
;;
;; Copyright © 2024 Howard X. Abrams
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams <howard.abrams@gmail.com>
;; Created: May 3, 2024
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; While popular with the Solo RPGers, as a GM, when my players come
;; up with a crazy idea, and ask something like, “Is there a candy
;; vendor anywhere on this street?” I often feel to turn to a random
;; number generator.
;;
;; A popular Luck or Fate chart involves rolling a six-sided dice with these interpretations:
;;
;;   1. No, and …
;;   2. No
;;   3. No, but …
;;   4. Yes, but ..
;;   5. Yes
;;   6. Yes, and ..
;;
;; In other words, when rolling a dice and landing on a 1, you not
;; only say “no”, but you make the situation worse, but on a 6, you
;; get a yes answer, and make it even better.
;;
;; See https://www.howardism.org/RPG/programming-and-but.html
;; for details on the whats and whys of this code.
;;
;; The Mythic GM Emulator [https://www.wordmillgames.com/mythic.html],
;; has a table of shifting odds, where you first decide on the
;; _likelihood_ of an event, and the 50/50 coin toss, adjusts up or
;; down the scale.
;;
;; This code re-imagines the *Fate Chart* with the *Yes, and* chart.
;;
;;    *Note:* One of the goals (or non-goals) attempts to keep the
;;    math simple. Not only should the algorithms (and code) to be
;;    readable, the yes/no boundary off by a few percentage points
;;    won’t be obvious when combined with random numbers.
;;
;;; Code:

(require 'rpgtk-dice)

(defvar rpgtk-odds-of-chaos-level 0
  "A value from -4 to +4 that affect the outcomes from the `odds' function.")

(defvar rpgtk-odds-of-chaos-max-level 4
  "The maximum value the chaos level could reach.
It also refers to the negative value limit, too.")

(defvar rpgtk-odds-no-low-mark 0.25
  "The percentage of the ‘no’ section for worse results.")

(defvar rpgtk-odds-no-high-mark 0.25
  "The percentage of the ‘no’ section for not-that-bad results.")

(defvar rpgtk-odds-yes-low-mark 0.25
  "The percentage of the ‘yes’ section for complications or less-desirable results.")

(defvar rpgtk-odds-yes-high-mark 0.25
  "The percentage of the ‘yes’ section for better than expected results.")

(defun rpgtk-highlight-choices (message)
  "Add emphasizing properties to the keystrokes in MESSAGE."
  (let ((start 0)
        (re (rx space (group (one-or-more (not space))) (group ": "))))
    (while (string-match re message start)
      (let* ((key-start (match-beginning 1))
             (key-end   (match-end 1))
             (par-start (match-beginning 2))
             (par-end   (match-end 2)))
        (put-text-property key-start key-end
                           'face 'hydra-face-blue message)
        (put-text-property par-start par-end 'face
                           '(:foreground "#666666") message))
      (setq start (match-end 0))))
  message)

(defvar rpgtk-choose-likelihood-prompt
  (rpgtk-highlight-choices
   "On a scale of 1: low to 9: high, what is the likelihood of a positive answer?
⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯
  i: Impossible      v: Very unlikely  u: Unlikely
  l: Likely          p: Probably       a: Absolutely
  n: Quite sure No   ⮐: Unsure…50/50  y: Quite sure Yes"))

(defun rpgtk-odds-choose-likelihood ()
  "Query user and return a numeric _odds_ level.
This number is from 1 to 10, where 5 is a 50/50."
  (let ((choice (read-char rpgtk-choose-likelihood-prompt)))
    (cond
     ((eq choice ?1)  10)    ; Numeric values correspond to a
     ((eq choice ?2)  10)    ; percentage, so typing 3 means 30%
     ((eq choice ?3)  30)
     ((eq choice ?4)  40)
     ((eq choice ?5)  50)
     ((eq choice ?6)  60)
     ((eq choice ?7)  70)
     ((eq choice ?8)  80)
     ((eq choice ?9)  90)

     ((eq choice ?i)  13)    ; With three negative values, we
     ((eq choice ?v)  25)    ; split under 50 section
     ((eq choice ?u)  37)
     ((eq choice ?l)  62)    ; Same with the three positive values
     ((eq choice ?p)  75)
     ((eq choice ?a)  87)

     ((eq choice ?n)  25)    ; The yes and no options are
     ((eq choice ?y)  75)    ; quarter options, 25% and 75%
     (t               50)))) ; Anything else is a 50/50

(defun rpgtk-odds-increase-chaos-level ()
  "Increase the current session's chaos level.
Does nothing if the chaos is at max value, according to
`rpgtk-odds-of-chaos-max-level'."
  (interactive)
  (when (< rpgtk-odds-of-chaos-level rpgtk-odds-of-chaos-max-level)
    (setq rpgtk-odds-of-chaos-level (1+ rpgtk-odds-of-chaos-max-level))))

(defun rpgtk-odds-decrease-chaos-level ()
  "Decrease the current session's chaos level.
Does nothing if the chaos is at min value, according to
`rpgtk-odds-of-chaos-max-level'."
  (interactive)
  (when (> rpgtk-odds-of-chaos-level (- rpgtk-odds-of-chaos-max-level))
    (setq rpgtk-odds-of-chaos-level (1- rpgtk-odds-of-chaos-max-level))))

(defun rpgtk-odds-choose-chaos-level ()
  "Read the chaos level, as a number, from the user."
  (read-number
   (format "Choose a chaos level from -%d to +%d.
Higher values increase odds of a ‘yes’ value. "
           rpgtk-odds-of-chaos-max-level rpgtk-odds-of-chaos-max-level)
   rpgtk-odds-of-chaos-level))

(defun rpgtk-odds-likelihood-offset (likelihood &optional chaos-level)
  "Return modified value of LIKELIHOOD affected by CHAOS-LEVEL.
If CHAOS-LEVEL is `0', return LIKELIHOOD unmodified.
Otherwise, increase or decrease the returned value of LIKELIHOOD
based on the magnitude of the positive or negative value of
CHAOS-LEVEL."
  (unless chaos-level
    (setq chaos-level rpgtk-odds-of-chaos-level))

  (let ((chaos-magnitude (abs chaos-level))
        (chaos-limit  (1+ rpgtk-odds-of-chaos-max-level)))
    (cond
     ;; If chaos is normal, return the original likelihood:
     ((= 0 chaos-level)   likelihood)

     ;; If chaos is high, take the area of possibilities (from
     ;; likelihood level to 100%), and divide into `chaos-limit'
     ;; sections (e.g. 5 levels) for each chaos level:
     ((>= chaos-level 0)
      (+ likelihood (* chaos-magnitude (/ (- 100 likelihood) chaos-limit))))

     ;; Otherwise, the chaos is low, so we do the same thing, but from
     ;; 0% to the given likelihood level:
     (t (- likelihood (* chaos-magnitude (/ likelihood chaos-limit)))))))

(defun rpgtk-odds-odds-markers (likelihood)
  "Return a list of yes, and limit values from LIKELIHOOD.

The LIKELIHOOD should be between 10 and 90 representing
the percentage separating ‘yes’ values from ‘no’ values.
Where the higher the value, the greater the ‘yes’ chance."
  (when (< likelihood 1)
    (setq likelihood (* 100 likelihood)))
  (when (< likelihood 10)
    (setq likelihood (* 10 likelihood)))

  (list
   (* likelihood yes-high-mark)
   (- likelihood (* likelihood yes-low-mark))
   likelihood
   (+ likelihood (* (- 100 likelihood) no-high-mark))
   (- 100 (* (- 100 likelihood) no-low-mark))))

(defun rpgtk-odds-odds-results (roll yes-high yes-low yes-or-no no-low no-high)
  "Compare ROLL with the values of the rest of the parameter.
Return a list consisting of the `main-message', a `helper-message',
and a proper face color to display the `main-message'.

If ROLL is less-than or equal to:

YES-HIGH (lowest number), we return ‘yes, and’
YES-LOW, we return ‘yes’
YES-OR-NO, we return ‘yes, but’
NO-LOW, we return ‘no, but’
NO-HIGH, we return ‘no’
Otherwise, we return ‘no, and’."
  (let ((yes-and  '("Yes, and" "make it better"     rpgtk-critical-success-roll))
        (yes-only '("Yes"      nil                  rpgtk-successful-roll))
        (yes-but  '("Yes, but" "add a complication" rpgtk-middlin-roll))
        (no-but   '("No, but"  "add a slight help"  rpgtk-other-roll))
        (no-only  '("No"       nil                  rpgtk-failed-roll))
        (no-and   '("No, and"  "make it worse"      rpgtk-critical-failure-roll)))
    (cond
     ((<= roll yes-high)  yes-and)
     ((<= roll yes-low)   yes-only)
     ((<= roll yes-or-no) yes-but)
     ((<= roll no-low)    no-but)
     ((<= roll no-high)   no-only)
     (t                  no-and))))

(defun rpgtk-odds (prefix likelihood &optional chaos-level)
  "Return a random Yes or No message (with twists).

The LIKELIHOOD represents the percentage that \"something\"
will be true (or \"yes\"), and should be a value from 10 to 90.

The CHAOS-LEVEL should be a value from -5 to +5.
The higher the CHAOS-LEVEL, the more chance for a yes.

With PREFIX, return message with more stats on the random roll."
  (interactive (list current-prefix-arg
                     (rpgtk-odds-choose-likelihood)
                     (rpgtk-odds-choose-chaos-level)))

  ;; Since this is an interactive function and the primary interface,
  ;; we need to validate all the parameters (or adjust them to fit):
  (cond
   ((< likelihood 0)
    (error "The likelihood odds require a percentage between 10 and 90"))
   ((> likelihood 90)
    (error "The odds won't work with likelihoods above 90%"))
   ((> chaos-level odds-of-chaos-max-level)
    (error "The chaos-level, %d, can't be above %d"
           chaos-level odds-of-chaos-max-level))
   ((< chaos-level (- odds-of-chaos-max-level))
    (error "The chaos-level, %d, can't be below -%d"
           chaos-level odds-of-chaos-max-level))
   ((< likelihood 1)
    (setq likelihood (* 100 likelihood)))
   ((< likelihood 10)
    (setq likelihood (* 10 likelihood))))

  ;; The `seq-let' assigns each member of the list returned by
  ;; `odds-markers' to individual variables:
  (seq-let (yes-high yes-low yes-or-no no-low no-high)
      (odds-markers (rpgtk-odds-likelihood-offset likelihood chaos-level))

    (let ((roll (random 100)))

      ;; This `seq-let' assigns differt parts of the "results table"
      ;; to variables to use to re-color text differently:
      (seq-let (main help color)
          (odds-results roll yes-high yes-low yes-or-no no-low no-high)

        ;; Giving the `C-u' prefix will display more information:
        (rpgtk-message
         (if prefix
             (rpgtk-odds-display-message-full main help color roll
                                              yes-high yes-low yes-or-no
                                              no-low no-high)
           (rpgtk-odds-display-message main help color)))))))

(defun rpgtk-odds-display-message (main help color)
  "Format MAIN and HELP into a propertized string.
Initial MAIN is formatted with COLOR."
  (if help
      (format "%s %s %s"
              (propertize main 'face `,color)
              (propertize "..." 'face 'rpgtk-display-dice-sequence-separator)
              (propertize help 'face 'rpgtk-dimmed-display))
    (propertize main 'face `,color)))

(defun rpgtk-odds-display-message-full (main-msg help-msg color roll
                                                 yes-high yes-low yes-or-no
                                                 no-low no-high)
  "Format MAIN-MSG and HELP-MSG into a propertized string.
Initial MAIN-MSG is formatted with COLOR. ROLL is a random
number, and displayed in a dimmer color, along with the boundary
values: YES-HIGH, YES-LOW, NO-LOW, NO-HIGH and YES-OR-NO, which
should be numbers."
  (cl-flet ((propertize-num (num)
              (propertize (number-to-string num) 'face
                          'rpgtk-display-dice-sequence)))

    (concat (rpgtk-odds-display-message main-msg help-msg color)
            (propertize "  (" 'face 'rpgtk-display-dice-sequence-separator)
            (propertize "Rolled: " 'face 'rpgtk-roll-expression)
            (propertize-num roll)
            (propertize " … " 'face 'rpgtk-dimmed-display)
            (propertize-num yes-high)
            (propertize " < " 'face 'rpgtk-display-dice-sequence-separator)
            (propertize-num yes-low)
            (propertize " < " 'face 'rpgtk-display-dice-sequence-separator)
            (propertize-num yes-or-no)
            (propertize " < " 'face 'rpgtk-display-dice-sequence-separator)
            (propertize-num no-low)
            (propertize " < " 'face 'rpgtk-display-dice-sequence-separator)
            (propertize-num no-high)
            (propertize ")" 'face 'rpgtk-display-dice-sequence-separator))))

(provide 'rpgtk-odds)
;;; rpgtk-odds.el ends here
