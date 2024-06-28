;;; rpgtk-deck.el --- drawing cards from decks  -*- lexical-binding: t; -*-
;;
;; © 2024 Howard X. Abrams
;;   Licensed under a Creative Commons Attribution 4.0 International License.
;;   See http://creativecommons.org/licenses/by/4.0/
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams
;; Created: 25 June 2024
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
;; Creates, shuffles and draws from a standard deck of playing cards
;; (or allows creating your own deck).
;;
;; For a standard deck of playing cards, interactively call
;; `rpgtk-deck-shuffle-standard', and then call `rpgtk-deck-draw' for
;; each card (or `rpgtk-deck-draw-cards' for more than one card.
;;
;; To make your own deck, create a list of strings for the numbers
;; (called "orders" later, and "suits". The `rpgtk-deck-make' will
;; combine these to make a list of cards of each combination. You can
;; add to it a list of trump cards, e.g two jokers.

;;; Code:

(require 'rpgtk-messages)

(defface rpgtk-deck-red-suit
  '((t :foreground "tomato" :weight bold))
  "Face for red cards in a standard deck."
  :group 'rpgtk)

(defface rpgtk-deck-black-suit
  '((t :weight bold))
  "Face for black cards in a standard deck."
  :group 'rpgtk)

(defvar rpgtk-deck-current nil
  "The current deck to draw from.
Call `rpgtk-deck-shuffle' to set this variable to a shuffled
deck, and call `rpgtk-deck-draw' to take the top card from it.")

(defvar rpgtk-deck-standard-suits
  (list
    (propertize "♠" 'face 'rpgtk-deck-black-suit)
    (propertize "♣" 'face 'rpgtk-deck-black-suit)
    (propertize "♡" 'face 'rpgtk-deck-red-suit)
    (propertize "♢" 'face 'rgptk-deck-red-suit))
  "Unicode symbols of a suits of a standard deck of cards.")

(defvar rpgtk-deck-standard-order
  ["A" "2" "3" "4" "5" "6" "7" "8" "9" "J" "Q" "K"]
  "Strings of the ordered numbers of a standard deck of cards.")

(defvar rpgtk-deck-standard-trumps
  `((,(propertize "🃟" 'face 'rpgtk-deck-black-suit) "Joker")
    (,(propertize "🂿" 'face 'rpgtk-deck-red-suit) "Joker"))
  "List of the standard trumps, e.g. Jokers.")

(defun rpgtk-deck-random (&optional order suits)
  "Return a random card from a standard deck of cards.
The ORDER is a vector of strings representing the number, and
SUITS is a vector of strings associating the suit of the card.
If ORDER or SUITS is nil, this assumes a standard deck of playing cards.
The card is returned, and can be redrawn."
  (interactive)
  (unless order (setq order rpgtk-deck-standard-order))
  (unless suits (setq suits rpgtk-deck-standard-suits))
  (let ((card (rpgtk-deck-display (seq-random-elt order) (seq-random-elt suits))))
    (if (called-interactively-p 'any)
        (rpgtk-message "Drawn: %s" card)
      card)))

(defun rpgtk-deck-display (order &optional suit)
  "Return a string for displaying the card.
The ORDER is a string representing the number,
and SUIT is the associated suit of the card."
  (concat
   (propertize "⸢" 'face 'rpgtk-display-dice-sequence-border)
   (if suit
       (concat order suit)
     order)
   (propertize "⸥" 'face 'rpgtk-display-dice-sequence-border)))

(defun rpgtk-deck--display-card (pair)
  "Given PAIR as a list of order and suit, call `rpgtk-deck-display'."
  (rpgtk-deck-display (nth 0 pair) (nth 1 pair)))

(defun rpgtk-deck-display-trump (pair)
  "Return a string for displaying the card.
The card is the first element of PAIR and is a string shown in
brackets, and a label (if given as a second element of PAIR) if
appended in parens."
  (let ((card (if (seqp pair)
                  (seq-first pair)
                pair))
        (label (if (seqp pair)
                   (seq-elt pair 1))))
    (concat
     (propertize "⸢" 'face 'rpgtk-display-dice-sequence-border)
     card
     (propertize "⸥" 'face 'rpgtk-display-dice-sequence-border)
     (when label (format " (%s)" label)))))

(defun rpgtk-deck-make (&optional orders suits trumps)
  "Return a deck made of ORDERS of SUITS and TRUMPS.

If ORDERS is nil, the numbers of the deck comes from
`rpgtk-deck-standard-order', otherwise, this should be a list of
string.

If SUITS is nil, the suits for the deck come from
`rpgtk-deck-standard-suits', otherwise, this is a list of
strings. The cards are made of concatenating each order with each
suit.

TRUMPS can be nil, but if t (and not a list), then this uses
`rpgtk-deck-standard-trumps', which is two jokers. Otherwise,
this should be a list of the strings representing cards that have
no suit."
  (unless orders (setq orders rpgtk-deck-standard-order))
  (unless suits  (setq suits  rpgtk-deck-standard-suits))
  (when (and trumps (not (listp trumps)))
    (setq trumps rpgtk-deck-standard-trumps))

  ;; Given a suit, let's create a function to make a list of that suit
  ;; with every number, e.g. '(("Heart" "1") ("Heart" "2") ...)
  (let ((cards (seq-map (lambda (suit)
                          (seq-map (lambda (order)
                                     (list order suit))
                                   orders))
                        suits)))
    (thread-last
      cards
      ;; Join each list of suits into a single list:
      (apply 'append)
      (seq-map 'rpgtk-deck--display-card)
      ;; Add the trumps if they are given:
      (append (when trumps
                (seq-map 'rpgtk-deck-display-trump trumps))))))

(ert-deftest rpgtk-deck-make-test ()
  (let ((deck (rpgtk-deck-make '("1" "2" "3") '("H" "B"))))
    (should (seq-contains-p deck "⸢3B⸥" 'equal))
    (should (seq-contains-p deck "⸢2H⸥" 'equal))
    (should (seq-contains-p deck "⸢1B⸥" 'equal)))
  (let ((deck (rpgtk-deck-make '("1" "2" "3") '("H" "B") t)))
    (should (seq-contains-p deck "⸢3B⸥" 'equal))
    (should (seq-contains-p deck "⸢2H⸥" 'equal))
    (should (seq-contains-p deck "⸢1B⸥" 'equal))
    (should (seq-contains-p deck "⸢🃟⸥ (Joker)" 'equal))
    (should (seq-contains-p deck "⸢🂿⸥ (Joker)" 'equal))))

(defun shuffle-list (lst)
  "Return copy LST where elements are in random order."
  (when lst
    (let* ((index (random (length lst)))
           (start (seq-elt lst index))
           (restof (seq-remove-at-position lst index)))
      (cons start (shuffle-list restof)))))

(ert-deftest shuffle-list-test ()
  (should (null (shuffle-list nil)))
  (should (equal '(a) (shuffle-list '(a))))
  (let ((new-order (shuffle-list '(a b c))))
    (should (or (equal '(a b c) new-order)
                (equal '(b a c) new-order)
                (equal '(b c a) new-order)
                (equal '(a c b) new-order)
                (equal '(c a b) new-order)
                (equal '(c b a) new-order)))))

(defun rpgtk-deck-shuffle (&optional orders suits trumps)
  "Return a shuffled deck, and set it as the *current* deck.
See `rpgtk-deck-draw'. If ORDERS is nil, the numbers of the deck
comes from `rpgtk-deck-standard-order', otherwise, this should be
a list of string. If SUITS is nil, the suits for the deck come
from `rpgtk-deck-standard-suits'. TRUMPS can be nil, but if t,
then this uses `rpgtk-deck-standard-trumps', which is two jokers.
Otherwise, this should be a list of the strings representing
cards that have no suit."
  (interactive)
  (setq rpgtk-deck-current
        (shuffle-list (rpgtk-deck-make orders suits trumps))))

(defun rpgtk-deck-shuffle-standard ()
  "Return a shuffled, standard deck of 52 cards and two Jokers."
  (interactive)
  (rpgtk-deck-shuffle nil nil t))

(defun rpgtk-deck-draw ()
  "Return the top card from DECK.
Note that DECK is altered, so taking the last card makes the DECK
nil. If DECK is nil (or called interactively), the deck is
`rpgtk-deck-current'. See `rpgtk-deck-shuffle' for setting up
this interactive deck."
  (interactive)
  (rpgtk-message (pop rpgtk-deck-current)))

(defun rpgtk-deck-draw-cards (number-of-cards)
  "Return a list of cards of size NUMBER-OF-CARDS.
The results are drawn from the *current* deck,
see `rpgtk-deck-draw' for getting each card."
  (interactive "nNumber of cards to draw: ")
  (rpgtk-message
   "Cards: %s"
   (loop for card from 1 to number-of-cards
         with cards = nil
         do
         (push (rpgtk-deck-draw) cards)
         finally return (string-join cards " "))))

(provide 'rpgtk-deck)
;;; rpgtk-deck.el ends here
