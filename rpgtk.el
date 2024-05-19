;;; rpgtk.el --- Role Playing Game Toolkit  -*- lexical-binding: t; -*-
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
;; This supplies a number of helper functions for crafting a role
;; playing game experience in the World's Most Versatile Editor.
;;
;; The primary user interfaces are:
;;
;; The most important functions:
;;
;;  - rpgtk-message ::
;;
;;; Code:

(require 'cl)
(require 'subr-x)
(require 'hydra)

;; (add-to-list 'load-path (file-name-directory (buffer-file-name)))

(require 'rpgtk-messages)
(require 'rpgtk-dice)
(require 'rpgtk-tables)
(require 'rpgtk-odds)

(defhydra hydra-rpgtk (:color blue :hint nil) "
 ^Dice^                                ^Tables^                ^Messages^
────────────────────────────────────────────────────────────────────────────────────
 _o_: Odds (What are the odds?)        _c_: Choose from Table  _m_: Last Messages
 _d_: Roll dice / _D_: Reroll Dice       _t_: Load Tables
 _a_: d20 Advantage / _A_: Disadvantage
 _b_: BitD / _B_: Fate Dice
────────────────────────────────────────────────────────────────────────────────────
 _3_: d3  _4_: d4  _6_: d6  _8_: d8  _1_: d10  _@_: d12  _2_: d20  _0_: d100 "
  ("o" rpgtk-odds)
  ("c" rpgtk-tables-choose)
  ("t" rpgtk-tables-load)

  ("m" rpgtk-last-message)

  ("d" rpgtk-roll)
  ("D" rpgtk-roll-again)
  ("M-d" rpgtk-dice-forward-roll)
  ("a" rpgtk-dice-roll-dnd-advantage)
  ("A" rpgtk-dice-roll-dnd-disadvantage)

  ("b" rpgtk-dice-roll-bitd :color pink)
  ("B" rpgtk-dice-roll-fate :color pink)
  ("1" rpgtk-dice-roll-d10 :color pink)
  ("2" rpgtk-dice-roll-d20 :color pink)
  ("3" rpgtk-dice-roll-d3 :color pink)
  ("4" rpgtk-dice-roll-d4 :color pink)
  ("6" rpgtk-dice-roll-d6 :color pink)
  ("8" rpgtk-dice-roll-d8 :color pink)
  ("0" rpgtk-dice-roll-d100 :color pink)
  ("@" rpgtk-dice-roll-d12 :color pink)

  ;; Extra bindings:
  ("q" nil :color blue))

(define-minor-mode rpgtk-mode
  "Minor mode for layering role-playing game master functions over your notes."
  :lighter " D&D"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<f6>") 'hydra-rpgtk/body)
            map))

(defun rpgtk-init (&optional tables)
  "Initialize the RPGTK system for current file.
This loads files from TABLES directory."
  (rpgtk-tables-load rpgtk-tables-directory)
  (when tables
    (rpgtk-tables-load tables))
  (rpgtk-mode))

(provide 'rpgtk)
;;; rpgtk.el ends here
