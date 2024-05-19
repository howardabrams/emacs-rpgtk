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

;; During local development, it might be helpful to run this expression:
;; (add-to-list 'load-path (file-name-directory (buffer-file-name)))

(require 'rpgtk-messages)
(require 'rpgtk-dice)
(require 'rpgtk-tables)
(require 'rpgtk-odds)

(when (fboundp 'defhydra)
  (load-library "rpgtk-hydra"))

(defun rpgtk-init (&optional tables)
  "Initialize the RPGTK system for current file.
This loads files from TABLES directory."
  (rpgtk-tables-load)
  (when tables
    (rpgtk-tables-load tables))
  (when (boundp 'rpgtk-mode)
    (rpgtk-mode)))

(provide 'rpgtk)
;;; rpgtk.el ends here
