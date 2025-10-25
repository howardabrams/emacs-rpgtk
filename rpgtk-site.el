;;; rpgtk-site.el --- Generate random sites  -*- lexical-binding: t; -*-
;;
;; © 2025 Howard X. Abrams
;;   Licensed under a Creative Commons Attribution 4.0 International License.
;;   See http://creativecommons.org/licenses/by/4.0/
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams
;; Created: 21 October 2025
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
;; Stuck making a location that needs "more content"? I'm calling them
;; a _site_, but they could be the dungeons under an ancient ruin or
;; an infested swamp. Either way, this procedure should kick off a
;; series of relevant prompts to glue together.
;;
;; I began with _Delve_ from Shawn Tompkin's _Ironsworn_, as an initial
;; starting point, as it is really great. I just needed something more
;; general purpose.
;;
;; One could reduce a site, like a dungeon, to a simple _point crawl_
;; (going left brings you to room 12, but right brings you to the
;; locked door before room 13), but if the choices are arbitrary,
;; then the players choice would irrelevant.
;; TODO ::We should work on this ...hints on each path...
;;
;; This code, once a type of site has been defined, will generate a
;; series of _points_ or _events_, which includes a feature, but may
;; include some advantage or dangerous situation.
;;
;;; Code:

(require 'rpgtk-tables)
(require 'rpgtk-messages)

;; Every "site" has both a _theme_ and a _domain_, and we store them
;; in some global variables. These will be overridden by using the C-u
;; prefix in some functions:

(defvar rpgtk-site-domain nil
  "A string that matches one of the domains in the tables directory.")

(defvar rpgtk-site-theme nil
  "A string that matches one of the themes in the tables directory.")

;; The goal of delving into this site, whether it be a maguffin or the
;; exit, is the "objective". At any point the game ref could say "here
;; it is", we can also place it "some ways away", and track how far
;; away it is.

(defvar rpgtk-site-progress 0
  "Current state of closeness to the objective the party is.
This should not be more than `rpgtk-site-progress-objective'.")

(defvar rpgtk-site-progress-objective nil
  "Expected number of steps to accomplish objective.
Should be an integer number, often of a single digit.")

;; No site would be a dynamic, living site without some NPCs. Not sure
;; about pre-populating with their location, but maybe we could make a
;; list of possible denizens of the site.
;;
;; Still not sure how one would best configure this, so right now, I'm
;; just going to read in a hash table where the keys are the frequency
;; label, and the values are a list of names.

(defvar rpgtk-site-denizens nil
  "Hashtable of possible denizens to encounter in the current site.")

;; Note we could have set this with lisp instead of calling either:
;; `rpgtk-site-denizens-prompt' or `rpgtk-site-denizens-parse'
;;
;; (setq rpgtk-site-denizens
;;   #s(hash-table
;;      test equal
;;      data ("common"    ("wandering peasant")
;;            "uncommon"  ("knight")
;;            "rare"      ("giant insect" "giant spider")
;;            "very rare" ("imp")
;;            "legendary" ("demon"))))

;; The following functions are wrapers around the variable, and return
;; the variable, unless it is either nil or PREFIX is not nil, and
;; then it prompts for it, and caches the results in the global
;; variable:

(defun rpgtk-site-theme (&optional prefix)
  "If set, return the `rpgtk-site-theme'.
Otherwise (or if PREFIX is non-nil), prompt for it first."
  (when (or prefix (not rpgtk-site-theme))
    (setq rpgtk-site-theme
          (thread-last rpgtk-tables
                       (gethash "site/theme")
                       (completing-read "Choose theme: ")
                       (downcase)
                       (string-replace " " "-"))))
  rpgtk-site-theme)

(defun rpgtk-site-domain (&optional prefix)
  "If set, return the `rpgtk-site-domain'.
Otherwise (or if PREFIX is non-nil), prompt for it first."
  (when (or prefix (not rpgtk-site-domain))
    (setq rpgtk-site-domain
          (thread-last rpgtk-tables
                       (gethash "site/domain")
                       (completing-read "Choose domain: ")
                       (downcase)
                       (string-replace " " "-"))))
  rpgtk-site-domain)

(defun rpgtk-site-progress (&optional prefix)
  "Return a list of the progress within current site.
The first element is current progress and the second element is
the number of expected progress before reaching the objective.
If PREFIX is non-nil, prompt for size of the site first."
  (when (or prefix (not rpgtk-site-progress-objective))
    ;; If we are setting the total progress, reset the current
    ;; progress back to 0:
    (setq rpgtk-site-progress 0
          rpgtk-site-progress-objective
          (pcase (completing-read "How big is this site? "
                                  '("Tiny" "Small" "Medium" "Large"
                                    "Extra Large"))
            ("Tiny" 2)
            ("Small" 4)
            ("Medium" 6)
            ("Large" 9)
            ("Extra Large" 14))))
  rpgtk-site-progress-objective)

(defun rpgtk-site-denizens (&optional prefix)
  "Return the hash-table of denizens.
If PREFIX is non-nil, get the denizens. This can be done one of
two ways: If the region is active, parse the buffer for the
denizens by using `rpgtk-site-denizens-parse', otherwise, call
`rpgtk-site-denizens-prmpt'."
  (when (or prefix
            (null rpgtk-site-denizens)
            (= 0 (hash-table-count rpgtk-site-denizens)))
    (setq rpgtk-site-denizens (make-hash-table :test 'equal))
    (if (not (region-active-p))
        (rpgtk-site-denizens-prompt)
      (rpgtk-site-denizens-parse)
      (goto-char (region-end))))
  rpgtk-site-denizens)

(defun rpgtk-site-denizens-prompt ()
  "Prompt the user for a list of denizens and their frequency."
  (clrhash rpgtk-site-denizens)
  (let* ((split-string-default-separators " *, *"))
    (dolist (freq '("common" "uncommon" "rare"
                    "very rare" "legendary"))
      (puthash freq
               (thread-last freq
                            (format "%sly encountered denizens: ")
                            (capitalize)
                            (read-string)
                            (split-string))
               rpgtk-site-denizens))))

(defun rpgtk-site-denizens-parse (&optional start end)
  "Parse buffer (or a region) for a pattern of denizens.
Looking for the following pattern in the buffer:

  - Commonly found :: peasant
  - Uncommon denizens :: giant spider,rat swarm
  - Rarely encountered :: knight
  - Very rare :: imp
  - Legendary :: hag

The buffer region is marked from START to END."
  (clrhash rpgtk-site-denizens)
  (cl-flet ((prop-strip (s)
              (set-text-properties 0 (length s) nil s)
              s)
            (labeling (s)
              (cond
               ((string-match "^common" s) "common")
               ((string-match "^uncommon" s) "uncommon")
               ((string-match "^rare" s) "rare")
               ((string-match "^very" s) "very rare")
               ((string-match "^legend" s) "legendary"))))
    (save-excursion
      (when start
        (goto-char start))
      (while (re-search-forward
              (rx line-start
                  (zero-or-more space) (or "*" "-" "+")
                  (one-or-more space)
                  (group (+? any))
                  (zero-or-more space) (1+ ":")
                  (zero-or-more space)
                  (group (+ any))) end t)
        (let* ((split-string-default-separators " *, *")
               (label (prop-strip (match-string 1)))
               (value (split-string (prop-strip (match-string 2)))))
          (puthash (labeling label) value rpgtk-site-denizens))))))

;; The following functions create different aspects of a site:

(defun rpgtk-site-feature (theme domain)
  "Return a feature by consulting the THEME or DOMAIN.
This is done by rolling on tables, so make sure you load
the tables with a `site/feature/theme' that matches the
currently stored theme. Same with the domain."
  (if (<= (random 100) 20)
      (rpgtk-tables-choose-str (format "site/feature/theme/%s" theme))
    (rpgtk-tables-choose-str (format "site/feature/domain/%s" domain))))

(defun rpgtk-site-exit-number ()
  "Return the number of paths from this scene.
The paths are the same a denizen rarity."
  (let ((rarity (random 100)))
    (cond
     ((< rarity 50) 1)
     ((< rarity 75) 2)
     ((< rarity 88) 0)
     ((< rarity 96) 3)
     (t             4))))

(defun rpgtk-site-denizen (_)
  "Return name of a denizen encountered in current site.
If a hashtable of denizens in `rpgtk-site-denizens'
has not been defined, return a label of how common it is."
  (let* ((rarity (random 100))
         (label (cond
                 ((< rarity 50) "common")
                 ((< rarity 75) "uncommon")
                 ((< rarity 88) "rare")
                 ((< rarity 96) "very rare")
                 (t "legendary/boss"))))

    ;; Return the name of a denizen from hash table:
    (seq-random-elt
     (gethash label rpgtk-site-denizens
              (list (format "%s denizen" label))))))


(defun rpgtk-site-danger (theme domain)
  "Return a danger by consulting the THEME or DOMAIN.
This is done by rolling on tables, so make sure you load
the tables with a `site/danger/theme' that matches the
currently stored theme. Same with the domain."
  (let ((roll (random 100)))
    (cond
     ((<= roll 30) (rpgtk-tables-choose-str (format "site/danger/theme/%s" theme)))
     ((<= roll 45) (rpgtk-tables-choose-str (format "site/danger/domain/%s" domain)))
     ((<= roll 57) "You encounter a hostile denizen.")
     ((<= roll 68) "You face an environmental or architectural hazard.")
     ((<= roll 76) "A discovery undermines or complicates your quest.")
     ((<= roll 79) "You confront a harrowing situation or sensation.")
     ((<= roll 82) "You face the consequences of an earlier choice or approach.")
     ((<= roll 85) "Your way is blocked or trapped.")
     ((<= roll 88) "A resource is diminished, broken, or lost.")
     ((<= roll 91) "You face a perplexing mystery or tough choice.")
     ((<= roll 94) "You lose your way or are delayed."))))

(defun rpgtk-site-parts (prefix)
  "Regenerate all features of a stie."
  (list (rpgtk-site-theme prefix)
        (rpgtk-site-domain prefix)
        (rpgtk-site-progress prefix)
        (rpgtk-site-denizens prefix)))

(defun rpgtk-site-scene-parts (&optional prefix)
  "Return descriptive scene and event in a site.
If PREFIX is given, pass that to helper functions to define a new
site, see the functions (not variables, this is a Lisp-2 after
all) of `rpgtk-site-theme', `rpgtk-site-deomain', etc."
  (seq-let (theme domain denizens total-progress)
      (rpgtk-site-parts prefix)
    (let* (progress opportunity danger
           (feature (rpgtk-site-feature theme domain))
           (roll (random 100)))
      (cond
       ((<= roll 35)
        (setq opportunity (rpgtk-tables-choose-str "site/opportunity")
              progress t))
       ((<= roll 41)
        (setq opportunity
              (rpgtk-tables-choose-str "site/opportunity")))
       ((<= roll 55) (setq progress t))
       ((<= roll 75)
        (setq danger (rpgtk-site-danger theme domain)
              progress t))
       (t
        (setq danger (rpgtk-site-danger theme domain))))

      (when progress
        (setq rpgtk-site-progress
              (1+ rpgtk-site-progress)))

      ;; Fix the denizen name (if any found)
      (when danger
        (setq danger
              (replace-regexp-in-string "denizen"
                                        #'rpgtk-site-denizen
                                        danger)))
      (when opportunity
        (setq opportunity
              (replace-regexp-in-string "denizen"
                                        #'rpgtk-site-denizen
                                        opportunity)))

      (if (>= rpgtk-site-progress total-progress)
          (list theme domain "Objective Accomplished")
        (list theme domain feature progress
              rpgtk-site-progress total-progress
              opportunity danger)))))

(defun rpgtk-site-scene (prefix)
  "Display a random scene in a stored site.
If PREFIX, prompt for the domain and theme of the site."
  (interactive "P")
  (seq-let (theme domain feature
                  progress current total
                  opportunity danger)
      (rpgtk-site-scene-parts prefix)
    (message "Delve into %s %s: %s %s %s"
             theme domain feature
             (if progress
                 (format "[Progress %d/%d]" current total)
               "-")
             (or opportunity danger ""))))

(defun rpgtk-site-scene-insert (prefix)
  "Insert the current scene of a stored site into the buffer.
If PREFIX, prompt for a new site to delve."
  (interactive "P")
  (seq-let (theme domain feature
                  progress current total
                  opportunity danger)
      (rpgtk-site-scene-parts prefix)
    (insert (format "# Next scene in %s %s" theme domain))
    (insert
     (if progress
         (format " (Mark progress: %d/%d):\n" current total)
       ":\n"))
    (insert (format "Feature: %s\n" feature))
    (when opportunity
      (insert (format "Opportunity: %s\n" opportunity)))
    (when danger
      (insert (format "Danger: %s\n" danger)))
    (insert (format "Exits: %d\n\n" (rpgtk-site-exit-number)))))

(defun rpgtk-site-start ()
  "Start delving into a new site.
Calling this function will prompt the user with _theme_ and
_domain_ and _length_ of the new site. With no buffer region
selected, this function will prompt for potential encounters.
Otherwise it will parse the region looking denizen likelihood
based on the following format:

  - Commonly found :: <creature1>, <creature2>, ...
  - Uncommon denizens :: <creature3>, <creature4>, ...
  - Rarely encountered :: ...
  - Very rare :: ...
  - Legendary :: ..."
  (interactive)
  (rpgtk-site-parts t))

(provide 'rpgtk-site)
;;; rpgtk-site.el ends here
