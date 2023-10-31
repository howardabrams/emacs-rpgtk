;; rpgtk-tables.el -- Choose item from tables -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Howard X. Abrams
;;
;; Author: Howard X. Abrams <http://gitlab.com/howardabrams>
;; Maintainer: Howard X. Abrams <howard.abrams@workday.com>
;; Created: January  8, 2021
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; By storing parse-able tables in files, we can randomly choose
;; entries. The primary interface functions are:
;;
;;   - `rpgtk-tables-load' :: Which when pointed to a directory, stores the
;;                            data in all text files read.
;;
;;   - `rpgtk-tables-choose' :: Which, when a table is chosen, returns a
;;                              random element.
;;
;; The files read can be formatted in the following ways:
;;
;;   - simple org lists, where every element has equal weight
;;
;;   - org tables, where the second column is a _frequency_ level,
;;     where some items will be chosen more often than others.
;;
;;   - dice tables, where the first column is a specified range, which
;;     mimics the tables found in many role-playing game supplements.
;;
;; These types of tables are described later in the code.
;;
;;; Code:

(require 'cl)
(require 'subr-x)

(require 'rpgtk-dice (expand-file-name "rpgtk-dice.el" (file-name-directory (buffer-file-name))))
(require 'rpgtk-messages (expand-file-name "rpgtk-messages.el" (file-name-directory (buffer-file-name))))

(defcustom rpgtk-tables-directory
  (expand-file-name "tables" (file-name-directory (buffer-file-name)))
  "Directory path containing the tables to load and create functions."
  :group 'rpgtk
  :type '(directory))

(defvar rpgtk-tables (make-hash-table :test 'equal)
  "Collection of tables and lists to select. See `rogtk-tables-choose'.")

(defun rpgtk-tables-load (&optional filepath)
  "Read and parse table files located in FILEPATH directory.
Files will be stored in `rpgtk-tables' hash, and available by name
when calling `rpgtk-tables-choose'."
  (interactive (list (read-directory-name "RPG Tables Directory: "
                                          rpgtk-tables-directory)))
  (unless filepath
    (setq filepath rpgtk-tables-directory))
  (rpgtk-tables--load-dir filepath)
  (message "Read: %s"
           (mapconcat 'identity (hash-table-keys rpgtk-tables) ", ")))

(defun rpgtk-tables--load-dir (filepath &optional prefix)
  "Read and parse the files in the directory given by FILEPATH.
PREFIX all tables if this parameter is given."
  (dolist (file (directory-files filepath t (rx bol (not "."))))
    (let* ((table-name (file-name-base file))
           (new-prefix (if prefix
                           (concat prefix "/" table-name)
                         table-name) ))
      (cond ((file-directory-p file)
             (rpgtk-tables--load-dir file new-prefix))
            ((and (file-regular-p file) (file-readable-p file))
             (rpgtk-tables--almost-load-file file prefix))))))

(defun rpgtk-tables--almost-load-file (filepath &optional prefix)
  "Loading hundreds of tables takes a long time, and fills up memory.
Instead, we store a reference to FILEPATH with the optional
PREFIX, and then if we query for that table (and it is just a
string), we load it then."
  (let ((name (file-name-base filepath)))
    (if prefix
        (setq name (format "%s/%s" prefix name)))
    (puthash name filepath rpgtk-tables)))

(defun rpgtk-tables-load-file (filepath &optional name)
  "Read, parse and store the table given by FILEPATH.
Store it by NAME in the `rpgtk-tables' hash table."
  (interactive (list (read-file-name "RPG Table: " rpgtk-tables-directory)))
  (when (null name)
    (setq name (file-name-base filepath)))
  (let ((contents (rpgtk-tables--read-table-file filepath)))
    (when (called-interactively-p nil)
      (message "Read: %s" name))
    (puthash name contents rpgtk-tables)))

(defun rpgtk-tables-choose (table-name)
  "Display an element from a file, TABLE-NAME.
See `rpgtk-tables-choose-str' for details."
  (interactive (list (completing-read "Choose from Table: "
                                      (sort (hash-table-keys rpgtk-tables) #'string-lessp))))
  (let ((results (rpgtk-tables-choose-str table-name)))
    (kill-new results)
    (rpgtk-message "%s" results)))

(defun rpgtk-tables-choose-str (table-name)
  "Return random item from a table of a given TABLE-NAME string.

The name is searched in the `rpgtk-tables' hash-table, and the
value returned is a _table_ of sorts. It could be a list, which
would be easy (see `rpgtk-tables--choose-list'), or it is either
a freq-uency table (see `rpgtk-tables--choose-freq-table') or a
dice table (see `rpgtk-tables--choose-dice-table')."
  (when-let ((table  (gethash table-name rpgtk-tables)))
    (when (stringp table)
      (setq table (rpgtk-tables-load-file table table-name)))
    (let ((result
           (cond ((rpgtk-tables-dice-obj-p table) (rpgtk-tables--choose-dice-table table))
                 ((hash-table-p table) (rpgtk-tables--choose-freq-table table))
                 ((listp table)        (rpgtk-tables--choose-list table))
                 ((symbolp table)      (funcall table))
                 (t (format "Error: Could not choose anything from %s (internal bug?)" table-name)))))

      (thread-first result
                    ;; Replace dice expression in the message with an roll:
                    (rpgtk-dice-format-string)
                    ;; Replace [one/two/three] with one of those words:
                    (rpgtk-tables--choose-string-list)))))

(defun rpgtk-tables--choose-list (lst)
  "Randomly choose (equal chance for any) element in LST."
  (when lst
    (seq-random-elt lst)))

(defun rpgtk-tables--choose-string-list (str)
  "Return a substituted item from _string-list_ in STR.
For instance, the string: 'You found a [chair/box/statue]'
would be converted randomly to something like: 'You found a box.'"
  (let ((regexp (rx "[" (+? any) "/" (+? any) "]"))
        (subbed (lambda (str) (--> str
                              (substring it 1 -1)
                              (s-split (rx (*? space) "/" (*? space)) it)
                              (seq-random-elt it)))))
    (replace-regexp-in-string regexp subbed str)))


;; I originally thought that I could have a single regular expression that
;; matched all possible tables, but that is a bit too complicated. The following
;; regular expression, however, will parse a list or a frequency table.

(defvar rpgtk-tables--line-parse
  (rx bol (zero-or-more space)
      (optional
       (any "+" "-" "*" "|")
       (one-or-more space))
      (group (+? any))
      (optional (zero-or-more space) (or ":" "|") (zero-or-more space)
                (group (one-or-more alphanumeric))
                (zero-or-more space) (optional (or ":" "|")) (zero-or-more space))
      eol)
  "A regular expression for locating parsable lines.")

(defun rpgtk-tables--read-table-file (table-file)
  "Read and parse TABLE-FILE as data. Whatever that means."
  (when (and (file-regular-p table-file) (file-readable-p table-file))
    (with-temp-buffer
      (insert-file-contents table-file)
      (goto-char (point-min))
      (flush-lines (rx bol (zero-or-more space) "#"))

      ;; The following predicates are not /pure functions/, as they scan the
      ;; current buffer, leaving the initial match in the 'hopper', so the parsing
      ;; function called makes that assumption, and will immediately grab that
      ;; `match-string' and then parse the rest of the buffer.
      (cond
       ((rpgtk-tables-dice-table?) (rpgtk-tables--parse-as-dice-table))
       ((rpgtk-tables-freq-table?) (rpgtk-tables--parse-as-freq-table))
       (t (rpgtk-tables--parse-as-list))))))

(defun rpgtk-tables--parse-as-list ()
  "Return list of lines matching `rpgtk-tables--line-parse'."
  (let ((results (list (match-string-no-properties 1))))
    (while (re-search-forward rpgtk-tables--line-parse nil t)
      (let ((entry (match-string-no-properties 1)))
        (setq results (cons entry results))))
    results))

;; ----------------------------------------------------------------------
;; FREQUENCY TABLES
;; ----------------------------------------------------------------------
;;
;; While the a table could be simple lists to choose a
;; random element, some lists could return /some elements/ more often
;; than /other elements/. While that sounds great in a sentence, this
;; code in this section describes this concept of /frequency tables/.
;; For instance, here is a Faction Encounter table:
;;
;;  | Church of Talos :: Worshipers of the god of storms/destruction.   | scarcely   |
;;  | City Watch :: Members of the Waterdeep constabulary.              | often      |
;;  | Cult of the Dragon :: Cultists who venerate evil dragons.         | seldom     |
;;  | Emerald Enclave :: Alliance of druids/rangers to defend the wilds | seldom     |
;;  | Enclave of Red Magic :: Thayan mages who smuggle slaves           | sometimes  |
;;  | Force Grey :: League of heroes sworn to protect Waterdeep.        | often      |
;;  | Halaster’s Heirs :: Dark arcanists trained at a hidden academy    | rarely     |
;;  | The Kraken Society :: Shadowy group of thieves and mages          | rarely     |
;;
;; While a Waterdeep could have over 50 factions running around, we
;; would assume players would run into the City Watch more often than
;; the delusional members of the /Kraken Society/.
;;
;; Unlike a normal list, we should have two columns, where the first
;; is the item and the second determines the frequency. The
;; `rpgdm-tables--line-parse' regular expression can already parse,
;; and return two groups, so we can create a predicate:

(defun rpgtk-tables-freq-table? ()
  "Return non-nil if current buffer has a frequency table."
  (goto-char (point-min))
  (re-search-forward rpgtk-tables--line-parse nil nil)
  (match-string 2))

;; Notice that this predicate is not a /pure function/, as it scans
;; the current buffer, leaving the initial match in the /hopper/, so
;; the parsing function called next, can /make an assumption/, and to
;; immediately grab that `match-string' and then parse the rest of the
;; buffer.

;; The `rpgdm-tables--parse-as-freq-table' will return a `hash-table'
;; where the /keys/ are the frequency labels, like 'often' or
;; 'common', and the /values/ will be the list of items of that
;; frequency.

;; First, grab the previously matched entry and store them, and then
;; with each successive match, append them (via `cons') to whatever
;; list is already there.

(defun rpgtk-tables--parse-as-freq-table ()
  "Return hashtable of lines matching `rpgtk-tables--line-parse'.
The keys in the hashtable are the tags from the file, and the
values will be a list of matching entries of that tag.

For instance, the file:
  - salt :common:
  - silver :rare:
  - gold :rare:
  - pepper :common:

Would return a hashtable containing:

  rare: [gold silver]
  common [salt peper]"
  ;; Create a hash, populated it, and return it:
  (let ((results (make-hash-table :test 'equal))
        (entry (match-string-no-properties 1))
        (tag (match-string-no-properties 2)))
    ;; Store initial match from parent call:
    (puthash tag (list entry) results)

    (while (re-search-forward rpgtk-tables--line-parse nil t)
      (let* ((entry (match-string-no-properties 1))
             (tag (match-string-no-properties 2))
             (prev-list (gethash tag results)))
        (puthash tag (cons entry prev-list) results)))

    ;; Combine the sublists of equivalent tags:
    (rpgtk-tables--merge-frequencies results)))

;; The `rpgtk-tables--parse-as-freq-table' needs to massage the hash
;; table a wee bit, as a frequency label may be written, _scarce_ one
;; time, and _scarcely_ another time. This is allowed, but the
;; `rpgtk-tables--merge-frequencies' combines them.

(defun rpgtk-tables--merge-frequencies (table)
  "Combine the values of equivalent table-tags in TABLE.
A table, read as a hash table, may have similar, but equal tags.
For instance, `veryrare' and `very-rare' are the same."
  (let* ((table-tags (rpgtk-tables--which-tag-group table))
         (tags (seq-map 'rest table-tags)))  ; Ignore all the numbers
    (dolist (subtag-list tags)
      (unless (= 1 (length subtag-list))
        (let ((keeper (first subtag-list)))
          (dolist (tag (rest subtag-list))
            (puthash keeper (append (gethash keeper table)
                                    (gethash tag table)) table)
            (remhash tag table)))))
    table))

;; Choosing an element in a hash of tags seems ... challenging. This
;; is because I want the tags to somehow add a particular weight to
;; the randomness. Not a complete standard distribution (bell curve),
;; but a little more favor to some items. For instance, labeling
;; something 'common' should show more often than 'uncommon'.
;;
;; Choosing an item from a hash table is a complicated algorithm that
;; may not be as obvious by reading the code, so let's describe this
;; with an example. Assume we have the following frequency table with
;; a /relative weight/ for each tag:
;;
;;   - often : 4
;;   - seldom : 3
;;   - scarely : 2
;;   - rarely : 1
;;
;; Is coded with the following list of lists:
;;
;;    ((4 "often")
;;     (3 "seldom" "sometimes")
;;     (2 "scarcely" "scarce" "hardly ever")
;;     (1 "rarely"))
;;
;; Read this as: we should have 4 times as many items labeled "often"
;; as "rarely".
;;
;; So we use the function, `rpgdm-tables--table-distribution' to make
;; a table-specific tag list, usually called `table-tags', where:
;;
;;    each weight = the number of items * relative weight
;;
;; So if we had 11 items in the table tagged as "often", and 8 rare
;; items, we would have a tag table as:
;;
;;    ((44 "often") (27 "seldom") (22 "scarcely") (8 "rarely"))
;;
;; Granted, English tags and their relative weights are hard-coded at
;; the moment. But this really should just be part of the file,
;; perhaps as a buffer-local variable?

(defconst rpgtk-tables-tag-groups
  '(((12 "common")
     (7 "uncommon")
     (4 "rare")
     (2 "veryrare" "very-rare" "very rare")
     (1 "legendary"))

    ((4 "often")
     (3 "seldom" "sometimes")
     (2 "scarcely" "scarce" "hardly ever")
     (1 "rarely")))
  "Weighted values for ENGLISH tags.

TODO: figure out a way to abstract this, and maybe even store it
in the text file itself, instead of relying on language-specific
terms.")

(defun rpgtk-tables--choose-freq-table (table)
  "Select item from a hash TABLE.
Note that tables stored in a hash table have weight keys and a list
of items associated with that weight."
  (let* ((table-tags (rpgtk-tables--table-distribution table))
         (tag (rpgtk-tables--choose-tag table-tags)))
    (seq-random-elt (gethash tag table))))


(defun rpgtk-tables--relevel-table (table tag)
  "Given a TAG of a hash TABLE, return new relative level.
The new value is based on the original weight, e.g. 4 and the
number of items of that particular tag.

Note: This is a helper function for `rpgtk-tables--table-distribution'."
  (let* ((name  (second tag))
         (items (gethash name table))
         (weight (first tag))
         (new-weight (* weight (length items))))
    (list new-weight name)))

(defun rpgtk-tables--table-distribution (table)
  "Return a relative frequency tag group for a given TABLE.
Works by running map over the table's tags through the
`rpgtk-tables--relevel-table' helper function."
  (let ((table-tags (rpgtk-tables--which-tag-group table)))
    (seq-map
     (lambda (it) (rpgtk-tables--relevel-table table it))
     table-tags)))


(defun rpgtk-tables--sum-tag-weights (tags)
  "The TAGS is a list of lists where the first element is a numeric weight.
Using `-reduce' allows us to sum these, but we need to make sure that the
first element of our list is our initial condition, so we `cons' a 0 onto
the start."
  (seq-reduce (lambda (acc it) (+ acc (car it))) tags 0))

(defun rpgtk-tables--find-tag (roll tag-list)
  "Given a ROLL as a level in TAG-LIST, return matching tag.
The matching is based on the weight. A million ways to do this,
but stepping through the list of tags to see roll is in that
*window*, and if not, both move to the next tag, as well as
decrement the ROLL value."
  (cl-loop for (num-elems tag) in tag-list do
           ;; (message "Comparing %d <= %d for %s" roll num-elems tag)
           (if (<= roll num-elems)
               (cl-return tag)
             (cl-decf roll num-elems))))

(defun rpgtk-tables--choose-tag (tags)
  "Select random tag from TAGS in `rpgtk-tables-tag-groups'.
Uses helper function, `rpgtk-tables--find-tag'."
  (let* ((upper-limit (rpgtk-tables--sum-tag-weights tags))
         (roll (1+ (random upper-limit))))
    ;; (message "Rolled %d on %d" roll upper-limit)
    (rpgtk-tables--find-tag roll tags)))

(defun rpgtk-tables--which-tag-group (table)
  "Return the tag table-tags associated with TABLE."
  (let (results
        (tag (first (hash-table-keys table))))
    (dolist (table-tags rpgtk-tables-tag-groups results)
      (let ((tag-list (thread-last table-tags
                                   ;; Drop numeric weight from each sublist
                                   (seq-map 'rest)
                                   (flatten-list))))
        (when (seq-contains-p tag-list tag)
          (setq results table-tags))))))

;; ----------------------------------------------------------------------
;; DICE TABLES
;; ----------------------------------------------------------------------
;;
;; A "dice table" is a table that is easy to manipulate with dice in
;; a game, and is pretty typical. The general idea is to roll one or
;; more specific dice, and compare the number in the first column to
;; see what the choice.
;;
;; For instance, Xanathar's Guide to Everything, a Dungeons and
;; Dragons supplement from Wizards of the Coast, allows you to
;; choose a random alignment with the following table:
;;
;;   | 3d6    | Alignment                                   |
;;   |--------|---------------------------------------------|
;;   | 3      | Chaotic evil (50%) or chaotic neutral (50%) |
;;   | 4--5   | Lawful evil                                 |
;;   | 6--8   | Neutral evil                                |
;;   | 9--12  | Neutral                                     |
;;   | 13--15 | Neutral good                                |
;;   | 16--17 | Lawful good (50%) or lawful neutral (50%)   |
;;   | 18     | Chaotic good (50%) or chaotic neutral (50%) |
;;
;; This would be render as a table with a range in the first column,
;; and each equally weighted choice in the rest of the columns:
;;
;;      Roll on Table: 3d6
;;
;;      |      3 | Chaotic evil | chaotic neutral |
;;      |   4--5 | Lawful evil  |                 |
;;      |   6--8 | Neutral evil |                 |
;;      |  9--12 | Neutral      |                 |
;;      | 13--15 | Neutral good |                 |
;;      | 16--17 | Lawful good  | lawful neutral  |
;;      |     18 | Chaotic good | chaotic neutral |
;;
;; Notice that we need to have a /dice expression/ to explain how to
;; arrive at a number for selecting a row.
;;
;; To represent these types of tables, we create a special type,
;; called a `rpgtk-tables-dice-obj'. Where the first "slot" is the
;; dice expression (or the number of sides of a dice to roll), and an
;; associative list of result values and the choice.

(cl-defstruct rpgtk-tables-dice-obj dice rows)

;; How is this used to render the example table above?
;;
;; (make-rpgtk-tables-dice-obj
;;   :dice "3d6"
;;   :rows '((3  . ("Chaotic evil" "Chaotic neutral"))
;;           (5  . "Lawful evil")
;;           (8  . "Neutral evil")
;;           (12 . "Neutral")
;;           (15 . "Neutral good")
;;           (17 . ("Lawful good" "Lawful neutral"))
;;           (18 . ("Chaotic good" "chaotic neutral"))))
;;
;; Couple things to notice about this rendering of the table. First,
;; we don't need a range, just the upper bound (for if we roll a 4, we
;; skip over the 3, we are below the next number, so we bugger off
;; with the answer).
;;
;; Second, a table row could have multiple choices. For instance, if
;; we were to roll a '3', we should flip a coin to choose between
;; /chaotic evil/ and /chaotic neutral/. In other words, if the value
;; of the row is a list, then we could just select from one of those
;; options.
;;
;; Let's do the fun part, and select an item from one of these
;; dice-tables. First, we grab the dice expression and the rows of the
;; table and put them into a couple of variables. We use a helper
;; function, `rpgdm-tables-dice--choose' to get the results of rolling
;; the dice expression

(defun rpgtk-tables--choose-dice-table (table)
  "Choose a string from a random dice table, TABLE."
  (let* ((roll (rpgtk-dice-roll-expression-sum (rpgtk-tables-dice-obj-dice table)))
         (rows (rpgtk-tables-dice-obj-rows table))
         (results (rpgtk-tables-dice--choose roll rows)))
    (if (stringp results)
        results
        (seq-random-elt results))))

;; If the results are not a single string item, we assume we have a
;; list sequence, and return one at random using `seq-random-elt'.

;; The helper function is recursive, as we can evaluate each /row/ to
;; see if it matches the dice roll:

(defun rpgtk-tables-dice--choose (roll rows)
  "Given a numeric ROLL, return row that matches.
This assumes ROWS is a sorted list where the first element (the
`car') is a numeric level that if ROLL is less than or equal, we
return the `rest' of the row. Otherwise, we recursively call this
function with the `rest' of the rows."
  (let* ((row (car rows))
         (level (car row))
         (answer (cdr row)))

    (if (<= roll level)
        answer
      (rpgtk-tables-dice--choose roll (cdr rows)))))

;; So, let's see it in action, by first assigning the dice-table
;; above, to a variable: `alignment-table':
;;
;;    (rpgdm-tables--choose-dice-table alignment-table)
;;
;; Would return:  Neutral good
;;
;; Nice. Now we just have to read and parse the table from an org-mode
;; file.
;;
;; Since I format my tables in different /styles/, I need to be able
;; to identify a /dice table/, I figured I would have a key word,
;; 'Roll on table' with a dice expression from `rpgdm-dice.el':

(defvar rpgtk-tables-dice-table-regexp
  (rx "Roll"
      (one-or-more space)
      (optional (or "on" "for"))
      (zero-or-more space)
      "Table:"
      (zero-or-more space)
      (group (regexp rpgtk-dice-roll-regexp)))
  "Regular expression for looking for the dice expression to use to roll.")

;; A predicate could return true when this regular expression returns
;; a valid response:

(defun rpgtk-tables-dice-table? ()
  "Return non-nil if current buffer has a dice-table."
  (goto-char (point-min))
  (re-search-forward rpgtk-tables-dice-table-regexp nil t))

(defvar rpgtk-tables-dice-table-rows
  (rx bol
      (zero-or-more space) "|" (zero-or-more space)
      (optional (one-or-more digit)
                (one-or-more "-"))
      (group
       (one-or-more digit))
      (zero-or-more space) "|" (zero-or-more space)
      (group (+? any))
      (zero-or-more space) "|" (zero-or-more space)
      eol)
  "Regular expression for parsing random tables.")

;; Assuming we just called that function, we can call `match-string'
;; to pick up that group and then parse the rest of the buffer as a
;; table:

(defun rpgtk-tables--parse-as-dice-table ()
  "Return `rpgtk-tables-dice-obj' of lines matching `rpgtk-tables-dice-table-rows'."
  (let ((dice (match-string-no-properties 1))         ; Grab expression before moving on
        (rows ())                                     ; Modify this with add-to-list
        (row-splitter (rx (* space) "|" (* space))))  ; Split rest of table row

    (while (re-search-forward rpgtk-tables-dice-table-rows nil t)
      (let* ((levelstr (match-string-no-properties 1))
             (level    (string-to-number levelstr))
             (row      (match-string-no-properties 2))
             (choices  (split-string row row-splitter t)))
        (push (cons level choices) rows)))
    (make-rpgtk-tables-dice-obj
     :dice dice
     :rows (sort rows (lambda (a b) (< (first a) (first b)))))))
(provide 'rpgtk-tables)
;;; rpgtk-tables.el ends here
