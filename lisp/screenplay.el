;;; screenplay.el --- A major mode for editing screenplay files.

;; $Id$

;; Copyright (C) 2000, 2001, 2002, 2003  V. L. Simpson

;; Author: V. L. Simpson <vls@m-net.arbornet.org>
;; Keywords:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary: This is easy to use.
;;; TAB-return enters a scene heading; TAB-TAB-return sets up writing
;;; an action block and TAB-TAB-TAB-return puts one in a dialog
;;; writing mode.
;;; WARNING: This is massively incomplete.  Don't blame me if you
;;; don't win an Academy(TM) award using this thing.
;;; 
;; 

;;; Code:

;; Requires

;; Constants, customization and variables
(defconst screenplay-version "0.6.1"
  "Current Emacs Screenplay Mode version number.")

(defconst screenplay-bug-address
  "screenplay-bug@mail.freesoftware.fsf.org"
  "Bug reports for Screenplay Mode go here.
Used by 'screenplay-bug-report'.")

(defgroup screenplay nil
  "Screenplay editing."
  :group 'applications
  :link '(emacs-commentary-link :tag "Help" "screenplay"))

(defcustom screenplay-mode-hook 'auto-fill-mode
  "List of functions to call when entering Screenplay Mode."
  :type 'hook
  :group 'screenplay
  :options '(flyspell-mode))

(defcustom screenplay-action-left-margin 0
  "*Left margin for action block."
  :type 'integer
  :group 'screenplay)

(defcustom screenplay-action-fill-column 65
  "*Fill column for action block."
  :type 'integer
  :group 'screenplay)

(defvar screenplay-slugline-properties-list
  '(slugline t hard nil point-entered screenplay--point-entered-function)
  "Text properties for scene headings.")

(defvar screenplay-slugline-history ()
  "Scene heading history and completion list.")

(defvar screenplay-slugline-left-margin 0)

(defvar screenplay-character-history ()
  "Character name history and completion list.")

(defvar screenplay-dialog-fill-column 45)

(defvar screenplay-dialog-left-margin 15)

(defvar screenplay-dialog-char-name-column 25)

(defvar screenplay-last-command nil
  "Last executed screenplay editing command.")

(defvar screenplay-mode-abbrev-table nil
  "Abbrev table used while in screenplay mode.")

(define-abbrev-table 'screenplay-mode-abbrev-table ())

(defvar screenplay-mode-map nil
  "Key bindings for Screenplay Mode.")

(if screenplay-mode-map
    ()
  (setq screenplay-mode-map (make-sparse-keymap))
  (define-key screenplay-mode-map "\t\r" 'screenplay-insert-slugline)
  (define-key screenplay-mode-map "\t\t\r" 'screenplay-action-block)
  (define-key screenplay-mode-map "\t\t\t\r" 'screenplay-dialog-block))

(defun screenplay-mode ()
  "Major mode for editing screenplays.
Special commands: \\{screenplay-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map screenplay-mode-map)
  (setq local-abbrev-table screenplay-mode-abbrev-table)
  (setq major-mode 'screenplay-mode)
  (setq mode-name "Screenplay")
  (make-local-variable 'screenplay-action-left-margin)
  (make-local-variable 'screenplay-action-fill-column)
  (make-local-variable 'screenplay-dialog-char-name-column)
  (make-local-variable 'screenplay-dialog-fill-column)
  (make-local-variable 'screenplay-dialog-left-margin)
  (make-local-variable 'screenplay-character-history)
  (make-local-variable 'screenplay-slugline-history)
  (make-local-variable 'screenplay-slugline-properties-list)
  (make-local-variable 'screenplay-last-command)
  (add-hook 'pre-command-hook 'dialog-block-properties nil t)
;; FIXME: Add filling code.
;;  (add-hook 'after-change-functions 'screenplay-refill nil t)
  (run-hooks 'screenplay-mode-hook))

;;; Scene Headings

(defun screenplay--get-slugline ()
  "Get scene heading with history recall and TAB completion.
Called by `screenplay-insert-slugline'."
  (substitute-key-definition 'minibuffer-complete-word
                             'self-insert-command
                             minibuffer-local-completion-map)
  (if current-prefix-arg
      (let ((prompt (format "Enter a scene heading: ")))
        (completing-read prompt
                         screenplay-slugline-history
                         nil
                         nil
                         nil
                         screenplay-slugline-history))
    ;; FIXME: Need some way to accept a history list entry,
    ;; bypassing the 'input one element at time' rigamarole
    (let ((elt1 (read-string
                 "Enter first element (default: INT.): " nil nil "INT."))
          (elt2 (read-string "Enter location: "))
          (elt3 (read-string "Enter time: ")))
      (concat elt1 " " elt2 " -- " elt3))))

(set (make-local-variable 'slugline-properties) nil)

(defun screenplay-insert-slugline (slugline)
  "Insert SLUGLINE as a scene heading.

History list of previously entered elements is available with\\<minibuffer-local-map> \\[previous-history-element] and \\[next-history-element].

You will be prompted for scene heading elements in the
mini-buffer, eg., 'INT.' or 'EXT.', a location, eg., PARK and a time,
eg., MIDNIGHT.

With prefix argument you can add your own specific heading with TAB
completion with previously entered scene headings.

Capitalization isn't necessary as the function will handle this for
you."
  (interactive (list (screenplay--get-slugline)))
  (setq screenplay-last-command "screenplay-insert-slugline")
  (setq left-margin 0)
  (use-hard-newlines -1)
  (newline 2)
  (insert (upcase slugline))
  (newline 2)
  (slugline-properties)
  (setq slugline-properties t)
  (setq screenplay-slugline-history
        (cons (upcase slugline)  screenplay-slugline-history))
  (define-key minibuffer-local-completion-map [32]
    'minibuffer-complete-word))

;; Set slugline text properties
(defun slugline-properties ()
  "Set scene heading text properties."
      (forward-line -2)
      (beginning-of-line)
      (setq m1 (point-marker))
      (end-of-line)
      (setq m2 (point-marker))
      (add-text-properties m1 m2 screenplay-slugline-properties-list))

;;; Action Block

(defun screenplay-action-margins ()
  "Set left|right margins for action block."
  (setq left-margin screenplay-action-left-margin
        fill-column screenplay-action-fill-column))

(defun screenplay-action-block ()
  "Edit a description block."
  (interactive)
  (setq screenplay-last-command "screenplay-action-block")
  (use-hard-newlines -1)
  (screenplay-action-margins)
  (newline 2))

(defun action--block-properties ()
  "Set text properties on action block.")

;;; Dialog Block
(defun screenplay--dialog-character-name ()
  "Get character name for dialog block and add to history list."
  (substitute-key-definition 'minibuffer-complete-word
                             'self-insert-command
                             minibuffer-local-completion-map)
  (let ((prompt (format "Enter character name: ")))
    (completing-read prompt
                     screenplay-character-history
                     nil
                     nil
                     nil
                     screenplay-character-history)))

(defun screenplay-dialog-block (name)
  "Edit a dialog block.
TAB gives completion Argument NAME inserted auto-capitalized."
  (interactive
   (list (screenplay--dialog-character-name)))
  (setq screenplay-last-command "screenplay-dialog-block")
  (setq left-margin screenplay-dialog-char-name-column)
  (use-hard-newlines 1 t) ;Need this to keep auto-fill from screwing up.
  (newline 2)
  (insert (upcase name))
  (setq screenplay-character-history (cons name screenplay-character-history))
  (setq fill-column screenplay-dialog-fill-column
        left-margin screenplay-dialog-left-margin)
  (newline)
  (dialog--char-name-properties)
  (setq dialog-char-properties t)
  (define-key minibuffer-local-completion-map [32] 'minibuffer-complete-word))

(defun dialog--char-name-properties ()
  "Set properties on dialog block character name."
  (save-excursion
    (forward-line -1)
    (end-of-line)
    (setq m2 (point-marker))
    (backward-word 1)
    (setq m1 (point-marker))
    (add-text-properties m1 m2 '(dialog-char-name t hard t ))))

(defun dialog-block-properties ()
  "Set text properties on dialog block."
  (save-excursion
   (if (equal screenplay-last-command "screenplay-dialog-block")
       (progn
         (setq screenplay-dialog-block nil
               (re-search-backward "\\([A-Z][A-Z ]+\\)\\([A-Z]+\\)?")
               (forward-line 1))))))

;;; Internal functions
;; Handle moving around screenplay buffer.
(defun screenplay--point-entered-function (&optional old-point new-point)
  "Scan for screenplay element properties and adjust accordingly."
  (cond
   ;; Scene heading
   ((get-text-property (point) 'slugline)
    (if slugline-properties
        nil
      (progn
        (setq slugline-properties nil)
        (setq left-margin screenplay-slugline-left-margin))))
   ;; dialog block name
   ((get-text-property (point) 'dialog-char-name)
    (if dialog-char-properties
        nil
      (progn
        (setq dialog-char-properties nil)
        (setq left-margin 25)
        (use-hard-newlines 1 t))))))

;; FIXME: I may not need this.
(defun screenplay--point-left-function (&optional old-point new-point)
  "Adjust point-entered function.
Based on text property value of previous point position.")
 
  
(defun screenplay-version ()
  "Display current program version in echo area."
  (interactive)
  (message "Screenplay Mode Version %s" screenplay-version))

(defun screenplay-submit-bug-report ()
  "Submit a bug report for Screenplay Mode."
  (interactive)
  (require 'reporter)
  (reporter-submit-bug-report
   screenplay-bug-address
   (concat "screenplay-" screenplay-version)
   nil
   nil
   nil
   "Please make your report as detailed as possible.
I'll try to fix it as soon as possible.

Thanks,
vls
Emacs Screenplay Mode
http://fs.fsf.org/screenplay"))

(provide 'screenplay)
;;; screenplay.el ends here
