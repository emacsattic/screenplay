;;; screenplay.el --- A major mode for editing screenplay files.

;; $Id$

;; Copyright (C) 2000, 2001, 2002, 2003, 2004  Vance L. Simpson

;; Author: V. L. Simpson <vls@freeshell.org>
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

;;; Commentary:
;; 
;; Massive complete start-over re-write back-to-the-drawing-board
;; start-from-square-one bottom-of-the-barrel-looking-up release.
;;
;; Installing and using screenplay.el:
;; Put this file somewhere on your emacs load-path.
;; Load the file with 'load-libray RET screenplay RET'.
;; Open up your Academy Award Winner(TM), M-x screenplay-mode and have
;; at it.
;; The TAB and RET keys let you insert and edit the basic screenplay
;; elements.
;;
;; TAB-RET asks for and inserts a scene heading, e.g., INT. HOUSE -- DAY.
;;
;; TAB-TAB-RET moves into action block mode, e.g., Describing the
;; house exploding.
;;
;; TAB-TAB-TAB-RET does the dialog thing, e.g.,
;;         BOB
;;   Gee, the house just
;;   exploded.
;;
;; Bugs and caveats:
;;
;; There is no way to go back and re-edit something according to it's
;; screenplay element function.  I'm working on it.  So get it right
;; the first time. 8-]
;;
;; Don't enter any spurious newlines when finished editing any one
;; particular element.  Just hit the key combo for the next thing you
;; want to do, e.g., INT. HOUSE -- DAY(Type TAB-TAB-RET to go into an
;; action block.
;;

;; Remember kiddies:
;; "Nobody knows anything" -- William Goldman _Adventures in the Screentrade_

;;; Code:

(defconst screenplay-version "0.7.0"
  "Current Emacs Screenplay Mode version number.")
(defconst screenplay-author-name  "V. L. Simpson")
(defconst screenplay-author-email "vls@freeshell.org")
(defconst screenplay-web-page     "http://www.nongnu.org/screenplay/")
(defconst screenplay-bug-address
  "screenplay-bug@mail.freesoftware.fsf.org"
  "Bug reports for Screenplay Mode go here.")

(defgroup screenplay nil
  "Screenplay editing."
  :group 'applications
  :link '(emacs-commentary-link :tag "Help" "screenplay"))

(defcustom screenplay-mode-hook 'auto-fill-mode
  "List of functions to call when entering Screenplay Mode."
  :type 'hook
  :group 'screenplay)

(defcustom screenplay-left-margin 0
  "Left margin for scene headings and action blocks" 
  :type 'integer
  :group 'screenplay)

(defcustom screenplay-right-margin 50
  "Right margin for scene-headings and action blocks"
  :type 'integer
  :group 'screenplay)

;; I'll give internal var's a 'scrn' prefix.
(defvar scrn-scene-hist ()
  "History list for scene headings.")

(defvar scrn-dialog-name-hist ())

(define-derived-mode screenplay-mode fundamental-mode "Screenplay"
  "Major mode for editing screenplays.
\\{screenplay-mode-map}"
;; Try some kind of command rotation scheme with just tab and enter.
  (define-key screenplay-mode-map "\t\r" 'screenplay-slugline)
  (define-key screenplay-mode-map "\t\t\r" 'screenplay-action-block)
  (define-key screenplay-mode-map "\t\t\t\r" 'screenplay-dialog-block)
  (make-local-variable 'scrn-scene-hist)
  (make-local-variable 'screenplay-right-margin)
  (make-local-variable 'screenplay-left-margin)
  (make-local-variable 'scrn-dialog-name-hist)
  )

(defun screenplay-read-slugline ()
  "Get scene heading.
Returns scene heading in upper-case format."
  (let ((scene-heading 
         (let ((prompt "Enter scene heading: "))
           (read-from-minibuffer prompt 
                                 nil           ;initial-contents
                                 nil           ;keymap
                                 nil           ;read
                                 'scrn-scene-hist   ;hist
                                 nil           ;default
                                 nil))))       ;inherit-input-method
    (upcase scene-heading)))

;; FIXME: Try prefix arg to set margin and fill-column for re-editing
;; a pre-existing element.  
(defun screenplay-slugline (scene)
  (interactive (list (screenplay-read-slugline)))
  (newline 2)
  (setq left-margin 0)
  (indent-to-left-margin)
  (insert scene))

(defun screenplay-action-block ()
  "Edit a description block."
  (interactive)
  (newline 2)
  (setq left-margin 0)
  (setq fill-column 50)
  (use-hard-newlines -1)
  (indent-to-left-margin))

(defun screenplay-dialog-char-name ()
  (let ((char-name
         (let ((prompt "Enter character name: "))
           (read-from-minibuffer prompt
                                 nil
                                 nil
                                 nil
                                 'scrn-dialog-name-hist
                                 nil
                                 nil))))
    (upcase char-name)))

(defun screenplay-dialog-block (name)
  (interactive (list (screenplay-dialog-char-name)))
  (use-hard-newlines 1 t)
  (newline 2)
  (setq left-margin 20)
  (indent-to-left-margin)
  (insert name)
  (newline 1)
  (setq left-margin 10)
  (indent-to-left-margin)
  (setq fill-column 40))

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
http://www.nongnu.org/screenplay/"))



(provide 'screenplay)
;;; screenplay.el ends here
