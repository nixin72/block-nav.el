;;; block-nav.el --- Block level navigation for quick jumping -*- lexical-binding: t; -*-
;;
;; Copyright © 2020 Philip Dumaresq
;;
;; Authors: Philip Dumaresq <phdumaresq@protonmail.com>
;; Maintainer: Philip Dumaresq <phdumaresq@protonmail.com>
;; URL: http://github.com/nixin72/block-nav.el
;; Keywords: navigation
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; License:
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;; Provides a number of interactive functions for easily navigating
;; between blocks of code at the same level of indentation.
;;
;; This package does not bind any keys for you. 
;; Here are some example bindings for evil-mode:
;;
;;   (define-key evil-motion-state-map "H" 'block-nav-previous-indentation-level)
;;   (define-key evil-motion-state-map "J" 'block-nav-next-block)
;;   (define-key evil-motion-state-map "K" 'block-nav-previous-block)
;;   (define-key evil-motion-state-map "L" 'block-nav-next-indentation-level)
;;
;;   ;; Although this may not be desireable since it overrides vim keys you may use.
;;
;;; Code:

(defvar block-nav-center-after-scroll nil
  "When not-nil, Emacs will recenter the current line after moving")
(defvar block-nav-move-skip-shallower nil
  "
  When not-nil, calling `block-nav-next/previous-block` will 
  skip over lines with a shallower indentation than the current line.
  ")

(defun block-nav-move-block (dir original-column)
  "
  Moves the cursor to the beginning of the next line that shares the same level of indentation.
  When `dir` is positive, move to the next line, when `dir` is negative, move to the previous line.
  Original column should be the value of `(current-column)` when the function is initially called.
  "
  (interactive)
  (forward-line dir)
  (back-to-indentation)
  (cond
   ;; When line is empty, skip it
   ((string-empty-p (buffer-substring (line-beginning-position) (line-end-position)))
    (block-nav-move-block dir original-column))
   ;; When line is shallower, skip it if enabled
   ((> original-column (current-column))
    (when block-nav-move-skip-shallower
      (block-nav-move-block dir original-column)))
   ;; When line is deeper, skip it
   ((< original-column (current-column))
    (block-nav-move-block dir original-column))
   ;; Otherwise, stop at that line
   (t
    (when block-nav-center-after-scroll
      (recenter)))))

(defun block-nav-next-block ()
  "
  Calls `block-nav-move-block` with 
  the arguments necessary to go to the next block.
  "
  (interactive)
  (block-nav-move-block 1 (current-column)))

(defun block-nav-previous-block ()
  "
  Calls `block-nav-move-block` with 
  the arguments necessary to go to the previous block.
  "
  (interactive)
  (block-nav-move-block -1 (current-column)))

(defun block-nav-move-indentation-level (dir original-column)
  "
  Moves the cursor to the start of the next line that has a deeper/shallower level of indentation.
  When `dir` is positive, move to the next line, when `dir` is negative, move to the previous line.
  Original column should be the value of `(current-column)` when the function is initially called.
  "
  (interactive)
  (forward-line dir)
  (back-to-indentation)
  (cond
   ((= original-column (current-column))
    (block-nav-move-indentation-level dir original-column))
   (t
    (when block-nav-center-after-scroll
      (recenter))))) 
  
(defun block-nav-next-indentation-level ()
  "
  Calls `block-nav-move-indentation-level` with 
  the arguments necessary to go deeper in indentation.
  "
  (interactive)
  (block-nav-move-indentation-level 1 (current-column)))

(defun block-nav-previous-indentation-level ()
  "
  Calls `block-nav-move-indentation-level` with 
  the arguments necessary to go back shallower in indentation.
  "
  (interactive)
  (block-nav-move-indentation-level -1 (current-column)))

(provide 'block-nav)
