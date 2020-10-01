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
(defvar block-nav-move-skip-shallower t
  "
  When not-nil, calling `block-nav-next/previous-block` will 
  skip over lines with a shallower indentation than the current line.
  ")

(defmacro do-while (cond &rest body)
  "Runs the body once, then runs it again in a while loop with the condition."
  `(progn
     (progn . ,body)
     (while ,cond
       (progn . ,body))))

(defun test-end-of-space (dir)
  "
  Returns true if the current line is either the first line in the file
  or if the current line is the last line in the file or beyond the last line
  "
  (or
   (and (> dir 0)
        (<= (count-lines (point-min) (point-max))
            (line-number-at-pos)))
   (and (< dir 0)
        (= 1 (line-number-at-pos)))))

(defun block-nav-move-block (dir original-column)
  "
  Moves the cursor to the beginning of the next line that shares the same level of indentation.
  When `dir` is positive, move to the next line, when `dir` is negative, move to the previous line.
  Original column should be the value of `(current-column)` when the function is initially called.
  "
  (interactive)
  (catch 'reached-end-of-file
   (let ((line-count 0))
     (do-while (or (if block-nav-move-skip-shallower
                       (/= original-column (current-column))
                       (< original-column (current-column)))
                   (string-empty-p (buffer-substring
                                    (line-beginning-position)
                                    (line-end-position))))
       (when (test-end-of-space dir)
         (message "Reached last block of this indentation.")
         (throw 'reached-end-of-file 0))
       (forward-line dir)
       (back-to-indentation)
       (setf line-count (+ 1 line-count)))
     line-count)))

(defun finish-move (line-count)
  "
  Will take in a number of lines to move, then will jump forward/backwards
  that many lines, and jump to the first non-whitespace character.
  If block-nav-center-after-scroll is non-nil, then it will also recenter the
  current line in the middle of the window.
  "
  (forward-line line-count)
  (back-to-indentation)
  (when block-nav-center-after-scroll
    (recenter)))

(defun block-nav-next-block ()
  "
  Calls `block-nav-move-block` with 
  the arguments necessary to go to the next block.
  "
  (interactive)
  (let ((move-lines
         (save-excursion
           (block-nav-move-block 1 (current-column)))))
    (finish-move move-lines)))

(defun block-nav-previous-block ()
  "
  Calls `block-nav-move-block` with 
  the arguments necessary to go to the previous block.
  "
  (interactive)
  (let ((move-lines
         (save-excursion
           (block-nav-move-block -1 (current-column)))))
    (finish-move (* move-lines -1))))

(defun block-nav-move-indentation-level (dir original-column)
  "
  Moves the cursor to the start of the next line that has a deeper/shallower level of indentation.
  When `dir` is positive, move to the next line, when `dir` is negative, move to the previous line.
  Original column should be the value of `(current-column)` when the function is initially called.
  "
  (interactive)
  (catch 'reached-end-of-file
   (let ((line-count 0))
     (do-while (or
                (and (> dir 0)
                     (>= original-column (current-column)))
                (and (< dir 0)
                     (<= original-column (current-column))))
       (when (test-end-of-space dir)
         (if (> dir 0)
             (message "Deepest indentation reached")
             (message "Shallowest indentation reached"))
         (throw 'reached-end-of-file 0))
       (forward-line dir)
       (back-to-indentation)
       (setf line-count (+ dir line-count)))
     line-count)))

(defun block-nav-next-indentation-level ()
  "
  Calls `block-nav-move-indentation-level` with 
  the arguments necessary to go deeper in indentation.
  "
  (interactive)
  (let ((move-lines
         (save-excursion
           (block-nav-move-indentation-level 1 (current-column)))))
    (finish-move move-lines)))

(defun block-nav-previous-indentation-level ()
  "
  Calls `block-nav-move-indentation-level` with 
  the arguments necessary to go back shallower in indentation.
  "
  (interactive)
  (let ((move-lines
         (save-excursion
           (block-nav-move-indentation-level -1 (current-column)))))
    (finish-move move-lines)))

(provide 'block-nav)
