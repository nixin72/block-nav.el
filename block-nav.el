;;; block-nav.el --- Jump across indentation levels for quick navigation -*- lexical-binding: t; -*-
;;
;; Copyright © 2020 Philip Dumaresq
;;
;; Authors: Philip Dumaresq <phdumaresq@protonmail.com>
;; Maintainer: Philip Dumaresq <phdumaresq@protonmail.com>
;; URL: http://github.com/nixin72/block-nav.el
;; Keywords: convenience
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
;;   ;; Although this may not be desirable since it overrides vim keys you may use.
;;
;;; Code:

(require 'subr-x)

(defvar block-nav-center-after-scroll nil
  "When not-nil, Emacs will recenter the current line after moving.")
(defvar block-nav-move-skip-shallower t
  "When not-nil, calling `block-nav-next/previous-block` will skip over lines with a shallower indentation than the current line.")

;;; Helper functions

(defmacro block-nav-do-while (cond &rest body)
  "Run the BODY once, then run it again in a while loop with the COND."
  `(progn
     (progn . ,body)
     (while ,cond
       (progn . ,body))))

(defun block-nav-test-end-of-space (dir)
  "Return non-nil if either of the following are true:
DIR is greater than 0 and you're at the last line of the file.
DIR is less than 0 and you're at the first line of the file."
  (or
   (and (> dir 0)
        (<= (count-lines (point-min) (point-max))
            (line-number-at-pos)))
   (and (< dir 0)
        (= 1 (line-number-at-pos)))))

(defun block-nav-do-move (line-count)
  "Given a LINE-COUNT, will move forwards/backwards that many lines.
It will then place the cursor at the first non-whitespace character.
If `block-nav-center-after-scroll' is non-nil, it will recenter the current line"
  (forward-line line-count)
  (back-to-indentation)
  (when block-nav-center-after-scroll
    (recenter)))

;;; Core functions

(defun block-nav-move-block (dir)
  "Move to the next or previous block with the same level of indentation.
When DIR is positive, move to the next line.
When DIR is negative, move to the previous line."
  (save-excursion
   (catch 'reached-end-of-file
    (let ((line-count 0)
          (original-column (current-column)))
      (block-nav-do-while (or (if block-nav-move-skip-shallower
                                  (/= original-column (current-column))
                                  (< original-column (current-column)))
                              (string-empty-p (buffer-substring
                                               (line-beginning-position)
                                               (line-end-position))))
        (when (block-nav-test-end-of-space dir)
          (message "Reached last block of this indentation.")
          (throw 'reached-end-of-file 0))
        (forward-line dir)
        (back-to-indentation)
        (setf line-count (+ 1 line-count)))
      line-count))))

(defun block-nav-move-indentation-level (dir)
  "Move to a block that has a deeper/shallower level of indentation.
When DIR is positive, move to the next line with deeper indentation.
When DIR is negative, move to the previous line with shallower indentation."
  (save-excursion
   (catch 'reached-end-of-file
    (let ((line-count 0)
          (original-column (current-column)))
      (block-nav-do-while (or
                           (and (> dir 0)
                                (>= original-column (current-column)))
                           (and (< dir 0)
                                (<= original-column (current-column)))
                           (string-empty-p (buffer-substring
                                            (line-beginning-position)
                                            (line-end-position))))
        (when (block-nav-test-end-of-space dir)
          (if (> dir 0)
              (message "Deepest indentation reached")
              (message "Shallowest indentation reached"))
          ;; If line is empty, move to non-empty line.
          (throw 'reached-end-of-file 0))
        (forward-line dir)
        (back-to-indentation)
        (setf line-count (+ dir line-count)))
      line-count))))

;;; Interactive functions to wrap functionality

(defun block-nav-next-block ()
  "Move the cursor to the next line that shares the same level of indentation."
  (interactive)
  (block-nav-do-move (block-nav-move-block 1)))

(defun block-nav-previous-block ()
  "Move the cursor to previous line that shares the same level of indentation."
  (interactive)
  (block-nav-do-move (* (block-nav-move-block -1) -1)))

(defun block-nav-next-indentation-level ()
  "Move the cursor to the next line that has a deeper level of indentation."
  (interactive)
  (block-nav-do-move (block-nav-move-indentation-level 1)))

(defun block-nav-previous-indentation-level ()
  "Move the cursor to the previous line that has shallower level of indentation."
  (interactive)
  (block-nav-do-move (block-nav-move-indentation-level -1)))

(provide 'block-nav)

;;; block-nav.el ends here
