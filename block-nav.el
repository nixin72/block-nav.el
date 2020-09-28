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

(defvar block-nav-center-after-scroll t)

(defun block-nav-move-block (dir original-column)
  (interactive)
  (forward-line dir)
  (back-to-indentation)
  (cond ((< original-column (current-column))
         (block-nav-move-block dir original-column))
        ((string-empty-p (buffer-substring (line-beginning-position) (line-end-position)))
         (block-nav-move-block dir original-column))
        (t (when block-nav-center-after-scroll
             (recenter)))))

(defun block-nav-next-block ()
  (interactive)
  (block-nav-move-block 1 (current-column)))

(defun block-nav-previous-block ()
  (interactive)
  (block-nav-move-block -1 (current-column)))

(defun block-nav-move-indentation-level (dir original-column)
  (interactive)
  (forward-line dir)
  (back-to-indentation)
  (cond ((= original-column (current-column))
         (block-nav-move-indentation-level dir original-column))
        (t (when block-nav-center-after-scroll
             (recenter))))) 
  
(defun block-nav-next-indentation-level ()
  (interactive)
  (block-nav-move-indentation-level 1 (current-column)))

(defun block-nav-previous-indentation-level ()
  (interactive)
  (block-nav-move-indentation-level -1 (current-column)))

(provide 'block-nav)
