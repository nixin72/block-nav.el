(defun block-nav-move-block (dir original-column)
  (interactive)
  (forward-line dir)
  (back-to-indentation)
  (cond ((< original-column (current-column))
         (space-jump-move-block dir original-column))
        ((string-empty-p (current-line-contents))
         (space-jump-move-block dir original-column))))

(defun block-nav-next-block ()
  (interactive)
  (space-jump-move-block 1 (current-column)))

(defun block-nav-previous-block ()
  (interactive)
  (space-jump-move-block -1 (current-column)))

(provide 'block-nav)
