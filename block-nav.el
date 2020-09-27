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
