(defvar block-nav-center-after-scroll t)

(defun block-nav-move-block (dir original-column)
  "
  Moves the cursor to the beginning of the next line that shares the same level of indentation.
  When `dir` is positive, move to the next line, when `dir` is negative, move to the previous line.
  Original column should be the value of `(current-column)` when the function is initially called.
  "
  (interactive)
  (forward-line dir)
  (back-to-indentation)
  (cond ((< original-column (current-column))
         (block-nav-move-block dir original-column))
        ((string-empty-p (current-line-contents))
         (block-nav-move-block dir original-column))
        (t (when block-nav-center-after-scroll
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
  (cond ((= original-column (current-column))
         (block-nav-move-indentation-level dir original-column))
        (t (when block-nav-center-after-scroll
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
