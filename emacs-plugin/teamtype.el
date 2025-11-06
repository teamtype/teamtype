(defun pos-to-line-col (pos)
  (save-excursion
    (goto-char pos)
    (cons (- (line-number-at-pos) 1) (- pos (line-beginning-position)))))

(defun line-col-to-pos (line col)
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (+ (line-beginning-position) col)))

(defvar my-last-change-old-text nil)

; Note: beg and end are 1-indexed "character positions"! Not sure yet what exactly that means.
(defun my-before-change (beg end)
  (setq my-last-change-old-text (buffer-substring-no-properties beg end)))

(defun my-after-change (beg end len)
  (message "Replaced '%s' with '%s' at position %d (%s)"
           my-last-change-old-text
           (buffer-substring-no-properties beg end)
           beg
           (pos-to-line-col beg)
           )
  (replace-region 0 0 0 0 "!"))

(defun replace-region (line1 col1 line2 col2 text)
  (let ((beg (line-col-to-pos line1 col1))
        (end (line-col-to-pos line2 col2)))
    (let ((inhibit-modification-hooks t))
      (save-excursion
        (delete-region beg end)
        (goto-char beg)
        (insert text)))))

(add-hook 'before-change-functions #'my-before-change)
(add-hook 'after-change-functions #'my-after-change)

(provide 'teamtype)
