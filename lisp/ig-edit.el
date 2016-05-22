;;; ig-edit.el --- Editing functions

;;; Commentary:
;; Various helpers for editing.

;;; Code:



;; Duplicate current line or region
;;;###autoload
(defun ig-get-bounds-lines-rect (&optional buf)
  "Get bounds of region, line or buffer for BUF."
  (cond ((use-region-p)
	 (let ((start (point)) (end (mark)))
	   (when (> start end)
	     (setq start (prog1 end (setq end start)))
	     (goto-char start))
	   (setq start (line-beginning-position))
	   (goto-char end)
	   (setq end (line-end-position))
	   (cons start end)))
	(buf (cons (point-min) (point-max)))
	(t (cons (line-beginning-position) (line-end-position)))))

;; https://github.com/bbatsov/prelude/blob/master/core/prelude-core.el
(defun prelude-duplicate-current-line-or-region (arg)
  "Duplicate the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "*p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (ig-get-bounds-lines-rect))
               (region (buffer-substring-no-properties beg end)))
    (-dotimes arg
      (lambda (n)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))))
    (goto-char (+ origin (* (length region) arg) arg))))



;; Comment or uncomment lines or region
;; TODO: arg????
(defun ig-comment-lines ()
  "(Un-)Comment current line or lines of region."
  (interactive "*")
  (let  ((reg (ig-get-bounds-lines-rect)))
    (comment-or-uncomment-region (car reg) (cdr reg))))



;; Delete current line or lines of region
;; TODO: arg????
(defun ig-delete-current-line-or-region ()
  "Deletes the current line or lines of region."
  (interactive "*")
  (let ((reg (ig-get-bounds-lines-rect)))
    (kill-region (car reg) (1+ (min (cdr reg) (buffer-size))))))



;; Kill region or line
;; https://github.com/bbatsov/prelude/blob/master/core/prelude-editor.el
;;;###autoload
(defun ig-kill-region-or-line (beg end &optional region)
  "When called interactively with no active region, kill a single line instead.
Argument BEG beginning position.
Argument END end position.
Optional argument REGION optional region argument."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))



;; Text size increase or decrease
;;;###autoload
(defun ig-text-scale-increase-all-buffers (orig-fun &rest args)
  "Apply zoom to every buffer, and not to the current one only.
Argument ORIG-FUN original function.
Optional argument ARGS additional arguments."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (apply orig-fun args))))

(advice-add 'text-scale-increase :around #'ig-text-scale-increase-all-buffers)

(defun ig-font-inc (arg)
  "Increase font size.
Argument ARG scale."
  (interactive "p") (text-scale-increase arg))

(defun ig-font-dec (arg)
  "Decrease font size.
Argument ARG scale."
  (interactive "p") (text-scale-increase (- arg)))

(defun ig-font-restore ()
  "Restore font size to default."
  (interactive) (text-scale-increase 0))



;; Get major mode
;;;###autoload
(defun ig-buffer-mode (buffer)
  "Get the major mode of the BUFFER."
  (buffer-local-value 'major-mode (get-buffer buffer)))



;; Indent region or buffer
(defun ig-indent-region-or-buffer ()
  "Indent a selected region or a buffer."
  (interactive "*")
  (save-excursion (funcall ig-indent-command))
  (if buffer-file-name (save-buffer))
  (message "Indented region or buffer"))



;; Wrap isearch
;; http://stackoverflow.com/a/287067/407953
;;;###autoload
(defadvice isearch-search (after isearch-no-fail activate)
  "Wraps isearch automatically."
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))



;; Helpers to delete/sort/reverse lines
;;;###autoload
(defun ig-delete-matching-lines (regexp)
  "Delete lines matching REGEXP in region or buffer."
  (interactive "*MFlush lines containing match for regexp: ")
  (let ((bounds (ig-get-bounds-lines-rect t)))
    (flush-lines regexp (car bounds) (cdr bounds) t)))

;;;###autoload
(defun ig-delete-non-matching-lines (regexp)
  "Keep only lines matching REGEXP in region or buffer."
  (interactive "*MKeep lines containing match for regexp: ")
  (let ((bounds (ig-get-bounds-lines-rect t)))
    (keep-lines regexp (car bounds) (cdr bounds) t)))

;;;###autoload
(defun ig-delete-adjacent-duplicate-lines ()
  "Delete adjacent duplicate lines in region or buffer."
  (interactive "*")
  (let ((bounds (ig-get-bounds-lines-rect t)))
    (delete-duplicate-lines (car bounds) (cdr bounds) nil t nil t)))

;;;###autoload
(defun ig-sort-lines (&optional reverse)
  "Sort lines in region or buffer.
Optional argument REVERSE whether to reverse the sort."
  (interactive "*")
  (let ((bounds (ig-get-bounds-lines-rect t)))
    (sort-lines reverse (car bounds) (cdr bounds))))

;;;###autoload
(defun ig-reverse-region ()
  "Reverse lines in region or buffer."
  (interactive "*")
  (let ((bounds (ig-get-bounds-lines-rect t)))
    (reverse-region (car bounds) (cdr bounds))))


(provide 'ig-edit)

;;; ig-edit.el ends here
