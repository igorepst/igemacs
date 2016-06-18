(defvar ig-ivy-pages--page-delimiter "^\014"
  "Regexp describing line-beginnings that separate pages.")

(defun ig-ivy-pages-function ()
  (let ((ig-ivy-pages-list ()))
    (save-excursion
      (goto-char (point-min))
      (save-restriction
	(if (and (save-excursion
		   (re-search-forward ig-ivy-pages--page-delimiter nil t))
		 (= 1 (match-beginning 0)))
	    (goto-char (match-end 0)))
	(push (ig-ivy-pages-copy-header-and-position) ig-ivy-pages-list)

	(while (re-search-forward ig-ivy-pages--page-delimiter nil t)
	  (push (ig-ivy-pages-copy-header-and-position) ig-ivy-pages-list))))
    (nreverse ig-ivy-pages-list)))

(defun ig-ivy-pages-copy-header-and-position ()
  "Copy page header and its position to list."

  (skip-chars-forward " \t\n")
  (let* ((start (point))
	 (end (line-end-position))
	 (substr (buffer-substring start end)))
    (cons substr start)))

(defun ig-ivy-pages ()
  (interactive)
  (ivy-read
   (funcall counsel-prompt-function "Pages")
   (ig-ivy-pages-function)
   :action (lambda (x)
	     (goto-char x)
	     (recenter-top-bottom 0))))


(provide 'ig-ivy-pages)
