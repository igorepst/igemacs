(defvar ig-counsel-pages--page-delimiter "^\014"
  "Regexp describing line-beginnings that separate pages.")

(defun ig-counsel-pages-function ()
  "Build list of pages and their positions."
  (let ((ig-counsel-pages-list ()))
    (save-excursion
      (goto-char (point-min))
      (save-restriction
	(if (and (save-excursion
		   (re-search-forward ig-counsel-pages--page-delimiter nil t))
		 (= 1 (match-beginning 0)))
	    (goto-char (match-end 0)))
	(push (ig-counsel-pages-get-header-and-position) ig-counsel-pages-list)

	(while (re-search-forward ig-counsel-pages--page-delimiter nil t)
	  (push (ig-counsel-pages-get-header-and-position) ig-counsel-pages-list))))
    (nreverse ig-counsel-pages-list)))

(defun ig-counsel-pages-get-header-and-position ()
  "Get page header and its position."
  (skip-chars-forward " \t\n")
  (let* ((start (point))
	 (end (line-end-position))
	 (substr (buffer-substring start end)))
    (cons substr start)))

(defun ig-counsel-pages ()
  "Select buffer's pages via `counsel'."
  (interactive)
  (ivy-read "Pages: "
   (ig-counsel-pages-function)
   :action (lambda (x)
	     (goto-char x)
	     (recenter-top-bottom 0))
   :require-match t
   :caller 'ig-counsel-pages))


(provide 'ig-counsel-pages)
