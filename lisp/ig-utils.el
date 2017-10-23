;;; -*- lexical-binding: t -*-

;;;###autoload
(defun ig-convert-eol()
  "Convert EOL between Windows, Unix and MAC."
  (interactive)
  (let* ((ccs buffer-file-coding-system)
	 (ccstr (symbol-name ccs))
	 (converts '())
	 (push-c (lambda (s)
		   (push `("Mac" . ,(concat s "-mac")) converts)
		   (push `("Windows" . ,(concat s "-dos")) converts)
		   (push `("Unix" . ,(concat s "-unix")) converts))))
    (if (string-match "-\\(?:unix\\|dos\\|mac\\)$" ccstr)
    	(progn
    	  (setq ccstr (substring ccstr 0 (match-beginning 0)))
	  (funcall push-c ccstr)
    	  (setq ccstr (symbol-name ccs)))
      (progn
	(funcall push-c ccstr)
    	(pcase system-type 
    	  ((or 'windows-nt 'ms-dos) (setq ccstr (concat ccstr "-[dos]")))
    	  ('darwin (setq ccstr (concat ccstr "-[mac]")))
    	  (_ (setq ccstr (concat ccstr "-[unix]"))))))    
    (setq ccstr (cdr (assoc (completing-read (concat "Set buffer coding system from " ccstr " to: ") converts nil t) converts))) 
    (set-buffer-file-coding-system (intern ccstr))))


(provide 'ig-utils)
