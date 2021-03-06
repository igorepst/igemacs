;;; ig-org.el --- Init Org mode

;;; Commentary:
;; Configuration of Org

;;; Code:

(defun ig-setup-org-headings(frame)
  "Make org headers larger, instead of different colors, on graphical displays only."
  (when (display-graphic-p frame)
    (let* ((variable-tuple (cond
			    ((x-list-fonts "DejaVu Sans Mono") '(:font "DejaVu Sans Mono"))
			    ((x-family-fonts "Sans Serif") '(:family "Sans Serif"))
			    (nil (warn "Cannot find a suitable font"))))
	   (base-font-color     (face-foreground 'default nil 'default))
	   (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

      (custom-theme-set-faces 'user
			      `(org-verbatim ((t (:foreground "deep sky blue"))))

			      `(org-level-8 ((t (,@headline ,@variable-tuple))))
			      `(org-level-7 ((t (,@headline ,@variable-tuple))))
			      `(org-level-6 ((t (,@headline ,@variable-tuple))))
			      `(org-level-5 ((t (,@headline ,@variable-tuple))))
			      `(org-level-4 ((t (,@headline ,@variable-tuple))))
			      `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.1))))
			      `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.2))))
			      `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.3))))
			      `(org-document-title ((t (,@headline ,@variable-tuple :height 1.3 :underline nil))))))))

(defun ig-add-nbsp-to-org-emphasis ()
  "Add non breaking space to `org-emphasis-regexp-components'.
Used with pandoc, but shouldn't hurt anyway."
  ;; http://www.xiangji.me/2015/07/13/a-few-of-my-org-mode-customizations/
  (let ((ig-nbsp (string ?\u00A0)) (ig-nbsp-added-p nil)
	(ig-nbsp-cand1 (car org-emphasis-regexp-components))
	(ig-nbsp-cand2 (car (nthcdr 1 org-emphasis-regexp-components))))
    (unless (string-match-p ig-nbsp ig-nbsp-cand1)
      (setcar org-emphasis-regexp-components (concat ig-nbsp-cand1 ig-nbsp))
      (setq ig-nbsp-added-p t))
    (unless (string-match-p ig-nbsp ig-nbsp-cand2)
      (setcar (nthcdr 1 org-emphasis-regexp-components) (concat ig-nbsp-cand2 ig-nbsp))
      (setq ig-nbsp-added-p t))
    (when ig-nbsp-added-p
      ;; http://permalink.gmane.org/gmane.emacs.orgmode/82672
      (custom-set-variables `(org-emphasis-alist ',org-emphasis-alist))
      ;; https://github.com/xiaohanyu/oh-my-emacs/blob/master/core/ome-org.org#code-block-fontification
      (require 'org-element)
      (org-element--set-regexps))))

;; http://emacs.stackexchange.com/a/12124/2477
(defun ig-yank-html-to-org ()
  "Convert clipboard contents from HTML to Org and yank."
  (interactive)
  (when (and (executable-find "xclip") (executable-find "pandoc"))
    (kill-new (shell-command-to-string
	       "xclip -o -t text/html | pandoc -f html -t json | pandoc -f json -t org --no-wrap")))
  (yank))

;; http://orgmode.org
;; Parts from http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html
(use-package org
  :defer t
  :diminish ""
  :init
  (when (daemonp)
    (add-hook 'after-make-frame-functions
  	      (lambda (_)
  		(with-eval-after-load 'org
  		  (ig-setup-org-headings nil)))))
  (setq org-list-allow-alphabetical t)
  :config
  (setq org-hide-emphasis-markers t
  	org-log-done 'time
  	org-ellipsis "⤵"
  	org-special-ctrl-a/e t
  	org-special-ctrl-k t
  	org-catch-invisible-edits `show-and-error
  	org-M-RET-may-split-line nil
  	org-list-use-circular-motion t
  	org-startup-folded t
	org-src-fontify-natively t)
  (unless (daemonp) (ig-setup-org-headings (selected-frame)))
  (ig-add-nbsp-to-org-emphasis)
  ;; Convert asterisks & dashes in bullet lists to Unicode char
  (font-lock-add-keywords 'org-mode
  			  '(("^ +\\([-*]\\) "
  			     (0 (prog1 ()
  				  (compose-region (match-beginning 1) (match-end 1) "•")))))))



;; https://github.com/sabof/org-bullets
(use-package org-bullets
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode)
  :config
  (defface org-bullet-face
    '((t (:foreground "green yellow")))
    "Face used for the org-bullets.")
  (setq org-bullets-face-name 'org-bullet-face
	org-bullets-bullet-list '("◐" "◑" "◓" "◒" "◴" "◷" "◵" "◶")))



(use-package org-protocol
  :defer t
  :config
  (use-package org-protocol-capture-html)
  (setq org-capture-templates
	(quote (("w" "Weblink" entry (file (expand-file-name "bookmarks.org" ig-org-directory))
		 "* %c\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  - Quote:\n    %i" :unnarrowed)
		("o" "Web site" entry (file "")
		 "* %a :website:\n\n%U %?\n\n%:initial")))))



(use-package org-agenda
  :defer t
  :config
  (setq org-agenda-weekend-days '(5 6)
	org-agenda-start-on-weekday 0
	org-agenda-time-leading-zero t))

(provide 'ig-org)

;;; ig-org.el ends here
