;;; ig-org.el --- Init org mode

;;; Commentary:
;;

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
  	org-startup-folded t)
  (unless (daemonp) (ig-setup-org-headings (selected-frame)))
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
  (setq org-capture-templates
	(quote (("w" "Weblink" entry (file "~/org/bookmarks.org")
		 "* %c\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  - Quote:\n    %i" :unnarrowed)))))



(use-package org-agenda
  :defer t
  :config
  (setq org-agenda-weekend-days '(5 6)
	org-agenda-start-on-weekday 0
	org-agenda-time-leading-zero t))

(provide 'ig-org)

;;; ig-org.el ends here
