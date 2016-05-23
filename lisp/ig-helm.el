;;; ig-helm.el --- Helm & support

;;; Commentary:
;; Initializes Helm and its additional libraries.
;; Tutorial: http://tuhdo.github.io/helm-intro.html
;; Official: https://github.com/emacs-helm/helm



;;; Code:

;;https://github.com/syl20bnr/spacemacs/blob/master/layers/%2Bdistribution/spacemacs-base/packages.el
(defun ig-helm-display-at-bottom ()
  "Display `helm' window at bottom in all cases."
  (setq helm-split-window-in-side-p t)
  (defvar spacemacs-helm-display-help-buffer-regexp '("*.*Helm.*Help.**"))
  (defvar spacemacs-helm-display-buffer-regexp `("*.*helm.**"
						 (display-buffer-in-side-window)
						 (inhibit-same-window . t)
						 (window-width . 0.6)
						 (window-height . 0.4)))
  (defvar spacemacs-display-buffer-alist nil)

  (setq helm-display-function (lambda (buffer)
				(let ((display-buffer-alist
				       (list spacemacs-helm-display-help-buffer-regexp
					     spacemacs-helm-display-buffer-regexp)))
				  (helm-default-display-buffer buffer))))

  (add-hook 'helm-after-initialize-hook (lambda ()
					  (let ((display-buffer-base-action '(nil)))
					    (setq spacemacs-display-buffer-alist display-buffer-alist)
					    (setq display-buffer-alist nil))))

  (add-hook 'helm-cleanup-hook (lambda () (setq display-buffer-alist spacemacs-display-buffer-alist))))

(use-package helm
  :demand t
  :diminish ""
  :config
  (require 'helm-config)
  (unbind-key "C-x c")
  (helm-mode 1)
  (ig-helm-display-at-bottom)
  :bind (("C-c h" . helm-command-prefix)
	 ("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x b" . helm-multi-files)
	 :map helm-map
	 ("<tab>" . helm-execute-persistent-action)
	 ("C-i" . helm-execute-persistent-action)
	 ("C-z" . helm-select-action)
	 :map helm-command-map
	 ("o" . helm-occur)))


(defvar ig-helm-ag-command-map (make-sparse-keymap)
  "Keymap for `helm-ag'.")
(defvar ig-helm-ag-command-prefix nil
  "Prefix for `ig-helm-ag-command-map'.")
(define-prefix-command 'ig-helm-ag-command-prefix)
(fset 'ig-helm-ag-command-prefix ig-helm-ag-command-map)
(setq ig-helm-ag-command-prefix ig-helm-ag-command-map)

(use-package helm-ag
  :config
  (defun ig-helm-ag--action-find-file (candidate)
    "Override `helm-ag--action-find-file' to support searching for files by names."
    (find-file (expand-file-name candidate helm-ag--default-directory)))

  (defun ig-helm-ag-search-for-file-project-root (unrestricted)
    "Search for files by names via `helm-ag'.
Argument UNRESTRICTED makes 'ag' to search all files."
    (interactive "P")
    (let ((helm-ag-base-command (if unrestricted "ag -u -g" "ag -g")) (current-prefix-arg nil))
      (advice-add 'helm-ag--action-find-file :override #'ig-helm-ag--action-find-file)
      (helm-ag-project-root)
      (advice-remove 'helm-ag--action-find-file #'ig-helm-ag--action-find-file)))

  (defun ig-helm-do-ag ()
    "Wrapper to pass default directory."
    (interactive)
    (helm-do-ag default-directory))
  :bind (("C-c a" . ig-helm-ag-command-prefix)
	 :map ig-helm-ag-command-map
	 ("f" . ig-helm-ag-search-for-file-project-root)
	 ("a" . ig-helm-do-ag)
	 ("t" . helm-do-ag-this-file)
	 ("p" . helm-do-ag-project-root)
	 ("b" . helm-do-ag-buffers)))



;; https://github.com/syohex/emacs-helm-gtags
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
	      (use-package helm-gtags
		:init
		(setq helm-gtags-ignore-case t
		      helm-gtags-use-input-at-cursor t
		      helm-gtags-highlight-candidate t
		      helm-gtags-auto-update t
		      helm-gtags-pulse-at-cursor t
		      helm-gtags-direct-helm-completing t
		      helm-gtags-preselect t)
		(helm-gtags-mode t)
		:diminish ""
		:config
		(defun ig-helm-gtags-pop()
		  "In case the stack is empty, use `pop-tag-mark'."
		  (interactive)
		  (condition-case nil
		      (helm-gtags-pop-stack)
		    (error (pop-tag-mark))))
		:bind (:map helm-gtags-mode-map
			    ("M-s" . helm-gtags-select)
			    ("M-." . helm-gtags-dwim)
			    ("M-," . ig-helm-gtags-pop)
			    ("C-<" . helm-gtags-previous-history)
			    ("C->" . helm-gtags-next-history))))))



;; https://github.com/david-christiansen/helm-pages
(use-package helm-pages
  :defer t)

;;;###autoload
(defun ig-helm-pages ()
  "Use `helm-org' or `helm-pages' conditionally."
  (interactive)
  (if (string= (ig-buffer-mode (current-buffer)) "org-mode")
      (helm-org-in-buffer-headings) (helm-pages)))
(bind-key "n" 'ig-helm-pages helm-command-map)



;; https://github.com/emacs-helm/helm-cmd-t
(use-package helm-cmd-t)

(defun ig-open-important-files ()
  "Open important files."
  (interactive)
  (helm :sources
	`(((name . "Emacs config files")
	   (candidates .
		       (("ig-init" .
			 ,(expand-file-name "ig-init.el" additional-lisp-dir))
			("Packages to install" .
			 ,(expand-file-name "ig-install-packages.el"
					    additional-lisp-dir))
			("Config packages" .
			 ,(expand-file-name "ig-packages.el"
					    additional-lisp-dir))
			("Main" . ,(expand-file-name "init.el" emacs-d))
			("Readme" . ,(expand-file-name "README.md" emacs-d))))
	   (action . (("Open" . (lambda (candidate) (find-file candidate)))))))))

(defun ig-open-org-files ()
  "Open org files."
  (interactive)
  (helm :sources
	(helm-cmd-t-get-create-source-dir (expand-file-name "org" (getenv "HOME")))
	:input "org$ "))


(provide 'ig-helm)

;;; ig-helm.el ends here
