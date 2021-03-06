;;; ig-packages.el --- Packages configuration

;;; Commentary:
;; Configuration of used packages.

;;; Code:



(defvar ig-org-directory (expand-file-name "org" (getenv "HOME"))
  "`Org' directory.")

(use-package ig-ivy)



;; Built-in - isearch
(use-package isearch
  :init
  ;; http://ergoemacs.org/emacs/elisp_thing-at-point_problems.html
  (defun ig-isearch-get-word-at-point ()
    "Return word at point."
    (let (start end)
      (if mark-active
	  (progn
	    (setq start (region-beginning))
	    (setq end (region-end)))
	(let ((exp "[:alnum:]-_") (orig (point)))
	  (setq start (+ orig (skip-chars-backward exp)))
	  (setq end (+ start (skip-chars-forward exp)))
	  (goto-char orig)))
      (buffer-substring-no-properties start end)))

  (defun ig-isearch-word-at-point ()
    "Search for word at point."
    (interactive)
    (let ((ig-isearch-word (ig-isearch-get-word-at-point)))
      (deactivate-mark)
      ;; Exit previous search, if any
      (let ((search-nonincremental-instead nil)
	    (isearch--current-buffer (current-buffer)))
	(isearch-exit))
      (isearch-mode t)
      (isearch-yank-string ig-isearch-word)))

  (defun ig-isearch-yank-word-at-point ()
    "Yank word at point during `isearch'."
    (interactive)
    (isearch-yank-string (ig-isearch-get-word-at-point)))
  :bind
  (("C-z" . ig-isearch-word-at-point)
   :map isearch-mode-map
   ("<up>" . isearch-ring-retreat)
   ("<down>" . isearch-ring-advance)
   ("<left>" . isearch-repeat-backward)
   ("<right>" . isearch-repeat-forward)
   ("C-w" . ig-isearch-yank-word-at-point)))



;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))



;; http://company-mode.github.io/
(use-package company
  :defer t
  :diminish ""
  :bind
  (("C->" . company-complete)
   :map company-active-map
   ("C-n" . company-select-next)
   ("C-d" . company-show-doc-buffer)
   ("<tab>" . company-complete))
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'eshell-mode-hook 'company-mode)
  (add-hook 'org-mode-hook 'company-mode)
  :config
  (setq company-backends '((company-capf company-dabbrev-code company-files))
	company-idle-delay 0.1
	company-minimum-prefix-length 3
	company-selection-wrap-around t
	company-show-numbers t
	company-tooltip-limit 20
	company-tooltip-flip-when-above t
	company-dabbrev-downcase nil
	company-transformers '(company-sort-by-occurrence)
	completion-ignore-case t
	pcomplete-ignore-case t)
  (use-package company-quickhelp
    :config
    (setq company-quickhelp-delay 0.1)
    (company-quickhelp-mode 1)))



;; https://github.com/pitkali/pos-tip
(use-package pos-tip
  :config
  (setq pos-tip-foreground-color (alect-get-color 'dark 'fg-1)
	pos-tip-background-color (alect-get-color 'dark 'bg+1)))



;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :diminish ""
  :config
  (setq which-key-popup-type 'side-window
	which-key-side-window-location 'bottom
	which-key-side-window-max-height 0.5
	which-key-use-C-h-commands t
	which-key-special-keys nil
	which-key-show-prefix 'left
	which-key-show-remaining-keys nil)
  (set-face-attribute 'which-key-local-map-description-face nil
		      :weight 'bold)
  (which-key-mode))



;; https://github.com/abo-abo/hydra
(use-package ig-hydra
  :commands (ig-hydra-dired-sort/body)
  :bind
  (("C-c f" . ig-hydra-matching-lines/body)
   ("C-x SPC" . hydra-rectangle/body)
   ("C-c w" . ig-hydra-windows/body)))



(use-package ig-keys)



;; https://github.com/wasamasa/form-feed
(use-package form-feed
  :diminish ""
  :config
  (add-hook 'emacs-lisp-mode-hook 'form-feed-mode))



;; ig-org
(defvar ig-org-command-map (make-sparse-keymap)
  "Keymap for `org'.")
(defvar ig-org-command-prefix nil
  "Prefix for `ig-org-command-map'.")
(define-prefix-command 'ig-org-command-prefix)
(fset 'ig-org-command-prefix ig-org-command-map)
(setq ig-org-command-prefix ig-org-command-map)

(use-package ig-org
  :defer 0
  :bind
  (("C-c o" . ig-org-command-prefix)
   :map ig-org-command-map
   ("l" . org-store-link)
   ("a" . org-agenda)
   ("c" . org-capture)
   ("p" . ig-yank-html-to-org)))



;; Built in - ediff
(use-package ediff
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))



;; https://github.com/dominikh/go-mode.el
(use-package go-mode
  :defer t
  :config
  (when (executable-find "go")
    (add-hook 'go-mode-hook
	      (lambda ()
		;; go get golang.org/x/tools/cmd/goimports
		;; Use goimports instead of gofmt
		(setq gofmt-command "goimports")
		(setq-local ig-indent-command 'gofmt)
		(add-hook 'before-save-hook 'gofmt-before-save)
		;; https://github.com/nsf/gocode/tree/master/emacs-company
		(use-package company-go
		  :init
		  ;; Add `company-go' to the same completion group
		  (setq-local company-backends
			      (append '(company-go) (car company-backends))))
		;; go get -v github.com/rogpeppe/godef
		(bind-keys :map go-mode-map
			   ("M-." . godef-jump)
			   ("M-," . pop-tag-mark))
		(set (make-local-variable 'compile-command)
		     "go generate && go test -v && go vet -v && go install")))))



;; https://github.com/joshwnj/json-mode
(use-package json-mode
  :defer t
  :config
  (add-hook 'json-mode-hook
	    (lambda () (setq-local ig-indent-command 'json-mode-beautify))))




;; Built in - eldoc
(use-package eldoc
  :diminish ""
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))



;; http://jblevins.org/git/markdown-mode.git
(use-package markdown-mode
  :init
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  :config
  (add-hook 'markdown-mode-hook
	    (lambda () (setq-local ig-indent-command
			      '(lambda () (error "Indenting Markdown is not supported")))))
  (when (executable-find "pandoc")
    ;; https://gist.github.com/fredRos/0e3a845de95ec654538f
    (setq markdown-command (concat "pandoc -c file://"
				   (expand-file-name "ghmd/github-pandoc.css" emacs-d)
				   " --from markdown_github -t html5 --mathjax "
				   "--highlight-style pygments --standalone"))))



;; Built in - eshell
(use-package eshell
  :defer t
  :init
  (setq eshell-directory-name (expand-file-name "eshell" volatile-dir)
	eshell-aliases-file (expand-file-name "eshell/alias" emacs-d))
  :bind (("<f5>" . eshell)))



;; ig-edit
(defvar ig-indent-command
  '(lambda () (if (region-active-p)
	     (indent-region (region-beginning) (region-end))
	   (indent-region (point-min) (point-max))))
  "Set default indentation command.")

(use-package ig-edit
  :bind (("C-c d" . prelude-duplicate-current-line-or-region)
	 ("C-d" . ig-delete-current-line-or-region)
	 ("C-c C-v" . ig-comment-lines)
	 ("C-+" . ig-font-inc)
	 ("C--" . ig-font-dec)
	 ("C-0" . ig-font-restore)
	 ("C-M-\\" . ig-indent-region-or-buffer)
	 ("M-z" . ig-describe-symbol))
  :init
  (advice-add 'kill-region :before #'ig-kill-region-or-line))



;; Built-in - dired
(use-package ig-dired)



(use-package ig-utils
  :bind (("C-x <f12>" . ig-save-buffers-kill-unconditionally)
	 ("C-c C-r" . ig-find-alternative-file-with-sudo)
	 ("C-<" . ig-goto-minibuffer)
	 ("C-x k" . ig-kill-this-buffer))
  :init
  (add-hook 'find-file-hook 'ig-find-file-root-header-warning)
  (add-hook 'dired-mode-hook 'ig-find-file-root-header-warning)
  (add-hook 'savehist-save-hook 'ig-savehist-save-hook-func))



;; https://github.com/rejeep/drag-stuff.el
(use-package drag-stuff
  :diminish drag-stuff-mode
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys)
  (drag-stuff-global-mode t))



;; https://github.com/xahlee/xahk-mode.el
(use-package xahk-mode
  :mode "\\.ahk\\'")



;; https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes
(use-package groovy-mode
  :mode "\\.gradle\\'")



;; Built in - auto-revert-tail-mode
(use-package auto-revert-tail-mode
  :init
  (setq auto-revert-verbose nil)
  :mode "\\.log\\'")



(use-package magit
  :defer t
  :init
  (setq vc-handled-backends (delq 'Git vc-handled-backends)))



(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-minor-mode)
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  :config
  (yas-reload-all)
  :bind
  ("C-c y" . company-yasnippet))



;; Built in - desktop
;; (use-package desktop
;;   :init
;;   ;; Fix problem of saving TTY font, failing to restore it in GUI
;;   (push '(font . :never) frameset-filter-alist)
;;   :config
;;   (setq desktop-save t
;; 	desktop-load-locked-desktop nil
;; 	desktop-path (list volatile-dir))
;;   (desktop-save-mode 1))



;; Built in - info
(use-package info
  :defer t
  :config
  (add-to-list 'Info-additional-directory-list (expand-file-name "info" (getenv "HOME"))))



;; https://github.com/flycheck/flycheck
(use-package flycheck
  :defer t
  :init
  ;; Depends on shellcheck
  (add-hook 'sh-mode-hook 'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  :config
  (flycheck-pos-tip-mode))



;; https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :defer t
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-character ?\|
	highlight-indent-guides-method 'character)
  (set-face-foreground 'highlight-indent-guides-character-face "darkgray"))



;; Built in - tramp
(use-package tramp
  :defer t
  :config
  ;; Enable overriding the history, disabling writing of it to not pollute the remote host with .tramp_history
  (setq tramp-histfile-override t))



;; https://github.com/Fanael/persistent-scratch
;; Should be defined near the end to use other modes
(use-package persistent-scratch
  :init
  (setq persistent-scratch-save-file
	(expand-file-name ".persistent-scratch" volatile-dir))
  :config
  (persistent-scratch-setup-default))


(provide 'ig-packages)

;;; ig-packages.el ends here
