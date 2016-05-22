;;; ig-packages.el --- Packages configuration

;;; Commentary:
;;


;;; Code:

(defvar ig-helm-open-command-map (make-sparse-keymap)
  "Keymap for opening important files via `helm'.")
(defvar ig-helm-open-command-prefix nil
  "Prefix for `ig-helm-open-command-map'.")
(define-prefix-command 'ig-helm-open-command-prefix)
(fset 'ig-helm-open-command-prefix ig-helm-open-command-map)
(setq ig-helm-open-command-prefix ig-helm-open-command-map)

(use-package ig-helm
  :diminish helm-mode
  :defer 0
  :bind
  (("C-c C-f" . ig-helm-open-command-prefix)
   :map ig-helm-open-command-map
   ("i" . ig-open-important-files)
   ("o" . ig-open-org-files)))

;; https://github.com/purcell/elisp-slime-nav
(use-package elisp-slime-nav
  :diminish ""
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook lisp-interaction-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode))
  :bind
  (:map elisp-slime-nav-mode-map
	("M-z" . elisp-slime-nav-describe-elisp-thing-at-point)))

;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; http://company-mode.github.io/
(use-package company
  :defer t
  :diminish ""
  :bind
  (("C-." . company-complete)
   :map company-active-map
   ("C-n" . company-select-next)
   ("C-d" . company-show-doc-buffer)
   ("<tab>" . company-complete))
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'eshell-mode-hook 'company-mode)
  :config
  (setq company-backends '((company-capf company-dabbrev-code company-files))
	company-idle-delay 0.1
	company-minimum-prefix-length 3
	company-selection-wrap-around t
	company-show-numbers t
	company-tooltip-limit 20
	company-tooltip-flip-when-above t
	company-dabbrev-downcase nil
	company-transformers '(company-sort-by-occurrence))
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
	which-key-prevent-C-h-from-cycling nil
	which-key-special-keys nil
	which-key-show-prefix 'left
	which-key-show-remaining-keys nil)
  (set-face-attribute 'which-key-local-map-description-face nil
		      :weight 'bold)
  (which-key-mode))



;; https://github.com/abo-abo/hydra
(use-package ig-hydra
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
   ("c" . org-capture)))



;; Built in - windmove
(use-package windmove
  :config
  (setq windmove-wrap-around t))



;; https://github.com/dominikh/go-mode.el
(use-package go-mode
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
	 ("C-M-\\" . ig-indent-region-or-buffer))
  :init
  (advice-add 'kill-region :before #'ig-kill-region-or-line))


;; File management - common
;; Note that the space used in time style format is actually a Unicode
;; char U+2008 PUNCTUATION SPACE to overcome a bug
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=18875
(defconst ig-time-style-space (string ?\u2008) "Punctuation space Unicode char.")
(defconst ig-ls-switches (concat
			  "--group-directories-first --time-style=+%d/%m/%y"
			  ig-time-style-space "%R -AhFlv") "'ls' switches.")

(use-package dired
  :config
  (setq dired-recursive-copies 'always
	dired-recursive-deletes 'top
	dired-dwim-target t
	dired-listing-switches (purecopy ig-ls-switches)
	;; We MUST now override the following regexp.
	;; There is a regular space in its end
	directory-listing-before-filename-regexp
	(purecopy (concat "\\([0-2][0-9]:[0-5][0-9] \\)\\|"
			  directory-listing-before-filename-regexp)))
  (defun ig-dired-home-dir ()
    (interactive)
    (dired (getenv "HOME")))
  (defun ig-dired-open-parent ()
    (interactive)
    (find-alternate-file ".."))
  :bind
  (("S-<f1>" . ig-dired-home-dir)
   :map dired-mode-map
   ("<RET>" . dired-find-alternate-file)
   ("^" . ig-dired-open-parent)
   ("<left>" . ig-dired-open-parent)))



(use-package ig-utils
  :bind (("C-x <f12>" . ig-save-buffers-kill-unconditionally)
	 ("C-c C-r" . ig-find-alternative-file-with-sudo)
	 ("C-z" . ig-isearch-word-at-point)
	 ("C-<" . ig-goto-minibuffer))
  :init
  (add-hook 'find-file-hook 'ig-find-file-root-header-warning)
  (add-hook 'dired-mode-hook 'ig-find-file-root-header-warning)
  (add-hook 'savehist-save-hook 'ig-savehist-save-hook-func))



;; https://github.com/rejeep/drag-stuff.el
(use-package drag-stuff
  :diminish drag-stuff-mode
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
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
  :init
  (setq vc-handled-backends (delq 'Git vc-handled-backends)))



;; Built in - desktop
(use-package desktop
  :init
  ;; Fix problem of saving TTY font, failing to restore it in GUI
  (push '(font . :never) frameset-filter-alist)
  :config
  (setq desktop-save t
	desktop-load-locked-desktop t
	desktop-path (list volatile-dir))
  (desktop-save-mode 1))



;; Built in - info
(use-package info
  :defer t
  :config
  (add-to-list 'Info-additional-directory-list (expand-file-name "info" (getenv "HOME"))))



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
