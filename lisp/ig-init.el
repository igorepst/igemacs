;;; ig-init.el --- Additional configuration beyond the minimal one

;;; Commentary:
;; Additional Emacs configuration



;;; Code:

;; The C source should reside in 'src' subdirectory of the
;; directory defined here
(setq source-directory (expand-file-name "c-src" emacs-d))

;; pp-macroexpand-last-sexp used to pretty-print macro expansion
;; https://github.com/jwiegley/use-package
(setq use-package-enable-imenu-support t)
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)



;; Decorations and UI
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-screen t
      initial-scratch-message nil
      inhibit-startup-echo-area-message t
      initial-major-mode 'emacs-lisp-mode)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode t)

(setq-default indicate-empty-lines t)

(transient-mark-mode 1)
(delete-selection-mode 1)
(show-paren-mode 1)
(column-number-mode 1)
(save-place-mode 1)

(setq scroll-margin 4
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position nil
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil)

(setq eval-expression-print-length nil
      eval-expression-print-level nil)

(add-hook 'prog-mode-hook #'electric-pair-local-mode)
(add-hook 'prog-mode-hook #'goto-address-prog-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

(setq disabled-command-function nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; https://github.com/alezost/alect-themes
(use-package alect-themes
  :demand t
  :config
  (setq custom-safe-themes t
	;; enable alect-themes in 256-colors terminals
	alect-display-class '((class color) (min-colors 256))
	alect-overriding-faces
	'((tooltip ((t (:inherit variable-pitch :background bg+1 :foreground fg-1))))))
  (load-theme 'alect-dark t))

(use-package ig-fonts
  :demand t
  :config
  (ig-set-ui-fonts))

(use-package ig-modeline)

(setq frame-title-format
      '((:eval (let ((bname buffer-file-name))
		 (if bname
		     (concat (if (buffer-modified-p) "+" "")
			     (file-name-nondirectory bname) " : "
			     (abbreviate-file-name
			      (file-name-directory bname))) "%b")))))

(setq mouse-yank-at-point t) ;yank at point instead of at click



;; Additional code
(setq apropos-do-all t ;; search more extensively, for ex., search for noninteractive f. too
      require-final-newline t
      sentence-end-double-space nil
      load-prefer-newer t
      save-interprogram-paste-before-kill t
      select-enable-clipboard t
      select-enable-primary t
      x-select-enable-clipboard-manager nil
      help-window-select t
      recenter-positions '(top middle bottom) ;; C-l (recenter-top-bottom starts from top)
      x-gtk-use-system-tooltips nil
      use-dialog-box nil
      echo-keystrokes 0.01)

(recentf-mode 1)
(global-hl-line-mode 1)
(global-prettify-symbols-mode 1)
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (push '("<=" . ?≤) prettify-symbols-alist)
	    (push '(">=" . ?≥) prettify-symbols-alist)))

(setq history-length 250
      savehist-save-minibuffer-history nil
      savehist-additional-variables
      '(search-ring regexp-search-ring extended-command-history
		    kill-ring shell-command-history
		    read-expression-history))
(savehist-mode 1)

(delete-selection-mode 1)

;; http://www.emacswiki.org/emacs/DebuggingParentheses
;; Check parens on save. Won't save until the error is fixed
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (add-hook 'local-write-file-hooks
		      'check-parens)))



;; http://emacsredux.com/blog/2013/07/24/highlight-comment-annotations
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\|NOSONAR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

(defun ig-font-lock-log-file ()
  "Highlight severity keywords in log files."
  (font-lock-add-keywords nil '(("\\<\\(ERROR\\|FATAL\\)\\>" 1 font-lock-warning-face t)))
  (font-lock-add-keywords nil '(("\\<\\(WARN\\)\\>" 1 font-lock-keyword-face t)))
  (font-lock-add-keywords nil '(("\\<\\(INFO\\)\\>" 1 font-lock-function-name-face t)))
  (font-lock-add-keywords nil '(("\\<\\(DEBUG\\|TRACE\\)\\>" 1 font-lock-constant-face t))))

(add-to-list 'auto-mode-alist
             '("\\.[Ll][Oo][Gg]\\'" . (lambda () (ig-font-lock-log-file))))



;; Bash & zsh
(defconst ig-sh-files
  '(".aliases"))
(defconst ig-bash-files '(".bashrc" ".bash_logout" "bash.bashrc" ".inputrc"))
(defconst ig-zsh-files
  '(".zhistory" "zlogin" "zlogout" "zpreztorc" "zprofile" "zshenv" "zshrc"
    "init.zsh"))

(defun ig-add-list-to-sh-mode (names)
  "Add NAMES to `sh-mode'."
  (dolist (name names)
    (add-to-list 'auto-mode-alist `(,(format "\\%s\\'" name) . sh-mode))))

(ig-add-list-to-sh-mode ig-sh-files)
(ig-add-list-to-sh-mode ig-bash-files)
(ig-add-list-to-sh-mode ig-zsh-files)

(defun ig-sh-set-shell (names shell-name buf-name)
  "Loop on NAMES to set SHELL-NAME, comparing to BUF-NAME."
  (dolist (name names)
    (when (string= buf-name name)
      (sh-set-shell shell-name) t)))

(add-hook 'sh-mode-hook
          (lambda ()
	    (when buffer-file-name
	      (let ((buf-name (file-name-nondirectory buffer-file-name)))
		(when (not (ig-sh-set-shell ig-sh-files "sh" buf-name))
		  (when (not (ig-sh-set-shell ig-bash-files "bash" buf-name))
		    (ig-sh-set-shell ig-zsh-files "zsh" buf-name)))))))



(setq visual-line-fringe-indicators '(left-curly-arrow nil))
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)



(use-package ig-packages)


(provide 'ig-init)

;;; ig-init.el ends here
