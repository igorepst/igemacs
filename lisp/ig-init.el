;;; -*- lexical-binding: t -*-

;; The C source should reside in 'src' subdirectory of the
;; directory defined here
(setq source-directory (expand-file-name "c-src" ig-emacs-d))
;;; Defined in C
;; Always load newest byte code
(setq load-prefer-newer t
      enable-recursive-minibuffers t
      system-time-locale "C"
      locale-coding-system 'utf-8)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq frame-title-format
      '((:eval (let ((bname buffer-file-name))
		 (cond (bname
			(concat (when (buffer-modified-p) "+")
				(file-name-nondirectory bname) " : "
				(abbreviate-file-name (file-name-directory bname))))
		       ((eq major-mode 'dired-mode) default-directory)
		       (t "%b"))))))

;; startup.el - not a package
(setq initial-buffer-choice t
      inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'emacs-lisp-mode
      auto-save-list-file-prefix (expand-file-name ".saves-" ig-volatile-dir))

(defalias 'yes-or-no-p 'y-or-n-p)



;; Must be set before loading use-package
(setq use-package-enable-imenu-support t)
(eval-when-compile
  (require 'use-package))

;; https://github.com/jwiegley/use-package
(use-package use-package
  :config
  (setq use-package-always-defer t))

;; https://savannah.nongnu.org/projects/delight
(use-package delight)



;; https://github.com/noctuid/general.el
(use-package general
  :demand t
  :config
  ;; Unbind key before using as a prefix
  (general-define-key "M-SPC" nil)
  (general-create-definer ig-leader
			  :states '(normal insert emacs) :keymaps 'global
			  :prefix "SPC" :non-normal-prefix "M-SPC")
  ;; Define prefixes and commands from the C code
  (ig-leader "w" '(:ignore t :which-key "Window")
	     "/" '(:ignore t :which-key "Search")
	     "f" '(:ignore t :which-key "File")
	     "b" '(:ignore t :which-key "Buffer")
	     "b k" '((lambda() (interactive) (kill-buffer (current-buffer))) :which-key "Kill current")))



;; Built-in
(use-package mule
  :config
  ;; use UTF-8 everywhere by default
  (set-language-environment 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  ;; use default utf-16-le to not alter the clipboard on Windows
  (set-selection-coding-system
   (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
  (prefer-coding-system 'utf-8)
  ;; open .nfo the right way
  (modify-coding-system-alist 'file "\\.[nN][fF][oO]\\'" 'ibm437))

;; Built-in
(use-package paren
  :init
  (show-paren-mode 1))

;; Built-in
(use-package elisp-mode
  :config
  (delight '((emacs-lisp-mode "Ɛ" :major))))

;; Built-in
(use-package savehist
  :demand t
  :defines savehist-additional-variables
  :config
  (setq savehist-save-minibuffer-history t
	savehist-file (expand-file-name ".emacs-history" ig-volatile-dir))
  (savehist-mode 1))

;; https://github.com/jwiegley/use-package/issues/203
;; Built-in
(use-package "isearch"
  :delight isearch-mode " 🔎" t
  :config
  (setq isearch-allow-scroll t
	lazy-highlight-cleanup nil
	lazy-highlight-initial-delay 0)
  (push 'search-ring savehist-additional-variables)
  (push 'regexp-search-ring savehist-additional-variables))

;; Built-in
(use-package recentf
  :demand t
  :config
  (setq recentf-max-saved-items 100
	recentf-save-file (expand-file-name ".recentf" ig-volatile-dir)
	;; disable recentf-cleanup (may cause problems with remote files)
        recentf-auto-cleanup 'never)
  (recentf-mode 1))

;; Built-in
(use-package custom
  :config
  (setq custom-file (expand-file-name "custom.el" ig-volatile-dir)
	custom-safe-themes t))

;; Built-in
(use-package saveplace
  :demand t
  :config
  (setq save-place-file (expand-file-name "places" ig-volatile-dir))
  (save-place-mode 1))

;; Built-in
(use-package novice
  :init
  (setq disabled-command-function nil))

;; Built-in
(use-package bookmark
  :config
  (setq bookmark-default-file (expand-file-name "bookmark" ig-volatile-dir)))

;; Built-in
(use-package tramp-cache
  :config
  (setq tramp-persistency-file-name (expand-file-name "tramp" ig-volatile-dir)))

;; Built-in
(use-package url
  :config
  (setq url-configuration-directory (expand-file-name "url" ig-volatile-dir)
	url-cookie-file (expand-file-name "cookies" url-configuration-directory)))

;; Built-in
(use-package server
  :config
  (setq server-auth-dir ig-volatile-dir))

;; Built-in
(use-package nsm
  :config
  (setq nsm-settings-file (expand-file-name "network-security.data"
					    ig-volatile-dir)))
;; Built-in
(use-package x-win
  :config
  (defun emacs-session-filename (session-id)
    (expand-file-name (concat "session." session-id) ig-volatile-dir)))

;; Built-in
(use-package mb-depth
  :init
  (minibuffer-depth-indicate-mode 1))

;; Built-in
(use-package delsel
  :init
  (delete-selection-mode 1))

(use-package goto-addr
  :init
  (add-hook 'prog-mode-hook #'goto-address-prog-mode))



;; https://github.com/emacs-evil/evil
(use-package evil
  :demand t
  :init
  (evil-mode 1)
  :general
  (ig-leader "/ /" #'evil-search-forward))

;; http://www.dr-qubit.org/undo-tree/undo-tree.el
(use-package undo-tree
  :delight undo-tree-mode nil t)



;; http://orgmode.org/
(use-package org
  :init
  (defvar ig-org-directory "~/org")
  :config
  (setq org-special-ctrl-a/e t
	org-special-ctrl-k t))



;; https://github.com/abo-abo/swiper
(use-package ivy
  :demand t
  :delight ivy-mode nil t
  :config
  (setq ivy-use-virtual-buffers t
	ivy-height 10
	ivy-count-format "(%d/%d) "
	ivy-wrap t)
  (ivy-mode 1)
  :general
  ("C-c C-r" #'ivy-resume)
  (ig-leader "b SPC" '(ivy-switch-buffer :which-key "Switch")))

;; https://github.com/abo-abo/swiper
(use-package counsel
  :general
  (ig-leader 
   "/ j" #'counsel-git-grep
   "/ r" #'counsel-rg
   "/ s" #'counsel-grep-or-swiper
   "f l" #'counsel-locate
   "f f" #'counsel-find-file
   "f g" #'counsel-git
   "SPC" #'counsel-M-x)
  ("M-x" #'counsel-M-x
   "C-s" #'counsel-grep-or-swiper
   "<f2> i" #'counsel-info-lookup-symbol
   "<f2> u" #'counsel-unicode-char)
  (:keymaps 'help-map
  	    "f" #'counsel-describe-function
  	    "v" #'counsel-describe-variable
  	    "l" #'counsel-find-library)
  (:keymaps 'read-expression-map
  	    "C-r" #'counsel-expression-history))

;; https://github.com/abo-abo/hydra
(use-package ig-hydra
  :general
  (ig-leader
   "h" '(:ignore t :which-key "Hydra")
   "h v" '(ig-hydra-ivy-views/body :which-key "Ivy views")))

;; Requirement for counsel-M-x
;; https://github.com/nonsequitur/smex
(use-package smex
  :config
  (setq smex-save-file (expand-file-name "smex-save" ig-volatile-dir)
	smex-history-length 10))



;; https://github.com/justbur/emacs-which-key/
(use-package which-key
  :demand t
  :delight which-key-mode nil t
  :config
  (setq which-key-sort-order 'which-key-description-order
	which-key-idle-delay 0.3)
  (which-key-mode))



;; https://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme
  :demand t
  :config
  (load-theme 'zenburn t))

(use-package ig-fonts
  :demand t
  :config
  (ig-set-ui-fonts))

;; https://github.com/wasamasa/form-feed
(use-package form-feed
  :delight form-feed-mode nil t
  :init
  (add-hook 'emacs-lisp-mode-hook 'form-feed-mode))

(use-package prettify-symbols-mode
  :init
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (global-prettify-symbols-mode 1)
  (add-hook 'prog-mode-hook
	    (lambda ()
	      (push '("<=" . ?≤) prettify-symbols-alist)
	      (push '(">=" . ?≥) prettify-symbols-alist))))



;; http://company-mode.github.io/
;; (use-package company
;;   :delight company-mode nil t
;;   :init
;;   (add-hook 'prog-mode-hook #'company-mode)
;;   (add-hook 'eshell-mode-hook #'company-mode)
;;   (add-hook 'shell-mode-hook #'company-mode)
;;   (add-hook 'org-mode-hook #'company-mode)
;;   :config
;;   (setq company-backends '((company-dabbrev-code company-dabbrev company-keywords company-capf company-files) company-bbdb company-nxml company-css)
;; 	company-require-match nil))



;; Buil-in
(use-package dired
  :config
  (setq dired-recursive-copies 'always
	dired-recursive-deletes 'always
	dired-dwim-target t)
  (defun ig-dired-maybe-visit-new-buffer()
    "Always leave one Dired buffer open."
    (let ((file (dired-get-file-for-visit)))
      (if (file-directory-p file)
	  (find-alternate-file file)
	(find-file (file-name-sans-versions file t)))))
  (defun ig-dired-mouse-find-file (event)
    "In Dired, visit the file or directory name you click on in the same window."
    (interactive "e")
    (let ((window (posn-window (event-end event)))
	  (pos (posn-point (event-end event))))
      ;; In case the active window is NOT where the click was done,
      ;; as the focus stays where it was
      (select-window window)
      (set-buffer (window-buffer window))
      (goto-char pos)
      (ig-dired-maybe-visit-new-buffer)))
  :general
  (:keymaps 'dired-mode-map
	    [mouse-2] #'ig-dired-mouse-find-file
	    [remap dired-find-file] #'((lambda() (interactive) (ig-dired-maybe-visit-new-buffer)))
	    [remap dired-up-directory] '((lambda() (interactive) (find-alternate-file "..")))))



(use-package ig-utils)

(provide 'ig-init)
