;;; ig-ivy.el --- Ivy configuration

;;; Commentary
;; Configuration of Ivy and supplementary packages

;;; Code

;; Requirement for counsel-M-x
;; https://github.com/nonsequitur/smex
(use-package smex
  :config
  (setq smex-save-file (expand-file-name "smex-save" volatile-dir)
	smex-history-length 10))

(defun ig-open-org-files ()
  "Open `org' files."
  (interactive)
  (ivy-read "Org file: "
	    (cl-remove-if-not
	     (lambda (x)
	       (string-match "org$" (file-name-nondirectory x)))
	     (directory-files ig-org-directory))
	    :action (lambda (x) (find-file (expand-file-name x ig-org-directory)))))

(defun ig-open-important-files ()
  "Open important files."
  (interactive)
  (ivy-read "Important file: "
	    `(("ig-init" .
	       ,(expand-file-name "ig-init.el" additional-lisp-dir))
	      ("Packages to install" .
	       ,(expand-file-name "ig-install-packages.el"
				  additional-lisp-dir))
	      ("Config packages" .
	       ,(expand-file-name "ig-packages.el"
				  additional-lisp-dir))
	      ("Main" . ,(expand-file-name "init.el" emacs-d))
	      ("Readme" . ,(expand-file-name "README.md" emacs-d)))
	    :action #'find-file))

(defvar ig-counsel-ag-files-dir nil
  "Directory to use in `ig-counsel-ag-files'.")
(defvar ig-counsel-ag-files-cmd nil
  "Command to use in `ig-counsel-ag-files'.")

(defun ig-counsel-ag-files-function (string &optional _pred &rest _unused)
  "Search for file names by ag with STRING."
  (let ((default-directory ig-counsel-ag-files-dir))
    (split-string
     (shell-command-to-string
      (format ig-counsel-ag-files-cmd string))
     "\n" t)))

(defun ig-counsel-ag-files-action (x)
  (when x
    (find-file (expand-file-name x ig-counsel-ag-files-dir))))

(defun ig-counsel-ag-files (unrestricted)
  "Search for files by names via 'ag'.
Argument UNRESTRICTED makes 'ag' to search all files."
  (interactive "P")
  (setq ig-counsel-ag-files-dir
	(or (locate-dominating-file default-directory ".git")
	    default-directory))
  (setq ig-counsel-ag-files-cmd
	(if unrestricted "ag --vimgrep --nocolor -u -i -g \"%s\""
	  "ag --vimgrep --nocolor --hidden -i -g \"%s\""))
  (ivy-read "Ag for files: "
   'ig-counsel-ag-files-function
   :action #'ig-counsel-ag-files-action
   :require-match t
   :caller 'ig-counsel-ag-files))


(defvar ig-ivy-open-command-map (make-sparse-keymap)
  "Keymap for opening important files via `ivy'.")
(defvar ig-ivy-open-command-prefix nil
  "Prefix for `ig-ivy-open-command-map'.")
(define-prefix-command 'ig-ivy-open-command-prefix)
(fset 'ig-ivy-open-command-prefix ig-ivy-open-command-map)
(setq ig-ivy-open-command-prefix ig-ivy-open-command-map)

(use-package ivy
  :diminish ""
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
	ivy-height 10
	ivy-count-format "(%d/%d) "
	ivy-wrap t
	ivy-re-builders-alist '((t . ivy--regex-fuzzy))
	ivy-views '(("elisp-info [–]"
		     (horz
		      (sexp (info "(elisp)Top"))
		      (sexp (find-file (expand-file-name "elisp.org" ig-org-directory)))))))
  :bind
  (("C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("<f6>" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c k" . counsel-ag)
   ("C-c a" . ig-counsel-ag-files)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-x l" . counsel-locate)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("C-c C-f" . ig-ivy-open-command-prefix)
   :map ig-ivy-open-command-map
   ("i" . ig-open-important-files)
   ("o" . ig-open-org-files)
   :map help-map
   ("f" . counsel-describe-function)
   ("v" . counsel-describe-variable)
   ("l" . counsel-load-library)
   :map read-expression-map
   ("C-r" . counsel-expression-history)))


(provide 'ig-ivy)

;;; ig-ivy.el ends here
