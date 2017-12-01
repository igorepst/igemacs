;;; -*- lexical-binding: t -*-
;;; init.el --- The main entry point of igemacs

;;; Commentary:
;; Main entry point.  This holds minimal configuration
;; with the possibility to load additional one



;;; Code:


;; Profiles and command line arguments
(defun ig-get-profile-arg-value (ig-cmd-arg)
  "Parse command line arguments, looking for IG-CMD-ARG.
`command-switch-alist' is not used here to alter the
behaviour, before this file is fully evaluated, or in
cases, where the argument is not present.  As a consequence,
this argument should be added after 'empty' argument ('--')
to prevent Emacs from complaining about 'Unknown option'."
    (catch 'tag
      (dolist (arg command-line-args)
	(when (string-prefix-p ig-cmd-arg arg)
	  ;; Delete the argument to prevent Emacs from creating a file
	  (setq command-line-args (delete arg command-line-args))
	  (throw 'tag (substring arg (length ig-cmd-arg)))))))


(defun ig-start-init()
  (pcase (ig-get-profile-arg-value "--ig-profile=")
    ("update"
     (require 'ig-install-packages)
     (ig-packages-manage))
    ("minimal"
     (princ "Minimal configuration was loaded\n"))
    ("profiler"
     (require 'profiler)
     (profiler-start 'cpu+mem)
     (require 'ig-init)
     (profiler-report)
     (profiler-stop))
    (_ (require 'ig-init))))


(defun ig-packages-init()
  (defconst package-archives-protocol
    (if (string= system-type `windows-nt) "http://" "https://")
    "More often than not GnuTLS doesn't work properly on Windows.")
  (setq package-archives
	`(("melpa" . ,(concat package-archives-protocol "melpa.org/packages/"))
	  ("org" . "http://orgmode.org/elpa/")
	  ("gnu" . ,(concat package-archives-protocol "elpa.gnu.org/packages/"))))
  (setq package-user-dir
	(expand-file-name "elpa" ig-emacs-d))
  (package-initialize)
  (setq package-enable-at-startup nil))

(defconst ig-emacs-d
  ;; `load-file-name' may be null if using `esup' start up profiler, for ex.
  (file-truename
   (if load-file-name
       (file-name-directory load-file-name) user-emacs-directory))
  "Base config directory.")
(defconst ig-additional-lisp-dir (expand-file-name "lisp" ig-emacs-d))

(defconst ig-volatile-dir (expand-file-name ".volatile" ig-emacs-d))
(unless (file-directory-p ig-volatile-dir)
  (make-directory ig-volatile-dir t))

(defconst ig-etc-dir (expand-file-name "etc" ig-emacs-d))
(unless (file-directory-p ig-etc-dir)
  (make-directory ig-etc-dir t))

;; Autoloads
(defconst ig-generated-autoload-file-name "loaddefs.el")
(defconst generated-autoload-file (expand-file-name ig-generated-autoload-file-name ig-volatile-dir))
(add-to-list 'revert-without-query ig-generated-autoload-file-name)

;; https://github.com/abo-abo/oremacs/blob/github/oleh/auto.el
;;;###autoload
(defun ig-update-all-autoloads ()
  "Update `autoload' in directories."
  (interactive)
  (cd ig-emacs-d)
  (when (not (file-exists-p generated-autoload-file))
    (with-current-buffer (find-file-noselect generated-autoload-file)
      (insert ";;") ;; create the file with non-zero size to appease autoload
      (save-buffer)))
  (mapcar #'update-directory-autoloads
	  '("" "lisp")))

;; Faster start by disabling special processing temporarily,
;; especially useful on Cygwin
(let ((file-name-handler-alist '()))
  (setq gc-cons-threshold (* 50 1024 1024)
	backup-inhibited 1)
  (add-to-list 'load-path ig-additional-lisp-dir)
  (let ((default-directory ig-additional-lisp-dir))
    (normal-top-level-add-subdirs-to-load-path))
  (ig-packages-init)
  (add-hook 'kill-emacs-hook #'ig-update-all-autoloads)
  (when (not (file-exists-p generated-autoload-file)) (ig-update-all-autoloads))
  (load generated-autoload-file nil t)
  (ig-start-init))

;;; init.el ends here
