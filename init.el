;;; init.el --- The main entry point of igemacs

;;; Commentary:
;; Main entry point.  This holds minimal configuration
;; with the possibility to load additional one



;;; Code:

;; Faster start by disabling special processing temporarily,
;; especially useful on Cygwin
(setq ig-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist '())

(defconst emacs-d
  ;; load-file-name may be null if using 'esup' start up profiler, for ex.
  (file-truename
   (if load-file-name
       (file-name-directory load-file-name) user-emacs-directory))
  "Base config directory.")

(setq gc-cons-threshold (* 40 1024 1024))

(eval-when-compile
  (require 'cl))

(setq backup-inhibited 1)

(defconst additional-lisp-dir (expand-file-name "lisp" emacs-d))
(add-to-list 'load-path additional-lisp-dir)
(let ((default-directory additional-lisp-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; Work in progress
(add-to-list 'load-path (expand-file-name "wip" emacs-d))

(require 'ig-volatile)

(add-to-list 'default-frame-alist '(fullscreen . maximized))



;; use UTF-8 everywhere by default
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8
      ;; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
      utf-translate-cjk-mode nil)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
;; use default utf-16-le to not alter the clipboard on Windows
(set-selection-coding-system
 (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
(prefer-coding-system 'utf-8)
(setq system-time-locale "C")



(defconst package-archives-protocol
  (if (string= system-type `windows-nt) "http://" "https://")
  "More often than not GnuTLS doesn't work properly on Windows.")
(setq package-archives
      `(("melpa" . ,(concat package-archives-protocol "melpa.org/packages/"))
	("org" . "http://orgmode.org/elpa/")
        ("gnu" . ,(concat package-archives-protocol "elpa.gnu.org/packages/"))))
(setq package-user-dir
      (expand-file-name "elpa" emacs-d))
(package-initialize)
(setq package-enable-at-startup nil)



;; Profiles and command line arguments
(defun ig-get-profile-arg-value (ig-cmd-arg)
  "Parse command line arguments, looking for IG-CMD-ARG.
`command-switch-alist' is not used here to alter the
behaviour, before this file is fully evaluated, or in
cases, where the argument is not present.  As a consequence,
this argument should be added after 'empty' argument ('--')
to prevent Emacs from complaining about 'Unknown option'."
  (dolist (arg command-line-args)
    (when (string-prefix-p ig-cmd-arg arg)
      ;; Delete the argument to prevent Emacs from creating a file
      (setq command-line-args (delete arg command-line-args))
      (return (substring arg (length ig-cmd-arg))))))

;; Use `pcase' to switch by strings, using `equal'
(pcase (ig-get-profile-arg-value "--ig-profile=")
  ("update" (require 'ig-install-packages))
  ("minimal" (princ "Minimal configuration was loaded"))
  ("profiler"
   (require 'profiler)
   (profiler-start 'cpu+mem)
   (require 'ig-init)
   (profiler-report)
   (profiler-stop))
  (_ (require 'ig-init)))



;; Restore original value
(setq file-name-handler-alist ig-file-name-handler-alist)
(setq ig-file-name-handler-alist nil)

;;; init.el ends here
