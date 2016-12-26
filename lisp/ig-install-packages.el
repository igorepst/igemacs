;;; ig-install-packages.el --- Used in updates

;;; Commentary:
;; Used to install or update packages (see 'up' make target).



;;; Code:

(package-refresh-contents)



(defvar ig-packages-req-list
  '(form-feed
    persistent-scratch
    which-key
    window-numbering
    hydra
    drag-stuff
    xahk-mode
    groovy-mode
    markdown-mode
    go-mode
    json-mode
    magit
    buffer-move
    yasnippet
    highlight-indent-guides
    ;; flycheck
    flycheck
    flycheck-pos-tip
    ;; ivy
    ivy
    ivy-rich
    swiper
    counsel
    ivy-hydra
    smex
    flx
    ivy-pages
    ;; org
    org-plus-contrib
    org-bullets
    htmlize
    ox-mediawiki
    ;; dired
    dired-rainbow
    ;; use-package
    use-package
    diminish
    bind-key
    ;; company
    company
    company-quickhelp
    ;; LAF
    alect-themes
    rainbow-delimiters
    powerline
    spaceline)
  "List of packages to install.")



(defvar ig-packages-install-list nil)
(defvar ig-packages-upgrade-list nil)
(defvar ig-packages-error-list nil)
(defvar ig-packages-delete-list nil)
(defvar ig-packages-upgrade-p nil)

(defun ig-packages-install-required ()
  "Install required packages."
  (dolist (package ig-packages-req-list)
    (unless (package-installed-p package)
      (condition-case nil
	  (progn
	    (package-install package)
	    (push (cadr (assq package package-alist)) ig-packages-install-list))
	(error
	 (push package ig-packages-error-list))))))

(defun ig-quelpa-package-install(orig-fun &rest args)
  (let ((res (apply orig-fun args)))
    (when res
      (push res (if ig-packages-upgrade-p ig-packages-upgrade-list ig-packages-install-list)))
    res))

;; https://github.com/quelpa/quelpa
;; TODO: find a way to know whether quelpa installed/upgraded something
;; TODO: use package-alist to know exactly what was changed
(defun ig-packages-quelpa ()
  "Install Quelpa and packages from it."
  (let (package-build-installed-p quelpa-installed-p)
    (when (package-installed-p 'package-build)
      (setq package-build-installed-p t))
    (setq quelpa-dir (expand-file-name "quelpa" volatile-dir))
    (if (require 'quelpa nil t)
	(progn
	  (setq quelpa-installed-p t)
	  (setq ig-packages-upgrade-p t)
	  (advice-add 'quelpa-package-install :around #'ig-quelpa-package-install)
	  (quelpa-self-upgrade)
	  (advice-remove 'quelpa-package-install #'ig-quelpa-package-install))
      (with-temp-buffer
	(url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
	(eval-buffer)))
    (unless package-build-installed-p
      (push (cadr (assq 'package-build package-alist)) ig-packages-install-list))
    (unless quelpa-installed-p
      (push (cadr (assq 'quelpa package-alist)) ig-packages-install-list)))

  (advice-add 'quelpa-package-install :around #'ig-quelpa-package-install)
  (let ((ig-quelpa-install-list
	 '((org-protocol-capture-html :repo "alphapapa/org-protocol-capture-html" :fetcher github))))
    (dolist (pack ig-quelpa-install-list)
      (let ((pack-name (car pack)))
	(setq ig-packages-upgrade-p (package-installed-p pack-name))
	(quelpa pack :upgrade t)
	(when (not (package-installed-p pack-name))
	  (push pack-name ig-packages-error-list)))))
  (advice-remove 'quelpa-package-install #'ig-quelpa-package-install))

(defun ig-packages-upgrade-and-clean ()
  "Upgrade packages and remove obsolete."
  (save-window-excursion
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (package-menu-mark-obsolete-for-deletion)

    (let (cmd pkg-desc)
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (setq cmd (char-after))
	  (unless (eq cmd ?\s)
	    ;; This is the key PKG-DESC.
	    (setq pkg-desc (tabulated-list-get-id))
	    (cond ((eq cmd ?D)
		   (push pkg-desc ig-packages-delete-list))
		  ((eq cmd ?I)
		   (push pkg-desc ig-packages-upgrade-list))))
	  (forward-line)))
      (when (or ig-packages-delete-list ig-packages-upgrade-list)
        (condition-case nil
	    (package-menu-execute t)
	  (error
	   (package-menu-execute)))))))

(defun ig-packages-print-packages-list (pack-list)
  "Print sorted PACK-LIST."
  (dolist (pack (sort pack-list 'string-lessp))
    (message "  %s" pack)))

(defun ig-packages-print-list-sum (msg pack-list)
  "Print MSG as a summary, process PACK-LIST as needed."
  (let ((len (length pack-list)))
    (message "* %s:" (format msg len))
    (when (/= len 0)
      (let ((el (car pack-list)))
	(if (vectorp el)
	    (let (conv-list)
	      (dolist (pack pack-list)
		(push (format "%s %s" (elt pack 1)
			      (mapconcat 'identity
					 (split-string (format "%s" (elt pack 2)) " ")
					 "."))
		      conv-list))
	      (ig-packages-print-packages-list conv-list))
	  (ig-packages-print-packages-list pack-list))))))

(defun ig-packages-print-report ()
  "Print report."
  (message "\n****************")
  (if (not (or ig-packages-install-list ig-packages-upgrade-list ig-packages-delete-list ig-packages-error-list))
      (message "No packages to manage")
    (progn
      (ig-packages-print-list-sum "Installed %d packages" ig-packages-install-list)
      (ig-packages-print-list-sum "Upgraded %d packages" ig-packages-upgrade-list)
      (ig-packages-print-list-sum "Removed %d obsolete packages" ig-packages-delete-list)
      (ig-packages-print-list-sum "Error during install of %d packages"
				  ig-packages-error-list))))

(defun ig-packages-manage ()
  "Manage packages."
  (interactive)
  (ig-packages-install-required)
  (ig-packages-quelpa)
  (ig-packages-upgrade-and-clean)
  (ig-packages-print-report)
  (setq ig-packages-install-list nil)
  (setq ig-packages-upgrade-list nil)
  (setq ig-packages-error-list nil)
  (setq ig-packages-delete-list nil))

(provide 'ig-install-packages)

;;; ig-install-packages.el ends here
