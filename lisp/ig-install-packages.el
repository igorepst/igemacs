;;; ig-install-packages.el --- Used in updates

;;; Commentary:
;; Used to install or update packages (see 'up' make target).



;;; Code:

(package-refresh-contents)



(defconst ig-packages-install-list
  '(elisp-slime-nav
    form-feed
    persistent-scratch
    which-key
    window-numbering
    anzu
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
    ;; flycheck
    flycheck
    flycheck-pos-tip
    ;; ivy
    ivy
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



;; Manage packages

(defvar install-list nil)
(defvar upgrade-list nil)
(defvar delete-list nil)
(defvar error-list nil)

;; Install required
(dolist (package ig-packages-install-list)
  (unless (package-installed-p package)
    (condition-case nil
	(progn
	  (package-install package)
	  (push package install-list))
      (error
       (push package error-list)))))

;; https://github.com/quelpa/quelpa
;; TODO: find a way to know whether quelpa installed/upgraded something
(unless (package-installed-p 'package-build)
  (push "package-build" install-list))
(setq quelpa-dir (expand-file-name "quelpa" volatile-dir))
(if (require 'quelpa nil t)
    (quelpa-self-upgrade)
  (with-temp-buffer
    (push "quelpa" install-list)
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(quelpa '(org-protocol-capture-html :repo "alphapapa/org-protocol-capture-html" :fetcher github) :upgrade t)

;; Upgrade installed. In addition, delete obsolete (as quelpa doesn't do it itself)
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
                 (push pkg-desc delete-list))
                ((eq cmd ?I)
                 (push pkg-desc upgrade-list))))
        (forward-line)))
    (when (or delete-list upgrade-list)
      (condition-case nil
	  (package-menu-execute t)
	(error
	 (package-menu-execute))))))

(message "\n****************")
(if (not (or install-list upgrade-list delete-list error-list))
    (message "No packages to manage")
  (progn
    (message "* Installed %d packages:" (length install-list))
    (dolist (pack (reverse install-list))
      (message "  %S" pack))
    (message "* Upgraded %d packages:" (length upgrade-list))
    (dolist (pack (reverse upgrade-list))
      (message "  %S" (elt pack 1)))
    (message "* Removed %d obsolete packages:" (length delete-list))
    (dolist (pack (reverse delete-list))
      (message "  %S" (elt pack 1)))
    (message "* Error during install of %d packages:" (length error-list))
    (dolist (pack (reverse error-list))
      (message "  %S" pack))
    (setq install-list nil)
    (setq upgrade-list nil)
    (setq delete-list nil)
    (setq error-list nil)))

(provide 'ig-install-packages)

;;; ig-install-packages.el ends here
