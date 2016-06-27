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
    ;; ivy
    ivy
    swiper
    counsel
    ivy-hydra
    smex
    flx
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
  "List of packages that I like.")



;; Install or upgrade
;; install required
(dolist (package ig-packages-install-list)
  (unless (package-installed-p package)
    (ignore-errors
      (package-install package))))

;; upgrade installed
(save-window-excursion
  (package-list-packages t)
  (package-menu-mark-upgrades)
  (condition-case nil
      (package-menu-execute t)
    (error
     (package-menu-execute))))


(provide 'ig-install-packages)

;;; ig-install-packages.el ends here
