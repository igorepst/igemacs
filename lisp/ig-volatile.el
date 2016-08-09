;;; ig-volatile.el --- Volatile data handling

;;; Commentary:
;; Configuration for volatile data handling (recent files, history, etc.)



;;; Code:

(defconst volatile-dir (expand-file-name "volatile" emacs-d))
(unless (file-directory-p volatile-dir)
  (make-directory volatile-dir t))

(setq recentf-save-file (expand-file-name ".recentf" volatile-dir)
      auto-save-list-file-prefix (expand-file-name ".saves-" volatile-dir)
      savehist-file (expand-file-name "savehist" volatile-dir)
      tramp-persistency-file-name (expand-file-name "tramp" volatile-dir)
      url-configuration-directory (expand-file-name "url" volatile-dir)
      url-cookie-file (expand-file-name "cookies" url-configuration-directory)
      server-auth-dir volatile-dir
      nsm-settings-file (expand-file-name "network-security.data"
					  volatile-dir)
      custom-file (expand-file-name "custom.el" volatile-dir)
      save-place-file (expand-file-name "places" volatile-dir))

(with-eval-after-load 'x-win
  (defun emacs-session-filename (session-id)
    (expand-file-name (concat "session." session-id) volatile-dir)))



;; Autoloads
(defconst generated-autoload-file-name "loaddefs.el")
(defconst generated-autoload-file (expand-file-name generated-autoload-file-name volatile-dir))
(add-to-list 'revert-without-query generated-autoload-file-name)

;; https://github.com/abo-abo/oremacs/blob/github/oleh/auto.el
;;;###autoload
(defun ig-update-all-autoloads ()
  "Update `autoload' in directories."
  (interactive)
  (cd emacs-d)
  (when (not (file-exists-p generated-autoload-file))
    (with-current-buffer (find-file-noselect generated-autoload-file)
      (insert ";;") ;; create the file with non-zero size to appease autoload
      (save-buffer)))
  (mapcar #'update-directory-autoloads
	  '("" "lisp")))

(add-hook 'kill-emacs-hook
	  (lambda() (ig-update-all-autoloads)))

(when (not (file-exists-p generated-autoload-file)) (ig-update-all-autoloads))
(load generated-autoload-file nil t)


(provide 'ig-volatile)

;;; ig-volatile.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
