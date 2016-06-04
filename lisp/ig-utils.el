;;; ig-utils.el --- Utility functions

;;; Commentary:
;; Various utility functions

;;; Code:



(defun ig-save-buffers-kill-unconditionally ()
  "Save all work and kill Emacs without further questions."
  (interactive)
  (save-some-buffers t t)
  (recentf-save-list)
  (savehist-save)
  (persistent-scratch-save)
  (let (kill-emacs-hook)
    (kill-emacs)))



;; Open file with sudo. Warn on root
;; TODO: check sudo installed
;; http://stackoverflow.com/a/18951887/407953
(defun ig-find-alternative-file-with-sudo ()
  "Open the file with 'sudo'."
  (interactive)
  (let ((bname (expand-file-name (or buffer-file-name
                                     default-directory)))
        (pt (point)))
    (setq bname (or (file-remote-p bname 'localname)
                    (concat "/sudo::" bname)))
    (cl-flet ((server-buffer-done
               (buffer &optional for-killing)
               nil))
      (find-alternate-file bname))
    (goto-char pt)))

;;;###autoload
(defun ig-find-file-root-header-warning ()
  "Display a warning in header line of the current buffer.
This function is suitable to add to `find-file-hook'."
  (when (string-equal
         (file-remote-p (or buffer-file-name default-directory) 'user)
         "root")
    (let* ((warning "WARNING: EDITING AS ROOT!")
	   ;; TODO: center it
           (space (- (window-width) (length warning)))
           (bracket (make-string (/ space 2) ?-))
           (warning (concat bracket warning bracket)))
      (setq header-line-format
            (propertize  warning 'face '(:foreground "white" :background "red3"))))))



;; Save `kill-ring' to history
;;;###autoload
(defun ig-savehist-save-hook-func ()
  "Strip text props from `kill-ring' on history save to remove highlighting."
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))



(defun ig-goto-minibuffer ()
  "Go to minibuffer window, if it's active."
  (interactive)
  (let ((miniw (active-minibuffer-window)))
    (when miniw (select-window miniw))))


(provide 'ig-utils)

;;; ig-utils.el ends here
