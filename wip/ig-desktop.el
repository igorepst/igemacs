(defconst ig-desktop-dir (expand-file-name "desktop/" emacs-d)
  "Base directory to save desktop files.")

(defconst ig-desktop-file-name (convert-standard-filename "emacs.desktop")
  "File name to save desktop to.")

(defun ig-desktop-create-dir (dir-path)
  (unless (file-exists-p dir-path)
    (make-directory dir-path)))

(defun ig-desktop-save-helper (desktop-path)
  (let ((desktop-base-file-name ig-desktop-file-name))
    (desktop-save desktop-path t t))
  (message "Desktop saved in '%s'" desktop-path))

(defun ig-desktop-restore-helper (desktop-path)
  (let ((desktop-base-file-name ig-desktop-file-name)
	(desktop-restore-eager t))
    (desktop-read desktop-path)))

(defun ig-desktop-get-name ()
  (ig-desktop-create-dir ig-desktop-dir)
  (let ((desk-dir (read-directory-name "Choose desktop name:" ig-desktop-dir nil nil)))
    (ig-desktop-create-dir desk-dir)
    desk-dir))

(defun ig-desktop-save ()
  (interactive)
  (ig-desktop-save-helper (ig-desktop-get-name)))

(defun ig-desktop-restore ()
  (interactive)
  (ig-desktop-restore-helper (ig-desktop-get-name)))

(provide 'ig-desktop)
