;; From https://github.com/emacsmirror/bookmark-plus/blob/master/bookmark%2B-1.el


;; Desktop bookmarks
;;;###autoload (autoload 'bmkp-set-desktop-bookmark "bookmark+")
(defun bmkp-set-desktop-bookmark (desktop-file &optional nosavep)
                                        ; Bound globally to `C-x p K', `C-x r K', `C-x p c K'
  "Save the desktop as a bookmark.
You are prompted for the desktop-file location and the bookmark name.
The default value for the desktop-file location is the current value
of `desktop-base-file-name'.  As always, you can use `M-n' to retrieve
the default value.
With a prefix arg, set a bookmark to an existing desktop file - do not
save the current desktop.  Do not overwrite the file whose name you
enter, just use it to set the bookmark.
If you also use library Icicles, then the desktop files of all
existing desktop bookmarks are available during the desktop file-name
completion as proxy candidates.  To see them, use `C-M-_' to turn on
the display of proxy candidates."
  (interactive
   (progn (unless (condition-case nil (require 'desktop nil t) (error nil))
            (error "You must have library `desktop.el' to use this command"))
          (let ((icicle-proxy-candidates                     (and (boundp 'icicle-mode) icicle-mode
                                                                  (mapcar (lambda (bmk)
                                                                            (bookmark-prop-get
                                                                             bmk 'desktop-file))
                                                                          (bmkp-desktop-alist-only))))
                (icicle-unpropertize-completion-result-flag  t))
            (list (read-file-name (if current-prefix-arg
                                      "Use existing desktop file: "
                                    "Save desktop in file: ")
                                  nil (if (boundp 'desktop-base-file-name)
                                          desktop-base-file-name
                                        desktop-basefilename) ; Emacs < 22 name.
                                  current-prefix-arg)
                  current-prefix-arg))))
  (set-text-properties 0 (length desktop-file) nil desktop-file)
  (unless (file-name-absolute-p desktop-file) (setq desktop-file  (expand-file-name desktop-file)))
  (unless (or nosavep  (condition-case nil (require 'desktop nil t) (error nil)))
    (error "You must have library `desktop.el' to use this command"))
  (if nosavep
      (unless (bmkp-desktop-file-p desktop-file) (error "Not a desktop file: `%s'" desktop-file))
    (bmkp-desktop-save desktop-file))
  (let ((bookmark-make-record-function  (lexical-let ((df  desktop-file))
                                          (lambda () (bmkp-make-desktop-record df))))
        (current-prefix-arg             99)) ; Use all bookmarks for completion, for `bookmark-set'.
    (call-interactively #'bookmark-set)))

(defun bmkp-desktop-save (desktop-file)
  "Save current desktop in DESKTOP-FILE."
  (let ((desktop-basefilename     (file-name-nondirectory desktop-file)) ; Emacs < 22
        (desktop-base-file-name   (file-name-nondirectory desktop-file)) ; Emacs 23+
        (desktop-dir              (file-name-directory desktop-file))
        (desktop-restore-eager    t)    ; Don't bother with lazy restore.
        (desktop-globals-to-save  (bmkp-remove-if (lambda (elt) (memq elt bmkp-desktop-no-save-vars))
                                                  desktop-globals-to-save)))
    (cond ((< emacs-major-version 22)   ; Emacs 22 introduced `RELEASE' (locking).
           (desktop-save desktop-dir))
          ((or (< emacs-major-version 24)
               (and (= emacs-major-version 24)  (< emacs-minor-version 4)))
           (desktop-save desktop-dir 'RELEASE))
          (t                            ; Emacs 24.4 introduced `AUTOSAVE'.
           (desktop-save desktop-dir 'RELEASE 'AUTOSAVE)))
    (message "Desktop saved in `%s'" desktop-file)))

(defun bmkp-desktop-save-as-last ()
  "Save desktop to the file that is the value of `bmkp-desktop-current-file'.
Do nothing if any of these are true:
 * `desktop-save-mode' is non-nil
 * `bmkp-desktop-current-file' is nil
 * `bmkp-desktop-current-file' does not seem to be current (a non-bookmark
   desktop was last made current)
You might want to use this on `kill-emacs-hook'."
  (when (and (not desktop-save-mode)  bmkp-desktop-current-file
             (bmkp-same-file-p (desktop-full-file-name) bmkp-desktop-current-file))
    (bmkp-desktop-save bmkp-desktop-current-file)))

;;; (defun bmkp-desktop-file-p (file)
;;;   "Return non-nil if FILE is readable and appears to be a desktop file.
;;; FILE is a file-name string."
;;;   (and (stringp file)
;;;        (file-readable-p file)
;;;        (with-current-buffer (let ((enable-local-variables nil)) (find-file-noselect file))
;;;          (goto-char (point-min))
;;;          (and (zerop (forward-line 2))
;;;               (bmkp-looking-at-p "^;; Desktop File for Emacs$")))))

;; Similar to `icicle-file-desktop-p' in `icicles-fn.el'.
;; This is better than using `find-file-noselect', which visits the file and leaves its buffer.
(defun bmkp-desktop-file-p (filename)
  "Return non-nil if FILENAME names a desktop file."
  (when (consp filename) (setq filename  (car filename)))
  (and (stringp filename)
       (file-readable-p filename)
       (not (file-directory-p filename))
       (with-temp-buffer
         (insert-file-contents-literally filename nil 0 1000)
         (goto-char (point-min))
         (and (zerop (forward-line 2))
              (bmkp-looking-at-p "^;; Desktop File for Emacs"))))) ; No $, because maybe eol chars (e.g. ^M).

(defun bmkp-make-desktop-record (desktop-file)
  "Create and return a desktop bookmark record.
DESKTOP-FILE is the absolute file name of the desktop file to use."
  `(,@(bookmark-make-record-default 'NO-FILE 'NO-CONTEXT nil nil 'NO-REGION)
    (filename     . ,bmkp-non-file-filename)
    (desktop-file . ,desktop-file)
    (handler      . bmkp-jump-desktop)))

(defun bmkp-jump-desktop (bookmark)
  "Jump to desktop bookmark BOOKMARK.
Handler function for record returned by `bmkp-make-desktop-record'.
BOOKMARK is a bookmark name or a bookmark record."
  (let ((desktop-file  (bookmark-prop-get bookmark 'desktop-file)))
    (unless (condition-case nil (require 'desktop nil t) (error nil))
      (error "You must have library `desktop.el' to use this command"))
    ;; (unless desktop-file (error "Not a desktop-bookmark: %S" bookmark)) ; Shouldn't happen.
    (bmkp-desktop-change-dir desktop-file)
    (unless (bmkp-desktop-read desktop-file) (error "Could not load desktop file"))))

;; Derived from code in `desktop-change-dir'.
;;;###autoload (autoload 'bmkp-desktop-change-dir "bookmark+")
(defun bmkp-desktop-change-dir (desktop-file)
  "Change to desktop saved in DESKTOP-FILE.
Kill the desktop as specified by variables `desktop-save-mode' and
 `desktop-save' (starting with Emacs 22).
Clear the desktop and load DESKTOP-FILE."
  (interactive (list (let ((icicle-unpropertize-completion-result-flag  t))
                       (read-file-name "Change to desktop file: "))))
  (set-text-properties 0 (length desktop-file) nil desktop-file)
  (unless (file-name-absolute-p desktop-file) (setq desktop-file  (expand-file-name desktop-file)))
  (unless (condition-case nil (require 'desktop nil t) (error nil))
    (error "You must have library `desktop.el' to use this command"))
  (let ((desktop-basefilename     (file-name-nondirectory desktop-file)) ; Emacs < 22
        (desktop-base-file-name   (file-name-nondirectory desktop-file)) ; Emacs 23+
        (desktop-dir              (file-name-directory desktop-file))
        (desktop-restore-eager    t)    ; Don't bother with lazy restore.
        (desktop-globals-to-save  (bmkp-remove-if (lambda (elt) (memq elt bmkp-desktop-no-save-vars))
                                                  desktop-globals-to-save)))
    (bmkp-desktop-kill)
    (desktop-clear)
    (if (< emacs-major-version 22) (desktop-read) (desktop-read desktop-dir))))

;; Derived from code in `desktop-kill'.
(defun bmkp-desktop-kill ()
  "If `desktop-save-mode' is non-nil, do what `desktop-save' says to do.
In any case, release the lock on the desktop file.
This function does nothing in Emacs versions prior to Emacs 22."
  ;; We assume here: `desktop.el' has been loaded and `desktop-dirname' is defined.
  (when (and (boundp 'desktop-save-mode) ; Not defined in Emacs 20-21.
             desktop-save-mode
             (let ((exists  (file-exists-p (desktop-full-file-name))))
               (or (eq desktop-save t)
                   (and exists  (memq desktop-save '(ask-if-new if-exists)))
                   (and (or (memq desktop-save '(ask ask-if-new))
                            (and exists  (eq desktop-save 'ask-if-exists)))
                        (y-or-n-p "Save current desktop first? ")))))
    (condition-case err
        (if (or (< emacs-major-version 24)
                (and (= emacs-major-version 24)  (< emacs-minor-version 4)))
            (desktop-save desktop-dirname 'RELEASE)
          (desktop-save desktop-dirname 'RELEASE 'AUTOSAVE)) ; Emacs 24.4 introduced `AUTOSAVE'.
      (file-error (unless (yes-or-no-p "Error while saving the desktop.  IGNORE? ")
                    (signal (car err) (cdr err))))))
  ;; Just release lock, regardless of whether this Emacs process is the owner.
  (when (fboundp 'desktop-release-lock) (desktop-release-lock))) ; Not defined for Emacs 20.

;; Derived from code in `desktop-read'.
;;;###autoload (autoload 'bmkp-desktop-read "bookmark+")
(defun bmkp-desktop-read (file)
  "Load desktop-file FILE, then run `desktop-after-read-hook'.
Return t if FILE was loaded, nil otherwise."
  (interactive)
  (unless (file-name-absolute-p file) (setq file  (expand-file-name file))) ; Shouldn't happen.
  (setq desktop-dirname  (file-name-directory file))
  (if (not (file-readable-p file))
      nil                               ; Return nil, meaning not loaded.
    (let ((desktop-restore-eager      t) ; Don't bother with lazy restore.
          (desktop-first-buffer       nil)
          (desktop-buffer-ok-count    0)
          (desktop-buffer-fail-count  0)
          (desktop-save               nil)) ; Prevent desktop saving during eval of desktop buffer.
      (when (fboundp 'desktop-lazy-abort) (desktop-lazy-abort)) ; Emacs 22+.
      (load file t t t)
      (when (boundp 'desktop-file-modtime) ; Emacs 22+
        (setq desktop-file-modtime  (nth 5 (file-attributes file))))
      ;; `desktop-create-buffer' puts buffers at end of the buffer list.
      ;; We want buffers existing prior to evaluating the desktop (and not reused) to be placed
      ;; at the end of the buffer list, so we move them here.
      (mapc 'bury-buffer (nreverse (cdr (memq desktop-first-buffer (nreverse (buffer-list))))))
      (switch-to-buffer (car (buffer-list)))
      (run-hooks 'desktop-delay-hook)
      (setq desktop-delay-hook  ())
      (run-hooks 'desktop-after-read-hook)
      (when (boundp 'desktop-buffer-ok-count) ; Emacs 22+
        (message "Desktop: %d buffer%s restored%s%s." desktop-buffer-ok-count
                 (if (= 1 desktop-buffer-ok-count) "" "s")
                 (if (< 0 desktop-buffer-fail-count)
                     (format ", %d failed to restore" desktop-buffer-fail-count)
                   "")
                 (if (and (boundp 'desktop-buffer-args-list)  desktop-buffer-args-list)
                     (format ", %d to be restored lazily" (length desktop-buffer-args-list))
                   "")))
      t)))                              ; Return t, meaning successfully loaded.

;;;###autoload (autoload 'bmkp-desktop-delete "bookmark+")
(defun bmkp-desktop-delete (bookmark-name)
  "Delete desktop bookmark BOOKMARK-NAME, and delete its desktop file.
Release the lock file in that desktop's directory.
\(This means that if you currently have locked a different desktop
in the same directory, then you will need to relock it.)"
  (interactive (let ((alist  (bmkp-desktop-alist-only)))
                 (list (bmkp-read-bookmark-for-type "desktop" alist nil nil 'bmkp-desktop-history
                                                    "Delete "))))
  (let ((desktop-file  (bookmark-prop-get bookmark-name 'desktop-file)))
    (unless desktop-file (error "Not a desktop-bookmark: `%s'" bookmark-name))
    ;; Release the desktop lock file in the same directory as DESKTOP-FILE.
    ;; This will NOT be the right thing to do if a desktop file different from DESKTOP-FILE
    ;; is currently locked in the same directory.
    (let ((desktop-dir  (file-name-directory desktop-file)))
      (when (fboundp 'desktop-release-lock) (desktop-release-lock))) ; Not defined for Emacs 20.
    (when (file-exists-p desktop-file) (delete-file desktop-file)))
  (bookmark-delete bookmark-name))



(provide 'desktop-bookmarks)
