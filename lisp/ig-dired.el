;;; ig-dired.el --- Dired configuration

;;; Commentary:
;; Configuration of dired mode.

;;; Code:

;; Note that the space used in time style format is actually a Unicode
;; char U+2008 PUNCTUATION SPACE to overcome a bug
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=18875
(defconst ig-time-style-space (string ?\u2008) "Punctuation space Unicode char.")
(defconst ig-ls-switches (concat
			  "--group-directories-first --time-style=+%d/%m/%y"
			  ig-time-style-space "%R -AhFl") "'ls' switches.")

;;;###autoload
(defun ig-dired-sort (variant &optional reverse)
  "Sort dired by VARIANT, possibly in REVERSE order.
The sorting mode will be used from now on."
  (let ((switches (concat (purecopy ig-ls-switches) " " variant)))
    (when reverse (setq switches (concat "--reverse " switches)))
    (setq dired-listing-switches switches)
    (when (eq major-mode 'dired-mode)
      (dired-sort-other switches))))

(use-package dired-rainbow
  :defer t
  :config
  (defconst ig-images-files-extensions
    '("bmp" "jpg" "jpeg" "png" "gif" "tif" "tiff" "psd" "psp" "wmf" "emf" "ppm" "pgm" "pbm" "pnm" "svg" "ai" "eps" "ico")
    "Images files.")
  (defconst ig-archives-files-extensions
    '("rar" "zip" "7z" "sqx" "gz" "tgz" "tar" "ace" "arj" "lha" "uc2" "lzma" "bz2" "z" "uc2")
    "Archives files.")
  (defconst ig-exec-files-extensions
    '("exe" "com" "jar" "bat" "cmd" "ahk" "btm" "vbs" "vbe" "js" "jse" "wsf" "wsh" "msi")
    "Executable files.")
  (defconst ig-docs-files-extensions
    '("doc" "docx" "odt" "xls" "xlsx" "ods" "pdf" "djvu")
    "Document files.")
  (defconst ig-media-files-extensions
    '("avi" "vob" "mpg" "mpeg" "mkv" "mp4" "wmv" "webm" "flv" "ogv" "ogm" "divx" "m2v" "h264" "aac" "flac" "mp3" "wma" "mp4" "m4a" "mpa" "wav" "mid" "ac3" "mka" "cda")
    "Media files.")
  (setq dired-rainbow-date-regexp (concat "\\(?:[0-3][0-9]/[0-1][0-9]/[0-9][0-9]" ig-time-style-space "[0-2][0-9]:[0-5][0-9]\\)"))
  (dired-rainbow-define ig-media-files-extensions "#6fb7d8" ig-media-files-extensions)
  (dired-rainbow-define ig-images-files-extensions "#9397b7" ig-images-files-extensions)
  (dired-rainbow-define ig-archives-files-extensions "#9fb696" ig-archives-files-extensions)
  (dired-rainbow-define ig-docs-files-extensions "#21a184" ig-docs-files-extensions)
  (dired-rainbow-define ig-exec-files-extensions "#d787a5" ig-exec-files-extensions)
  (dired-rainbow-define-chmod ig-exec-files-chmod "#d787a5" "-.*x.*")
  (dired-rainbow-define-chmod ig-dirs-chmod "#00aef4" "d"))

(use-package dired
  :defer t
  :config
  (setq dired-recursive-copies 'always
	dired-recursive-deletes 'always
	dired-dwim-target t
	dired-use-ls-dired t
	;; We MUST now override the following regexp.
	;; There is a regular space in its end
	directory-listing-before-filename-regexp
	(purecopy (concat "\\([0-2][0-9]:[0-5][0-9] \\)\\|"
			  directory-listing-before-filename-regexp)))
  (defun ig-dired-sort-set-mode-line (args)
    "Override mode name. ARGS are not used"
    (setq mode-name
	  (concat
	   (cond ((string-match-p
		   "-v$" dired-actual-switches)
		  "Dir name")
		 ((string-match-p
		   "-t$" dired-actual-switches)
		  "Dir time")
		 ((string-match-p
		   "-S$" dired-actual-switches)
		  "Dir size")
		 ((string-match-p
		   "-X$" dired-actual-switches)
		  "Dir ext")
		 (t
		  (concat "Dired " dired-actual-switches)))
	   (if (string-match-p "^--reverse" dired-actual-switches)
	       " ↑" " ↓")))
    (force-mode-line-update))
  (advice-add 'dired-sort-set-mode-line :around #'ig-dired-sort-set-mode-line)
  (ig-dired-sort "-v" nil)
  (defun ig-dired-home-dir ()
    (interactive)
    (dired (getenv "HOME")))
  ;; (defun ig-dired-open-parent ()
  ;;   (interactive)
  ;;   (find-alternate-file ".."))

  (require 'dired-rainbow)

  :bind
  (("S-<f1>" . ig-dired-home-dir)
   :map dired-mode-map
   ("|" . ig-hydra-dired-sort/body)
   ;; ("<RET>" . dired-find-alternate-file)
   ;; ("^" . ig-dired-open-parent)
   ;; ("<left>" . ig-dired-open-parent)
   ;; ("<right>" . dired-find-alternate-file)
   ))

(provide 'ig-dired)

;;; ig-dired.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp emacs-lisp-checkdoc)
;; End:
