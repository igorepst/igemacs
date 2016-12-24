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

(use-package dired
  :config
  (setq dired-recursive-copies 'always
	dired-recursive-deletes 'always
	dired-dwim-target t
	dired-use-ls-dired t
	;; dired-listing-switches (purecopy ig-ls-switches)
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
