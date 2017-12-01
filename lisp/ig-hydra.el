;;; -*- lexical-binding: t -*-
;;; ig-hydra.el --- Various hydras

;;; Commentary:
;; Various hydras

;;; Code:

(require 'hydra)

(defun ig-hydra-ivy-views-helper (arg)
  "Helper to call `ivy-set-view-recur' with ARG."
  (delete-other-windows)
  (ivy-set-view-recur arg))

(defhydra ig-hydra-ivy-views (:color teal)
    ("c" (ig-hydra-ivy-views-helper  `(horz
		      (file ,(expand-file-name "ig-init.el" ig-additional-lisp-dir))
		      (file "~/.emacs.d-various/.emacs.d-new-master/lisp"))) "copy config")
    ("o" (ig-hydra-ivy-views-helper `(horz
		      (file ,(expand-file-name "org-tutorial.org" ig-org-directory))
		      (sexp (eww "http://orgmode.org/guide/index.html#Top")))) "Org tutorial")
    ("SPC" nil))

(defvar ig-dired-sort-reverse-p nil)
(defhydra ig-hydra-dired-sort (:hint nil :color blue
				     :body-pre (setq ig-dired-sort-reverse-p nil))
  "
Sort dired by:
_n_: name         _t_: time
_s_: size         _r_: reverse
_e_: extension    _SPC_: quit
"
  ("n" (ig-dired-sort "-v" ig-dired-sort-reverse-p))
  ("s" (ig-dired-sort "-S" ig-dired-sort-reverse-p))
  ("e" (ig-dired-sort "-X" ig-dired-sort-reverse-p))
  ("t" (ig-dired-sort "-t" ig-dired-sort-reverse-p))
  ("r" (setq ig-dired-sort-reverse-p t) :color pink)
  ("SPC" nil))

(provide 'ig-hydra)

;;; ig-hydra.el ends here
