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

;;https://github.com/abo-abo/hydra/wiki/Dired
(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

(provide 'ig-hydra)

;;; ig-hydra.el ends here
