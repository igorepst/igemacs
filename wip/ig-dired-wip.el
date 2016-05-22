
(defun ig-open-dired-2pane ()
  (interactive)
  (select-frame (make-frame))
  (set-frame-parameter nil 'fullscreen 'maximized)
  (dired emacs-d)
  (goto-line 1)
  (split-window-horizontally)
  ;(other-buffer 1)
  (find-file-other-window additional-lisp-dir)
;(dired-other-window emacs-d)
  )

(provide 'ig-dired-wip)
