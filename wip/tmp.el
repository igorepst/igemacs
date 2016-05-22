;; Swap values
(setq a  (prog1 b (setq b  a)))

;; Fixed defadvice
;; https://github.com/bbatsov/prelude/issues/902
(require 'rect)
(defadvice kill-region (before smart-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end) rectangle-mark-mode)
     (list (line-beginning-position)
           (line-beginning-position 2)))))
