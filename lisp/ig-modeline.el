;;; ig-modeline.el --- Config modeline

;;; Commentary:
;; Configures the modeline.

;;; Code:



;; Built in - time
(use-package time
  :config
  (setq display-time-24hr-format t
	display-time-default-load-average nil)
  (display-time-mode 1))



;; https://github.com/nschum/window-numbering.el
(use-package window-numbering
  :config
  (window-numbering-mode t))



;; https://github.com/milkypostman/powerline
(use-package powerline
  :config
  (setq powerline-height 22
	powerline-default-separator 'wave)
  ;; Fix look and feel. Leave inactive as box
  (set-face-attribute 'mode-line nil :box nil))



;; https://github.com/TheBB/spaceline
(use-package spaceline-config
  :config
  (defface ig-spcln-hl-face
    `((t (:foreground "DeepSkyBlue" :background "grey20")))
    "Spaceline highlight face.")
  (defface ig-spcln-face1
    `((t (:foreground "SpringGreen" :background "grey11")))
    "Spaceline `face1' face.")
  (defface ig-spcln-face2
    `((t (:foreground "SandyBrown" :background "grey30")))
    "Spaceline `face2' face.")
  (defun ig-spaceline-face-func (face active)
    (pcase face
      (`face1 (if active 'ig-spcln-face1 'powerline-inactive1))
      (`face2 (if active 'ig-spcln-face2 'mode-line-inactive))
      (`line  (if active 'ig-spcln-hl-face 'powerline-inactive2))
      (`highlight (if active 'ig-spcln-hl-face 'powerline-inactive1))))
  (setq spaceline-face-func 'ig-spaceline-face-func)

  (setq spaceline-window-numbers-unicode t
	spaceline-workspace-numbers-unicode t
	spaceline-separator-dir-left '(right . right)
	spaceline-separator-dir-right '(left . left))
  (spaceline-toggle-hud-off)
  (spaceline-spacemacs-theme))



;; Set major modes names
(add-hook 'lisp-interaction-mode-hook (lambda () (setq mode-name "LI")))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "EL")))
(add-hook 'eshell-mode-hook (lambda () (setq mode-name "Esh")))
(add-hook 'isearch-mode-hook  (lambda () (diminish 'isearch-mode)))
;(setq projectile-mode-line '(:eval (if (condition-case nil
;                                          (and projectile-require-project-root
;                                                (projectile-project-root))
;                                         (error nil))
;				       " Ple" "")))

;; Built in
(use-package server
  :defer t
  :diminish server-buffer-clients)

(provide 'ig-modeline)

;;; ig-modeline.el ends here
