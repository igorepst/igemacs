;;; ig-keys.el --- Key bindings

;;; Commentary:
;; Custom key bindings



;;; Code:

(bind-keys :map isearch-mode-map
	   ("<up>" . isearch-ring-retreat)
	   ("<down>" . isearch-ring-advance)
	   ("<left>" . isearch-repeat-backward)
	   ("<right>" . isearch-repeat-forward))

;; cycle through buffers with Ctrl-Tab (like Firefox)
(bind-key "<C-tab>" 'bury-buffer)
;; toggle menu-bar visibility
(bind-key "<f12>" 'menu-bar-mode)
(bind-key "C-c C-e" 'eval-buffer)


(bind-key "C-c C-y" (lambda ()
		      (interactive)
		      (if (executable-find "sudo")
			  (find-file (read-file-name "Find Tramp file: " "/sudo::/"))
			(message "Please install sudo"))))

(bind-keys :map help-map
	   ;; find the source of a library
	   ("C-l" . find-library)
	   ;; find the source of a function definition
	   ("C-f" . find-function)
	   ;; jump to a command definition using a keybinding of the command
	   ("C-k" . find-function-on-key)
	   ;; find the source of a variable definition
	   ("C-v" . find-variable)
	   ("C-r" . describe-char))



;; Escape always quits
;; TODO: bind-key + C-x [escape]
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'keyboard-quit)


(provide 'ig-keys)

;;; ig-keys.el ends here
