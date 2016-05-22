;;; ig-hydra.el --- Various hydras

;;; Commentary:
;; Various hydras

;;; Code:

(require 'hydra)
(require 'hydra-examples)

(setq lv-use-separator t)



(defhydra ig-hydra-matching-lines (:hint nil :color blue)
  "
Matching lines:
_o_: occur       _d_: delete dups      _e_: reverse region
_f_: flush       _s_: sort             _h_: highlight
_k_: keep        _r_: reverse sort     _SPC_: quit
"
  ("o" occur)
  ("f" (call-interactively 'ig-delete-matching-lines))
  ("k" (call-interactively 'ig-delete-non-matching-lines))
  ("d" ig-delete-adjacent-duplicate-lines)
  ("s" ig-sort-lines)
  ("r" (ig-sort-lines t))
  ("e" ig-reverse-region)
  ("h" highlight-lines-matching-regexp)
  ("SPC" nil))



(defhydra ig-hydra-windows (:color pink
				   :hint nil)
  "
      ^Move          ^^^^^Swap      ^^^Resize                  ^^^^^^Del
      _<up>_            ^^^_k_         ^^_K_      ^_r_: new ⮞     _d_: cur     _SPC_: quit
_<left>_   _<right>_    _h_   _l_     _H_   _L_    _n_: new ⮟     _x_: buf
     _<down>_           ^^^_j_         ^^_J_      ^_b_: bury buf  _o_: other
"
  ("<left>" windmove-left)
  ("<down>" windmove-down)
  ("<up>" windmove-up)
  ("<right>" windmove-right)
  ("h" buf-move-left)
  ("j" buf-move-down)
  ("k" buf-move-up)
  ("l" buf-move-right)
  ("H" hydra-move-splitter-left)
  ("J" hydra-move-splitter-down)
  ("K" hydra-move-splitter-up)
  ("L" hydra-move-splitter-right)
  ("r" (lambda () (interactive) (split-window-right)(windmove-right)) :color blue)
  ("n" (lambda () (interactive) (split-window-below)(windmove-down)) :color blue)
  ("d" delete-window :color blue)
  ("o" delete-other-windows :color blue)
  ("x" kill-this-buffer :color blue)
  ("b" bury-buffer :color blue)
  ("SPC" hydra-keyboard-quit :color blue))


(provide 'ig-hydra)

;;; ig-hydra.el ends here
