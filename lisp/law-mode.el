;;; law-mode.el --- Personal keybindings             -*- lexical-binding: t; -*-


(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-z") nil)

(dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]))
  (global-unset-key k))

(defvar law-bind-mode-map (make-sparse-keymap)
  "Keymap for 'law-bind-mode'.")

;; (define-key law-bind-mode-map (kbd "C-<right>") #'other-window)
;; (define-key law-bind-mode-map (kbd "C-<left>") #'prev-window)
;; (define-key law-bind-mode-map (kbd "M-<right>") 'next-buffer)
;; (define-key law-bind-mode-map (kbd "M-<left>") 'previous-buffer)

(define-key law-bind-mode-map (kbd "M-h") 'windmove-left)
(define-key law-bind-mode-map (kbd "M-j") 'windmove-down)
(define-key law-bind-mode-map (kbd "M-k") 'windmove-up)
(define-key law-bind-mode-map (kbd "M-l") 'windmove-right)


(defun prev-window () (interactive) (other-window -1))

(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))

(global-set-key (kbd "<f7>") 'switch-to-minibuffer-window)

;;JS

;;Editing
(define-key law-bind-mode-map (kbd "C-x C-m") 'execute-extended-command)
(define-key law-bind-mode-map (kbd "C-c C-m") 'execute-extended-command)
(define-key law-bind-mode-map (kbd "C-c l") "λ")
(define-key law-bind-mode-map (kbd "C-c r") 'query-replace)
(define-key law-bind-mode-map (kbd "C-c s") 'ff-find-other-file)
(define-key law-bind-mode-map (kbd "C-c c") 'compile)
(define-key law-bind-mode-map (kbd "C-c e") 'eval-buffer)

(define-key law-bind-mode-map (kbd "C-c i") 'hs-hide-block)
(define-key law-bind-mode-map (kbd "C-c o") 'hs-show-block)
(define-key law-bind-mode-map (kbd "C-c C-i") 'hs-hide-all)
(define-key law-bind-mode-map (kbd "C-c C-o") 'hs-show-all)


(define-minor-mode law-bind-mode
  "A minor mode so that my key settings override major modes."
  :init-value t
  :lighter " law-bind-mode"
  :keymap law-bind-mode-map)

(define-globalized-minor-mode global-law-bind-mode law-bind-mode law-bind-mode)

(add-to-list 'emulation-mode-map-alists `((law-bind-mode . ,law-bind-mode-map)))


(defun turn-off-law-bind-mode ()
  "Turn off law-bind-mode."
  (law-bind-mode -1))
(add-hook 'minibuffer-setup-hook #'turn-off-law-bind-mode)

(provide 'law-mode)
