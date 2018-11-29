;;; law-mode.el --- Personal keybindings             -*- lexical-binding: t; -*-

(defun prev-window () (interactive) (other-window -1))

(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))

(defvar law-mode-map (make-sparse-keymap) "Keymap for 'law-mode'.")

(define-key law-mode-map (kbd "C-;") 'execute-extended-command)
(define-key law-mode-map (kbd "C-c l") "λ")
(define-key law-mode-map (kbd "C-c r") 'query-replace)
(define-key law-mode-map (kbd "C-c s") 'ff-find-other-file)
(define-key law-mode-map (kbd "C-c c") 'compile)
(define-key law-mode-map (kbd "C-c e") 'eval-buffer)
(define-key law-mode-map (kbd "C-c C-f") 'find-file-other-window)

(define-key law-mode-map (kbd "C-c i") 'hs-hide-block)
(define-key law-mode-map (kbd "C-c o") 'hs-show-block)
(define-key law-mode-map (kbd "C-c C-i") 'hs-hide-all)
(define-key law-mode-map (kbd "C-c C-o") 'hs-show-all)

(define-key law-mode-map (kbd "C-x g") 'magit-status)
(define-key law-mode-map (kbd "C-x M-g") 'magit-dispatch-popup)
(define-key law-mode-map (kbd "C-z") nil)
(define-key law-mode-map (kbd "C-x C-z") nil)

;; (define-key law-mode-map (kbd "C-<right>") #'other-window)
;; (define-key law-mode-map (kbd "C-<left>") #'prev-window)
;; (define-key law-mode-map (kbd "M-<right>") 'next-buffer)
;; (define-key law-mode-map (kbd "M-<left>") 'previous-buffer)

(define-key law-mode-map (kbd "M-h") 'windmove-left)
(define-key law-mode-map (kbd "M-j") 'windmove-down)
(define-key law-mode-map (kbd "M-k") 'windmove-up)
(define-key law-mode-map (kbd "M-l") 'windmove-right)

(define-key law-mode-map (kbd "<f7>") 'switch-to-minibuffer-window)

(defface font-lock-note
  '((t (:foreground "green" :underline t)))
  "NOTE comment highlighting"
  :group 'basic-faces)

(defface font-lock-todo
  '((t (:foreground "red" :underline t)))
  "TODO comment highlighting"
  :group 'basic-faces)

(defface font-lock-important
  '((t (:foreground "yellow" :underline t)))
  "IMPORTANT comment highlighting"
  :group 'basic-faces)

(setq law-mode-keywords
      '(("\\<\\(λ\\)" 1 'font-lock-constant-face t)
        ("\\<\\(NOTE\\)" 1 'font-lock-note t)
        ("\\<\\(TODO\\)" 1 'font-lock-todo t)
        ("\\<\\(IMPORTANT\\)" 1 'font-lock-important t)))

(define-minor-mode law-mode
  "A minor mode so that my key settings override major modes."
  :init-value t
  :lighter " law-mode"
  :keymap law-mode-map

  (if (bound-and-true-p law-mode)
      (font-lock-add-keywords nil law-mode-keywords)
    (font-lock-remove-keywords nil law-mode-keywords))

  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))

(define-globalized-minor-mode global-law-mode law-mode law-mode)

(add-to-list 'emulation-mode-map-alists `((law-mode . ,law-mode-map)))

(defun turn-off-law-mode () (law-mode -1))

(add-hook 'minibuffer-setup-hook #'turn-off-law-mode)

(provide 'law-mode)
