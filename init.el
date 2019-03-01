(let ((file-name-handler-alist nil)) ; NOTE(law): Speed hack by excluding initial regexes
  (setq law-osx    (eq system-type 'darwin)
        law-win32  (eq system-type 'windows-nt)
        law-cygwin (eq system-type 'cygwin)
        law-linux  (eq system-type 'gnu/linux))

  (defun law-create-emacs-path (dirName)
    (concat user-emacs-directory
            (convert-standard-filename dirName)))

  (add-to-list 'load-path (law-create-emacs-path "lisp/"))
  (add-to-list 'load-path (law-create-emacs-path "themes/"))
  (add-to-list 'custom-theme-load-path (law-create-emacs-path "themes/"))

  (require 'law-config)
  (require 'law-lib)
  (require 'law-mode-line)
  ;; (require 'law-adv-mode-line)

  (load-theme 'glacier t nil)

  (setq package-archives
        '(("gnu"       . "http://elpa.gnu.org/packages/")
          ("original"  . "http://tromey.com/elpa/")
          ("org"       . "http://orgmode.org/elpa/")
          ("marmalade" . "http://marmalade-repo.org/packages/")
          ("melpa"     . "http://melpa.org/packages/")))

  (package-initialize)

  (unless package-archive-contents
    (package-refresh-contents))

  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  (require 'use-package)

  (use-package diminish :ensure t)

  (use-package evil
    :ensure
    :init
    (setq evil-toggle-key "")
    (setq evil-want-minibuffer nil)
    :config
    (evil-mode 1))

  (use-package paredit
    :ensure
    :diminish paredit-mode
    :bind (("RET" . law-electrify-return-if-match))
    :hook ((emacs-lisp-mode lisp-mode clojure-mode
                            lisp-interaction-mode scheme-mode)
           . paredit-mode)
    :config
    (paredit-mode t))

  (use-package eldoc
    :ensure
    :diminish eldoc-mode
    :config
    (eldoc-mode)
    (eldoc-add-command 'paredit-backward-delete 'paredit-close-round)
    (eldoc-add-command 'paredit-backward-delete 'paredit-close-round)
    (eldoc-add-command 'law-electrify-return-if-match))

  (use-package evil-paredit
    :ensure
    :hook (paredit-mode . evil-paredit-mode))

  (use-package ivy :ensure
    :diminish ivy-mode
    :bind (:map ivy-minibuffer-map
                ("C-h" . "DEL")
                ("C-j" . ivy-next-line)
                ("C-k" . ivy-previous-line)
                ("C-l" . ivy-alt-done)
                ("RET" . ivy-alt-done))
    :config
    (setq ivy-use-virtual-buffers t)
    (setq ivy-height 25)
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-extra-directories nil)

    :init
    (ivy-mode 1))

  ;; Used for smooth scrolling:
  ;; (use-package sublimity
  ;;   :ensure
  ;;   :config
  ;;   (require 'sublimity)
  ;;   (require 'sublimity-scroll)
  ;;   (sublimity-mode 1)
  ;;   (setq sublimity-scroll-weight 2
  ;;         sublimity-scroll-drift-length 1)
  ;;   (setq sublimity-auto-hscroll-mode 1))

  (use-package d-mode :ensure :defer t)

  (use-package tuareg :ensure :defer t
    :init
    ;; (setq tuareg-prettify-symbols-full t)
    (setq tuareg-indent-align-with-first-arg t))

  (use-package highlight-numbers :ensure :defer t)


  (defface visible-mark-active
    '((t (:foreground green :underline "green"))) "")

  ;; (use-package visible-mark :ensure :defer t
    ;; :init (global-visible-mark-mode 1))

  (use-package js2-mode :ensure :defer t
    :init
    (setq js2-mode-show-parse-errors nil)
    (setq js2-mode-show-strict-warnings nil)
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.cfc\\'" . js2-mode))

    :bind (("\C-x\C-e" . js-send-last-sexp)
           ("\C-\M-x"  . js-send-last-sexp-and-go)
           ("\C-cb"    . js-send-buffer)
           ("\C-c\C-b" . js-send-buffer-and-go)
           ("\C-cl"    . js-load-file-and-go)))

  (use-package geiser :ensure :defer t
    :hook (scheme-mode . geiser-mode))

  (use-package markdown-mode :defer t
    :init
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

  (use-package ox-reveal :ensure :defer t
    :init (setq org-reveal-title-slide nil))

  (use-package rust-mode :ensure :defer t)

  (use-package slack :defer t
    :commands (slack-start)
    :init
    (setq slack-buffer-emojify nil)
    (setq slack-prefer-current-team t)
    :config

    (when (file-exists-p "~/.emacs.d/lisp/law-slack.el")
      (load "~/.emacs.d/lisp/law-slack.el"))

    (evil-define-key 'normal slack-info-mode-map
      ",u" 'slack-room-update-messages)

    (evil-define-key 'normal slack-mode-map
      ",c" 'slack-buffer-kill
      ",ra" 'slack-message-add-reaction
      ",rr" 'slack-message-remove-reaction
      ",rs" 'slack-message-show-reaction-users
      ",pl" 'slack-room-pins-list
      ",pa" 'slack-message-pins-add
      ",pr" 'slack-message-pins-remove
      ",mm" 'slack-message-write-another-buffer
      ",me" 'slack-message-edit
      ",md" 'slack-message-delete
      ",u" 'slack-room-update-messages
      ",2" 'slack-message-embed-mention
      ",3" 'slack-message-embed-channel
      "\C-n" 'slack-buffer-goto-next-message
      "\C-p" 'slack-buffer-goto-prev-message)

    (evil-define-key 'normal slack-edit-message-mode-map
      ",k" 'slack-message-cancel-edit
      ",s" 'slack-message-send-from-buffer
      ",2" 'slack-message-embed-mention
      ",3" 'slack-message-embed-channel))


  (autoload 'key-chord-mode "key-chord")
  (autoload 'rainbow-mode "rainbow-mode" "Color current buffer" t)
  (autoload 'adaptive-wrap "adaptive-wrap" nil t)
  (autoload 'run-js "js-comint" "JS Repl" t)

  (add-to-list 'auto-mode-alist '("\\.metal\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cfm\\'" . html-mode))
  (add-to-list 'auto-mode-alist '("\\.cfc\\'" . js-mode))

  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'c-mode-common-hook 'law-fix-c-mode)
  (add-hook 'compilation-mode-hook 'law-compilation-mode-hook)
  (add-hook 'js2-mode-hook 'law-fix-c-mode)
  (add-hook 'rust-mode-hook 'law-fix-c-mode)
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  (add-hook 'prog-mode-hook 'law-add-comment-keywords)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'sh-mode-hook #'law-fix-sh-mode)

  (diminish 'undo-tree-mode)
  (diminish 'hs-minor-mode)
  (diminish 'abbrev-mode)

  ;; (when (equal (frame-parameter nil 'fullscreen) nil)
  ;;   (toggle-frame-fullscreen))

  ;; NOTE(law): this only works if emacs _starts_ in fullscreen (aka use -mm flag)
  (law-split-window))

;; Keep these out the let to prevent them from stacking up on evals
(eval-after-load 'autoinsert law-c-source-template)
(eval-after-load 'autoinsert law-c-header-template)
(eval-after-load 'autoinsert law-d-module-template)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("45f945caaeaa81a400d5b9b0232550185a91e4130db3af4a9c259399acf2b98f" default)))
 '(evil-insert-state-cursor (quote ((bar . 3) "#6ee2ff")) t)
 '(evil-motion-state-cursor (quote (box "#6ee2ff")) t)
 '(evil-normal-state-cursor (quote (box "#6ee2ff")) t)
 '(evil-operator-state-cursor (quote (box "#f76ed5")) t)
 '(evil-replace-state-cursor (quote (box "#f76ed5")) t)
 '(evil-visual-state-cursor (quote (box "#27f1bf")) t)
 '(global-visible-mark-mode t)
 '(package-selected-packages
   (quote
    (slack rust-mode diminish markdown-mode swift-mode ox-reveal pdf-tools tuareg d-mode sublimity highlight-numbers visible-mark ess typing use-package speed-type slime rainbow-mode key-chord js2-mode js-comint ivy geiser evil-paredit cider bongo auctex adaptive-wrap)))
 '(safe-local-variable-values (quote ((Lexical-binding . t)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
