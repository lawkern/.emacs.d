;; law-config.el --- Simple configurations          -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author: Lawrence D. Kern

;;NOTE: increase cursor speed w/ key repeat->fast & delay until repeat->short

(setq law-work-code-style nil)

;; (setq initial-scratch-message ";; Lisp *scratch* Buffer\n\n")
(setq truncate-partial-width-windows t)
(setq ad-redefinition-action 'accept)
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(setq global-linum-mode nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)
;;(setq hscroll-step 1)
(setq gdb-many-windows t)
(setq gdb-show-main t)
(setq compilation-skip-threshold 1)
(setq compilation-context-lines 0)
(setq dabbrev-case-replace t)
(setq dabbrev-case-fold-search t)
(setq dabbrev-upcase-means-case-search t)
(setq split-window-preferred-function nil)
(setq split-width-threshold 0)
(setq split-height-threshold nil)
(setq show-paren-delay 0)
(setq gc-cons-threshold 1600000)
(setq use-package-always-ensure t)
;; (setq same-window-regexps '("."))
(setq same-window-regexps nil)
(setq font-lock-maximum-decoration 1)
(setq org-export-dispatch-use-expert-ui 1)

(setq cperl-indent-level 2)
(setq javascript-indent-level 2)
(setq js-indent-level 2)
(setq js-switch-indent-offset 2)
(setq js2-indent-level 2)
(setq css-indent-offset 2)

(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;; M-x shell should affect the currently-active window
(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)

(fset 'yes-or-no-p 'y-or-n-p)

;; (key-chord-mode 1)
(show-paren-mode 1)
;; (abbrev-mode 1)
(auto-insert-mode t)
(column-number-mode)
;; (global-undo-tree-mode nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(global-hl-line-mode -1)

(set-frame-parameter nil 'scroll-bar-background nil)
(windmove-default-keybindings)

(set-default 'truncate-lines t)
(set 'gdb-use-separate-io-buffer nil)
(set-fringe-mode 3)

(set-variable 'grep-command "grep -irHn ")

(global-set-key (kbd "C-;") 'execute-extended-command)
(global-set-key (kbd "C-c l") "λ")
(global-set-key (kbd "C-c r") 'query-replace)
(global-set-key (kbd "C-c s") 'ff-find-other-file)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c e") 'eval-buffer)
(global-set-key (kbd "C-c f") 'find-file-other-window)

(global-set-key (kbd "C-c i") 'hs-hide-block)
(global-set-key (kbd "C-c o") 'hs-show-block)
(global-set-key (kbd "C-c h") 'hs-hide-all)
(global-set-key (kbd "C-c o") 'hs-show-all)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-z") nil)

(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-l") 'windmove-right)

(global-set-key (kbd "<f7>") 'law-switch-to-minibuffer-window)

(setq law-font
      (cond
       ((member "Essential PragmataPro" (font-family-list)) "Essential PragmataPro-11")
       ((member "Px437 ATI 8x16" (font-family-list)) "Px437 ATI 8x16-16")
       ((member "Px437 ATI 8x8-2y" (font-family-list)) "Px437 ATI 8x8-2y-12")
       ((member "Input" (font-family-list)) "Input-11")
       ((member "Fira Code" (font-family-list)) "Fira Code-11")
       ((member "ProggyCleanTTSZ" (font-family-list)) "ProggyCleanTTSZ-16")
       ((member "Source Code Pro" (font-family-list)) "Source Code Pro-10")
       ((member "Meslo LG M" (font-family-list)) "Meslo LG M-12")
       (t "monospace")))

(setq line-spacing nil)
(set-frame-font law-font nil)
(add-to-list 'default-frame-alist `(font . ,law-font))

(when law-win32
  (setq compile-command "build.bat")
  (setq python-shell-interpreter "c:\\python27\\python.exe")
  (setq exec-path (append exec-path '("H:\\node\\;C:\\NASM\\")))
  (set-variable 'grep-command "findstr -s -n -i -l ")
  (setenv "PATH" (concat "c:\\node\\;c:\\NASM\\;" (getenv "PATH"))))

(when law-cygwin
  (setq compile-command "build.bat")
  (setenv "HOME" "/home/law")
  (let ((shellname "/bin/bash"))
    (setenv "ESHELL" shellname)
    (setenv "SHELL" shellname)
    (setq shell-file-name shellname))

  (load "comint")
  (setq comint-completion-addsuffix t)
  (setq comint-eol-on-send t)
  (setq law-font "Source Code Pro-9"))

(when law-osx
  (setq compile-command "sh build.sh")
  (setq mac-command-modifier 'meta)
  (setq mac-pass-command-to-system nil)
  (setq mac-command-key-is-meta t
        exec-path (append exec-path '("/usr/local/bin:/Users/law/.cargo/bin"))
        inferior-lisp-program "/applications/lang/cmucl/bin/lisp"
        geiser-racket-binary "/applications/lang/racket/bin/racket"
        slime-contribs '(slime-fancy))

  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq ns-use-proxy-icon  nil)
  (setq frame-title-format nil)
  (setq ns-use-native-fullscreen nil)


  (setq slime-lisp-implementations
        '((cmucl ("/applications/lang/cmucl/bin/lisp"))
          (sbcl ("sbcl"))))

  (setenv "PATH"
          (concat (getenv "PATH")
                  ":/usr/local/bin:/Users/law/.cargo/bin:/Library/TeX/texbin")))

(when law-work-code-style
  (add-hook 'js2-mode-hook 'law-fix-js-for-work)
  (add-hook 'html-mode-hook 'law-fix-html-for-work))

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(setq inferior-js-program-command "node --interactive")
(setenv "NODE_NO_READLINE" "1")

;; (add-to-list 'tooltip-frame-parameters '(internal-border-width . 10))
;; (add-to-list 'tooltip-frame-parameters '(border-width . 0))

(setq tooltip-frame-parameters
      '((border-width . 3)
        (internal-border-width . 10)
        (name . "tooltip")
        (no-special-glyphs . t)))

(setq whitespace-style
      '(face spaces tabs newline space-mark tab-mark newline-mark))

(setq whitespace-display-mappings
      '((space-mark 32 [183] [46])
        (newline-mark 10 [? 8629 10]) ;[8629 10]
        (tab-mark 9 [8594 9] [92 9])))

;; (setq fixme-modes '(law-mode))
;; bury *scratch* buffer instead of killing it
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

;; Inscrutable fix for jumping to compile errors in Clang:
(require 'compile)
(nth 5 (assoc 'gcc-include compilation-error-regexp-alist-alist))
;; (4 . 5)

(setf (nth 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)

;; Fix for D compile errors:
(add-to-list
 'compilation-error-regexp-alist
 '("^\\([^ \n]+\\)(\\([0-9]+\\)): \\(?:error\\|.\\|warnin\\(g\\)\\|remar\\(k\\)\\)"
   1 2 nil (3 . 4)))

(provide 'law-config)
