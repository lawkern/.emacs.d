;; law-config.el --- Simple configurations          -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author: Lawrence D. Kern

;;NOTE: increase cursor speed w/ key repeat->fast & delay until repeat->short

(setq law-work-code-style nil)

(setq law-modes '(c-mode d-mode c++-mode objc-mode emacs-lisp-mode lisp-mode
                         scheme-mode html-mode nxml-mode js-mode js2-mode
                         ruby-mode))

(setq initial-scratch-message ";; Lisp *scratch* Buffer\n\n")
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
(setq gc-cons-threshold 100000000)
(setq use-package-always-ensure t)

(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(setq c-default-style "linux" c-basic-offset 2)
(setq c-basic-offset 2)
(setq cperl-indent-level 2)

(setq javascript-indent-level 2)
(setq js-indent-level 2)
(setq js2-indent-level 2)
(setq js-switch-indent-offset 2)

(setq css-indent-offset 2)

(setq compile-command "sh build.sh")

(fset 'yes-or-no-p 'y-or-n-p)

;; (key-chord-mode 1)
(show-paren-mode 1)
(abbrev-mode 1)
(auto-insert-mode t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'c-mode-common-hook 'law-fix-c-mode)
(add-hook 'compilation-mode-hook 'law-compilation-mode-hook)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(set-frame-parameter nil 'scroll-bar-background nil)
(windmove-default-keybindings)
(global-hl-line-mode -1)

(set-default 'truncate-lines t)
(set 'gdb-use-separate-io-buffer nil)
(set-fringe-mode 3)

(set-variable 'grep-command "grep -irHn ")

(setq law-font "Source Code Pro-9")
;; (setq law-font "ProggyCleanTTSZ-12")
;; (setq law-font "Source Code Pro Semibold-10")
;; (setq law-font "Meslo LG M-12")

(set-frame-font law-font nil)
(add-to-list 'default-frame-alist `(font . ,law-font))

(when law-win32
  (set-variable 'grep-command "findstr -s -n -i -l ")
  (setq compile-command "build.bat")
  (setenv "PATH" (concat (getenv "PATH") ";H:\\node\\;C:\\jdk1.8.0_162;C:\\jdk1.8.0_162\\bin;C:\\NASM\\"))
  (setenv "JRE_HOME" "C:\\jdk1.8.0_162\\jre")
  (setq python-shell-interpreter "c:\\python27\\python.exe")
  (setq exec-path (append exec-path '("H:\\node\\;C:\\NASM\\"))))

(when law-osx
  (setq mac-pass-command-to-system nil)
  (setq mac-command-key-is-meta t
        exec-path (append exec-path '("/usr/local/bin"))
        inferior-lisp-program "/applications/lang/cmucl/bin/lisp"
        geiser-racket-binary "/applications/lang/racket/bin/racket"
        slime-contribs '(slime-fancy))

  (setq slime-lisp-implementations
        '((cmucl ("/applications/lang/cmucl/bin/lisp"))
          (sbcl ("sbcl"))))

  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Library/TeX/texbin")))

(when law-work-code-style
  (add-hook 'js2-mode-hook 'law-fix-js-for-work)
  (add-hook 'html-mode-hook 'law-fix-html-for-work))

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(setq inferior-js-program-command "node --interactive")
(setenv "NODE_NO_READLINE" "1")

(add-to-list 'tooltip-frame-parameters '(internal-border-width . 10))

(setq whitespace-style
      '(face spaces tabs newline space-mark tab-mark newline-mark))

(setq whitespace-display-mappings
      '((space-mark 32 [183] [46])
        (newline-mark 10 [? 8629 10]) ;[8629 10]
        (tab-mark 9 [8594 9] [92 9])))

;; (setq fixme-modes '(law-mode))
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

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(λ\\)" 1 'font-lock-constant-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note t)
           ("\\<\\(TODO\\)" 1 'font-lock-todo t)
           ("\\<\\(IMPORTANT\\)" 1 'font-lock-important t))))
      law-modes)

;; bury *scratch* buffer instead of kill it
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
