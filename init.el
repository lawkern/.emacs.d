;;; -*- lexical-binding: t; -*-

;; NOTE(law): Set global key bindings up top, so they still work if something
;; breaks later in the configuration.
(defun law-next-window ()
  (interactive)
  (other-window 1))

(defun law-prev-window ()
  (interactive)
  (other-window -1))

(defun law-switch-to-minibuffer ()
  "Switch to minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))


(global-set-key (kbd "C-;") 'execute-extended-command)
(global-set-key (kbd "C-,") 'law-next-window)
(global-set-key (kbd "C-<") 'law-prev-window)
(global-set-key (kbd "C-c b") 'revert-buffer)
(global-set-key (kbd "C-c e") 'eval-buffer)
(global-set-key (kbd "C-c s") 'ff-find-other-file)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-'")  'recompile)
(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "<f7>") 'law-switch-to-minibuffer)
(global-set-key (kbd "<f9>") 'previous-error)
(global-set-key (kbd "<f10>") 'next-error)
(global-set-key (kbd "<f12>") 'first-error)

;; NOTE(law): Any personal lisp code is added to the `code` directory inside
;; .emacs.d.
(add-to-list 'load-path (expand-file-name "code/" user-emacs-directory))

;; NOTE(law): Use a separate file for any "customize"-based settings.
(let ((custom-path (expand-file-name "code/custom.el" user-emacs-directory)))
  (setq custom-file custom-path)
  (when (file-exists-p custom-path)
    (load custom-file nil t)))

(menu-bar-mode -1) ; NOTE(law): Turn off regardless of display.

(defun law-customize-frame (frame)
  (when (display-graphic-p frame)
    ;; NOTE(law): Turn off the built-in GUI.
    (tool-bar-mode   -1)
    (scroll-bar-mode -1)
    (tooltip-mode    -1)

    ;; NOTE(law): Reduce the fringe size.
    (fringe-mode '(2 . 2))))

;; NOTE(law): Apply customization to all existing frames.
(mapc 'law-customize-frame (frame-list))

;; NOTE(law): Apply customization to future frames.
(add-hook 'after-make-frame-functions 'law-customize-frame)

  ;; NOTE(law): Reduce fringe width for all frames.

;; NOTE(law): Turn on relative line numbering.
;; (global-display-line-numbers-mode)
;; (setq-default display-line-numbers 'relative)
;; (setq display-line-numbers-type 'relative)

;; NOTE(law): Display the cursor's current column in the mode-line.
(column-number-mode 1)

;; NOTE(law): Delete trailing whitespace when saving a file.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; NOTE(law): Simplify the mode-line display.
(setq mode-line-format
      (list mode-line-front-space
            mode-line-mule-info
            mode-line-client
            mode-line-modified
            mode-line-remote
            mode-line-frame-identification
            mode-line-buffer-identification
            "   "
            mode-line-position
            "   "
            "("
            '(:eval mode-name)
            ")"
            "   "
            '(vc-mode vc-mode)
            "   "
            mode-line-misc-info
            mode-line-end-spaces))

(setq-default mode-line-format mode-line-format)

;; NOTE(law): Remove lag from displaying paren matches. The mode must be toggled
;; before the delay update will take effect.
(setq show-paren-delay 0)
(show-paren-mode 1)

;; NOTE(law): Set default font.
(add-to-list 'default-frame-alist '(font . "Iosevka Law-10"))

;; NOTE(law): Define custom font-lock faces.
(defface font-lock-operator-face
  '((t (:inherit font-lock-builtin-face)))
  "Highlighting for operators."
  :group 'law-faces)

(defface font-lock-assignment-face
  '((t (:inherit font-lock-warning-face)))
  "Highlighting for assignment operators."
  :group 'law-faces)

(defface font-lock-equality-face
  '((t (:inherit font-lock-operator-face)))
  "Highlighting for equality operators."
  :group 'law-faces)

(defface font-lock-type-decl-face
  '((t (:inherit font-lock-type-face :bold t)))
  "Highlighting for type names in declarations."
  :group 'law-faces)

(defface font-lock-function-decl-face
  '((t (:inherit font-lock-function-name-face :bold t)))
  "Highlighting for function names in declarations."
  :group 'law-faces)

(defface font-lock-note
  '((t (:inherit font-lock-comment-face :underline t :bold t :foreground "#00c06f")))
  "NOTE comment highlighting"
  :group 'law-faces)

(defface font-lock-todo
  '((t (:inherit font-lock-comment-face :underline t :bold t :foreground "#ff8059")))
  "TODO comment highlighting"
  :group 'law-faces)

(defface font-lock-important
  '((t (:inherit font-lock-comment-face :underline t :bold t :foreground "#c0c530")))
  "IMPORTANT comment highlighting"
  :group 'law-faces)

(defface font-lock-number-face
  '((t (:inherit font-lock-string-face)))
  "Highlighting for number constants."
  :group 'law-faces)

(let ((theme-path (expand-file-name "themes/" user-emacs-directory)))
  (add-to-list 'load-path theme-path)
  (add-to-list 'custom-theme-load-path theme-path))

;; NOTE(law): Configure the built-in modus color themes.
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-syntax '(faint))
(setq modus-themes-mode-line '())
(setq modus-themes-paren-match '(bold intense))
(setq modus-themes-region '(bg-only))

;; NOTE(law): Keep colors minimal in C-ish modes.
(setq font-lock-maximum-decoration '((c-mode . 1) (c++-mode . 1) (t . t)))

;; NOTE(law): Load any preferred color themes.
(load-theme 'drybones t nil)
(load-theme 'modus-vivendi t t)
(load-theme 'modus-operandi t t)

;; NOTE(law): Don't generate ~backup-files.
(setq make-backup-files nil)

;; NOTE(law): Don't bother creating #lock-files#.
(setq create-lockfiles nil)

;; NOTE(law): Don't use tabs for indentation.
(setq-default indent-tabs-mode nil)

;; NOTE(law): Make accidental tab ('	') characters obviously visible.
(setq whitespace-style '(face tabs))
(whitespace-mode 1)

;; NOTE(law): Store information about recently accessed files in a local temp
;; directory.
(setq recentf-save-file (expand-file-name "temp/recentf" user-emacs-directory))
(recentf-mode t)

;; NOTE(law): Use y/n for prompts, rather than typing out the full
;; "yes"/"no".
(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default fill-column 80)

;; NOTE(law): Open new buffers in existing windows.
(setq split-window-preferred-function nil)

;; NOTE(law): Never split windows vertically.
(setq split-width-threshold 0)
(setq split-height-threshold nil)

;; NOTE(law): Turn off the startup screen.
(setq inhibit-startup-screen      t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-message     t)

;; NOTE(law): Increase undo limit
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; TODO(law): It would be nice to require no packages at all, or at least store
;; a definitive local version for each one.
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

;; NOTE(law): Use evil-mode to emulate Vim keybindings.
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; NOTE(law): Use native Emacs undo instead of undo-tree.el.
(setq evil-undo-system 'undo-redo)

;; NOTE(law): Must be set before evil is included.
(setq evil-respect-visual-line-mode t)
(setq evil-toggle-key "C-`")
(setq evil-want-C-u-scroll nil)

(require 'evil)
(evil-mode 1)

(define-key evil-insert-state-map (kbd "C-;") 'evil-force-normal-state)
(define-key evil-normal-state-map (kbd "C-;") 'execute-extended-command)

;; NOTE(law): Use key-chords for entering normal mode.
(unless (package-installed-p 'key-chord)
  (package-install 'key-chord))

(setq key-chord-two-keys-delay 0.05)
(setq key-chord-one-keys-delay 0.1)

(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)

;; NOTE(law): Use ivy for minibuffer autocompletion.
(unless (package-installed-p 'ivy)
  (package-install 'ivy))

(require 'ivy)
(ivy-mode 1)

;; NOTE(law): Don't autocomplete over new directories names.
(setq ivy-magic-slash-non-match-action nil)

;; NOTE(law): Configure ivy minibuffer.
(setq ivy-height 25)
(setq ivy-wrap t)
(setq ivy-extra-directories '("../" "./"))
(setq ivy-display-style 'fancy)
(setq ivy-switch-buffer-map nil)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

;; NOTE(law): Configure ivy keymaps.
(define-key ivy-minibuffer-map (kbd "C-h") 'ivy-backward-delete-char)
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "C-l") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)

;; NOTE(law): Customize fundamental mode.
(add-hook 'fundamental-mode-hook
          #'(lambda ()
              ;; NOTE(law): Treat underscore_identifiers as words.
              (modify-syntax-entry ?_ "w")))

(defun law-highlight-comment-keywords ()
  (font-lock-add-keywords
   nil
   `(("\\<\\(NOTE\\)\\>\\(?:(\\(\\w*\\))\\)" ; NOTE(law): Test comment.
      (1 'font-lock-note prepend))

     ("\\<\\(TODO\\)\\>\\(?:(\\(\\w*\\))\\)" ; TODO(law): Test Comment.
      (1 'font-lock-todo prepend))

     ("\\<\\(IMPORTANT\\)\\>\\(?:(\\(\\w*\\))\\)" ; IMPORTANT(law): Test Comment.
      (1 'font-lock-important prepend)))))

(defun law-highlight-number-keywords ()
  (font-lock-add-keywords
   nil

   `(
     ;; Valid hex number (will highlight invalid suffix though)
     ("\\b0x[[:xdigit:]]+[uUlL]*\\b" . 'font-lock-number-face)

     ;; Invalid hex number
     ("\\b0x\\(\\w\\|\\.\\)+\\b" . font-lock-warning-face)

     ;; Valid floating point number.
     ("\\(\\b[0-9]+\\|\\)\\(\\.\\)\\([0-9]+\\(e[-]?[0-9]+\\)?\\([lL]?\\|[dD]?[fF]?\\)\\)\\b"
      (1 'font-lock-number-face)
      (2 'font-lock-number-face)
      (3 'font-lock-number-face))

     ;; Invalid floating point number. Must be before valid decimal.
     ;; ("\\b[0-9].*?\\..+?\\b" . font-lock-warning-face)

     ;; Valid decimal number. Must be before octal regexes otherwise 0 and 0l
     ;; will be highlighted as errors. Will highlight invalid suffix though.
     ("\\b\\(0\\|[1-9][0-9]*\\)\\([uUlL]*\\)\\b"
      (1 'font-lock-number-face)
      (2 'font-lock-number-face))

     ;; Valid octal number
     ("\\b0[0-7]+[uUlL]*\\b" . 'font-lock-number-face)

     ;; Floating point number with no digits after the period.  This must be
     ;; after the invalid numbers, otherwise it will "steal" some invalid
     ;; numbers and highlight them as valid
     ("\\b\\([0-9]+\\)\\." (1 'font-lock-number-face))

     ;; Invalid number. Must be last so it only highlights anything not matched
     ;; above.
     ("\\b[0-9]\\(\\w\\|\\.\\)+?\\b" . font-lock-warning-face)
     )))

;; NOTE(law): Customize programming-based modes.
(add-hook 'prog-mode-hook
          #'(lambda ()
              ;; NOTE(law): Turn off line-wrapping when in any programming-based
              ;; modes.
              (setq truncate-lines t)

              ;; NOTE(law): Highlight number constants.
              (law-highlight-number-keywords)

              ;; NOTE(law): Highlight keywords in comments.
              (law-highlight-comment-keywords)

              ;; NOTE(law): Treat underscore_identifiers as words.
              (modify-syntax-entry ?_ "w")))

;; NOTE(law): Display hidden files and use human readable format.
(setq law-dired-ls-switches "-lha")

(add-hook 'dired-mode-hook
          #'(lambda ()
              ;; NOTE(law): Show file metadata in dired mode.
              (dired-hide-details-mode 0)))

;; NOTE(law): Customize lisp-based modes.
(add-hook 'lisp-data-mode-hook
          #'(lambda ()
              ;; NOTE(law): Treat hyphenated-symbols as words
              (modify-syntax-entry ?- "w")))

;; NOTE(law): Customize compilation buffer.
(add-hook 'compilation-mode-hook
          #'(lambda ()
              (setq truncate-lines nil)
              (setq compilation-ask-about-save nil)
              (setq compilation-skip-threshold 1)
              (setq compilation-context-lines 0)))

;; NOTE(law): Customize cc-mode-based modes (c, c++, java, etc.).
(add-hook 'c-mode-common-hook
          #'(lambda ()
              ;; NOTE(law): Use // instead of /*
              (c-toggle-comment-style  -1)

              ;; NOTE(law): Use electric indentation.
              (c-toggle-electric-state -1)

              ;; NOTE(law): Insert newlines after special characters.
              ;; (c-toggle-auto-newline -1)

              ;; NOTE(law): Last turned off because insertion of parens into
              ;; existing expressions became annoying
              (electric-pair-local-mode 0)

              (electric-indent-local-mode 1)))

;; NOTE(law): Define a custom C-style for specific indentation and other
;; formatting rules.
(c-add-style
 "law"
 '("linux"
   (c-basic-offset . 3)
   (c-cleanup-list . nil)
   (c-hanging-semi&comma-criteria . ((lambda () 'stop)))
   (c-offsets-alist . ((case-label +)
                       (access-label 0)
                       (statement-cont 0)
                       (inline-open 0)))
   (c-echo-syntactic-information-p . t))
 nil)

;; NOTE(law): Use the custom C-style for other C-like modes.
(setq c-default-style '((csharp-mode . "law")
                        (java-mode   . "law")
                        (awk-mode    . "awk")
                        (other       . "law")))

;; NOTE(law): Define regexes for highlighting specific classes of C operators.
(setq law-c-self-assignment (regexp-opt '("<<=" ">>="  "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^=" "~=") t))
(setq law-c-bit-shift       (regexp-opt '("<<" ">>") t))
(setq law-c-comparison      (regexp-opt '("<" ">" "<=" ">=") t))
(setq law-c-equality        (regexp-opt '("==" "!=") t))
(setq law-c-assignment      (regexp-opt '("=") t))

(defun law-c-mode-hook ()
  ;; NOTE(law): Remove the standard CC-mode font lock keywords.
  (font-lock-add-keywords nil '() 'set)

  ;; NOTE(law): Only highlight specific identifiers that are helpful to
  ;; differentiate in C (Function declarations, type declarations, = vs ==,
  ;; etc.).

  (law-highlight-comment-keywords)
  (law-highlight-number-keywords)

  (font-lock-add-keywords
   nil
   `(
     ;; struct|union|enum
     ("\\<\\(struct\\|union\\|enum\\|extern\\)\\>"
      (1 'font-lock-keyword-face))

     ;; struct|union|enum Foo
     ("^\\(?:struct\\|union\\|enum\\)\\s-+\\(\\w*\\)\\s-*$"
      (1 'font-lock-type-decl-face))

     ;; typedef ... Foo;
     ("^typedef\\s-+\\(?:.*\\)\\s-+\\**\\(\\w*\\);"
      (1 'font-lock-type-decl-face))

     ;; typedef COMPUTE_TRANSLATION(Translate);
     ("^typedef\\s-+\\(.*\\)(\\(\\w*\\));"
      (1 'default)
      (2 'font-lock-type-decl-face))

     ;; typedef struct|union|enum Foo
     ("^typedef\\s-+\\(?:struct\\|union\\|enum\\)\\s-+\\(\\w*\\)$"
      (1 'font-lock-type-decl-face))

     ;; } Foo;
     ("^}\\s-+\\(\\w*\\);"
      (1 'font-lock-type-decl-face))

     ;; NOTE(law): Self assignment operators are matched before comparison, so <
     ;; and <= don't take precedence over <<=.
     (,law-c-self-assignment . 'font-lock-assignment-face)

     ;; NOTE(law): Equality operators are matched before assignment, so =
     ;; doesn't take precedence over == and !=.
     (,law-c-equality . 'font-lock-equality-face)

     ;; NOTE(law): Bit shift operators are matched before comparison, so <
     ;; doesn't take precedence over <<.
     (,law-c-bit-shift . 'default)

     ;; NOTE(law): Comparison operators are matched before assignment, so =
     ;; doesn't take precedence over <=.
     (,law-c-comparison . 'default)

     ;; NOTE(law): The = operator is matched after any other operators that
     ;; contain = (e.g. <=, ==, etc.).
     (,law-c-assignment . 'font-lock-assignment-face)

     ;; Macro function declaration
     ("^#define\\s-+\\(\\w+\\)(" (1 'font-lock-function-decl-face))

     ;; Function declaration
     ;; ("^\\(?:\\w+\\s-+\\|\*\\)*\\(\\w+\\)("
     ("^\\(__attribute__\\)"
      (1 'default))

     ("^\\(?:__attribute__((\\(?:\\w+\\|,\\s-\\)*))\\s-+\\)*\\(?:\\w+\\s-+\\|\*\\)*\\(\\w+\\)("
      (1 'font-lock-function-decl-face))
     )))

(add-hook 'c-mode-hook 'law-c-mode-hook)
(add-hook 'c++-mode-hook 'law-c-mode-hook)

(defun law-js-mode-hook ()
  (font-lock-add-keywords
   nil
   `(("\\bfunction\\s-*\\(\\w*\\)\("
      (1 'font-lock-function-decl-face))))

  (setq js-indent-level 2)
  (setq js-switch-indent-offset 2)
  (setq indent-tabs-mode nil)
  (setq-default tab-width 8))

(add-hook 'js-mode-hook 'law-js-mode-hook)

(defun law-css-mode-hook ()
  (setq css-indent-offset 2)
  (modify-syntax-entry ?- "w"))

(add-hook 'css-mode-hook 'law-css-mode-hook)

(defun law-go-mode-hook ()
  (setq-default tab-width 4)
  (modify-syntax-entry ?_ "w"))

(add-hook 'go-mode-hook 'law-go-mode-hook)


;; NOTE(law): Automatically insert comment headers in source code files.
(auto-insert-mode t)

(defun law-define-auto-inserts ()
  (define-auto-insert
    '("\\.\\(CC?\\|cc\\|cxx\\|cpp\\|c++\\|c\\|m\\)\\'" . "C/C++ skeleton")
    '(nil
      "/* /////////////////////////////////////////////////////////////////////////// */\n"
      "/* (c) copyright " (format-time-string "%Y")
      " Lawrence D. Kern /////////////////////////////////////// */\n"
      "/* /////////////////////////////////////////////////////////////////////////// */\n\n"
      _))

  (define-auto-insert
    '("\\.\\(HH?\\|hh\\|hxx\\|hpp\\|h++\\|h\\)\\'" . "Header skeleton")
    '(nil
      "#if !defined("
      (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name)))
      "_H)\n"
      "/* /////////////////////////////////////////////////////////////////////////// */\n"
      "/* (c) copyright " (format-time-string "%Y")
      " Lawrence D. Kern /////////////////////////////////////// */\n"
      "/* /////////////////////////////////////////////////////////////////////////// */\n\n"
      _ "\n\n"
      "#define " (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name))) "_H\n"
      "#endif")))

(add-hook 'after-init-hook 'law-define-auto-inserts)

;; NOTE(law): Configure Windows specific functionality.
(when (eq system-type 'windows-nt)
  ;; NOTE(law): Don't assume Windows will have a reasonable grep installed by
  ;; default.
  (require 'grep)
  (grep-apply-setting 'grep-command "findstr -snil ")

  ;; NOTE(law): Don't assume Windows will have a reasonable build system
  ;; installed by default.
  (setq compile-command "build.bat"))
