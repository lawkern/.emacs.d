;;; law.el --- Just a bunch of utilities and helper functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author: Lawrence D. Kern

(defun law-switch-to-minibuffer-window ()
  "Switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))

(defun prev-window ()
  (interactive)
  (other-window -1))

(defun law-split-window ()
  "Split the frame horizontally based on the frame's width."
  (interactive)
  (delete-other-windows)
  (message "frame width: %d\n" (frame-width))
  (let ((max-window-width 100))
    (dotimes (i (- (/ (frame-width) max-window-width) 1))
      (split-window-horizontally)))
  (balance-windows))

(defun law-set-frame-opacity (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nSet frame opacity [0:100]:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun law-shorten-directory (dir max-length)
  "Show up to max-length of a directory name"
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(defun law-compilation-mode-hook ()
  (local-set-key (kbd "h") nil)
  (setq truncate-lines nil) ;; automatically becomes buffer local
  (set (make-local-variable 'truncate-partial-width-windows) nil))

(defun law-rename-file-and-buffer (new-name)
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(defun law-move-buffer-file (dir)
  (interactive "sNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (copy-file filename newname 1)
      (delete-file filename)
      (set-visited-file-name newname)
      (set-buffer-modified-p nil)
      t)))

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

(setq law-comment-keywords
      '(("\\<\\(λ\\)" 1 'font-lock-constant-face t)
        ("\\<\\(NOTE\\)" 1 'font-lock-note t)
        ("\\<\\(TODO\\)" 1 'font-lock-todo t)
        ("\\<\\(IMPORTANT\\)" 1 'font-lock-important t)))

(defun law-add-comment-keywords ()
  ;; (if (bound-and-true-p law-mode)
  ;;    (font-lock-add-keywords nil law-mode-keywords)
  ;;  (font-lock-remove-keywords nil law-mode-keywords))

  ;; (if (fboundp 'font-lock-flush)
  ;;    (font-lock-flush)
  ;;  (when font-lock-mode
  ;;    (with-no-warnings (font-lock-fontify-buffer))))

  (font-lock-add-keywords nil law-comment-keywords))


(defvar law-electrify-return-match "[\]}\)\"]")

(defun law-electrify-return-if-match (arg)
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at law-electrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(defun law-insert-c-separator ()
  "Add a source code separator using a C-style comment."
  (interactive)
  (insert "\n/*")
  (insert-char ?/ (- fill-column 3))
  (insert "*/\n"))

;; Mode Fixes

(defun law-fix-c-mode ()
  ;; nil
  (interactive)
  ;; (hs-minor-mode)
  (law--fix-c-indentation)
  (local-set-key (kbd "C-c C-c") #'compile)
  (local-set-key (kbd "C-c -") #'law-insert-c-separator)
  (law--fix-c-font-lock)
  ;; (local-set-key (kbd "RET") 'law-electrify-return-if-match)
  ;; (highlight-numbers-mode)
  (message "c-mode was fixed\n"))

(defun law--fix-c-indentation ()
  (setq c-default-style "linux")
  (setq c-basic-offset 3)
  (setq comment-style 'indent)
  (setq comment-start "//")
  (setq comment-end "")
  (c-set-offset 'case-label '+)
  ;; (c-set-offset 'access-label 0)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'label '+)
  (c-set-offset 'statement-cont 0)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'inline-open 0)
  ;; (c-set-offset 'cpp-macro 0)
  (c-set-offset 'arglist-close 0))

(defun law-fix-sh-mode ()
  (local-set-key (kbd "C-c C-c") #'compile))

(defface font-lock-operator-face
  '((t (:foreground "blue")))
  "Basic face for operators."
  :group 'basic-faces)

;; (setq law-c-types '("u8 u16 u32 u64 s8 s16 s32 s64 b32 r32 r64"))
;; (setq law-c-builtin '("global_variable" "local_persist" "internal"))
(setq law-c-operators '("===" "!==" "==" "!="
                        ;; "===" "+=" "-=" "->" "--" "++"
                        ;; "*" "/" "~" "&" "|" "%" "+" "-"
                        ;; "<" ">" "." "=" "," ";" "[" "]" "(" ")"
                        ))

;; (setq law-c-types-regex (regexp-opt law-c-types))
;; (setq law-c-builtin-regex (regexp-opt law-c-builtin 'words))
(setq law-c-operators-regex (regexp-opt law-c-operators))

(defun law--fix-c-font-lock ()
  (font-lock-add-keywords
   nil
   `((,law-c-operators-regex . 'font-lock-operator-face)
     ;; (,law-c-types-regex . 'font-lock-type-face)
     ;; (,law-c-builtin-regex . 'font-lock-builtin-face)
     )))

;; (defun law--add-c-keywords ()
;;   (font-lock-add-keywords
;;    nil
;;    `((,law-c-builtin-regex 0 'font-lock-builtin-face)

;;      ;; Valid hex number (will highlight invalid suffix though)
;;      ("\\b0x[[:xdigit:]]+[uUlL]*\\b" . font-lock-string-face)

;;      ;; Invalid hex number
;;      ("\\b0x\\(\\w\\|\\.\\)+\\b" . font-lock-warning-face)

;;      ;; Valid floating point number.
;;      ("\\(\\b[0-9]+\\|\\)\\(\\.\\)\\([0-9]+\\(e[-]?[0-9]+\\)?\\([lL]?\\|[dD]?[fF]?\\)\\)\\b"
;;       (1 font-lock-string-face)
;;       (3 font-lock-string-face))

;;      ;; Invalid floating point number.  Must be before valid decimal.
;;      ("\\b[0-9].*?\\..+?\\b" . font-lock-warning-face)

;;      ;; Valid decimal number.  Must be before octal regexes otherwise 0 and 0l
;;      ;; will be highlighted as errors.  Will highlight invalid suffix though.
;;      ("\\b\\(\\(0\\|[1-9][0-9]*\\)[uUlL]*\\)\\b" 1 font-lock-string-face)

;;      ;; Valid octal number
;;      ("\\b0[0-7]+[uUlL]*\\b" . font-lock-string-face)

;;      ;; Floating point number with no digits after the period.  This must be
;;      ;; after the invalid numbers, otherwise it will "steal" some invalid
;;      ;; numbers and highlight them as valid
;;      ("\\b\\([0-9]+\\)\\." (1 font-lock-string-face))

;;      ;; Invalid number.  Must be last so it only highlights anything not
;;      ;; matched above.
;;      ("\\b[0-9]\\(\\w\\|\\.\\)+?\\b" . font-lock-warning-face)

;;      ;; Function call
;;      ;; ("\\(\\w+\\)\\s-*\(" (1 font-lock-function-name-face))

;;      )))

(defun law-fix-html-for-work ()
  (setq indent-tabs-mode t
        comment-start "<!---"
        comment-end "--->"))

(defun law-fix-js-for-work ()
  (setq indent-tabs-mode t))

(setq law-c-source-template
      '(define-auto-insert
         '("\\.\\(CC?\\|cc\\|cxx\\|cpp\\|c++\\|c\\|m\\)\\'" . "C/C++ skeleton")
         '(nil
           "/*/////////////////////////////////////////////////////////////////////////////*/\n"
           "/* (c) copyright " (format-time-string "%Y")
           " Lawrence D. Kern ////////////////////////////////////////*/\n"
           "/*/////////////////////////////////////////////////////////////////////////////*/\n\n"
           _)))

(setq law-c-header-template
      '(define-auto-insert
         '("\\.\\(HH?\\|hh\\|hxx\\|hpp\\|h++\\|h\\)\\'" . "Header skeleton")
         '(nil
           "#if !defined("
           (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name)))
           "_H)\n"
           "/*/////////////////////////////////////////////////////////////////////////////*/\n"
           "/* (c) copyright " (format-time-string "%Y")
           " Lawrence D. Kern ////////////////////////////////////////*/\n"
           "/*/////////////////////////////////////////////////////////////////////////////*/\n\n"
           _ "\n\n"
           "#define " (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name))) "_H\n"
           "#endif")))

(setq law-d-module-template
      '(define-auto-insert
         '("\\.\\(d\\)\\'" . "D module skeleton")
         '(nil
           "///////////////////////////////////////////////////////////////////////////////////\n"
           "// (c) copyright " (format-time-string "%Y") " Lawrence D. Kern\n"
           "//\n\n"
           "module "
           (file-name-nondirectory (file-name-sans-extension buffer-file-name))
           ";\n"
           _ "\n\n")))

(provide 'law-lib)
