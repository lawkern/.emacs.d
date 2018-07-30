(unless (>= emacs-major-version 24)
  (error "Requires Emacs >=24"))

;; (defgroup glacier nil nil :group 'faces)

(deftheme glacier "Theme based on the Glacier GameBoy Advance")

(defmacro glacier-with-colors (variant &rest body)
  (declare (indent defun))
  `(let* ((class '((class color) (min-colors 89)))
          (light-class (append '((background light)) class))
          (dark-class (append '((background dark)) class))
          (variant ,variant)

          (glacier-ice++ "#a3bfdc") ;;dbe5e9
          (glacier-ice+ "#7ea4ce")
          (glacier-ice "#5980a6") ;; 7c94b0
          ;; (glacier-ice- "#546c92")
          (glacier-ice- "#3e5974") ;; 495e80
          (glacier-ice-- "#1a2f47")

          (glacier-shadow+ "#233343")
          (glacier-shadow "#192430")
          (glacier-shadow- "#101822")

          (glacier-blue+ "#6ee2ff")
          (glacier-blue "#00ccff")
          (glacier-blue- "#0086b3")

          (glacier-yellow "#c9ff94") ;; ffe792
          (glacier-green "#27f1bf") ;;27f1a4
          (glacier-red+ "#f99ff9") ;; edabcc
          (glacier-red "#f448f4")

          (glacier-fg (if (eq variant 'light) glacier-ice glacier-ice))
          (glacier-bg (if (eq variant 'light) glacier-ice+ glacier-shadow))
          (glacier-hl (if (eq variant 'light) glacier-ice++ glacier-shadow-))
          (glacier-dim (if (eq variant 'light) glacier-ice++ glacier-shadow-))

          (glacier-cursor glacier-blue+)

          (glacier-normal glacier-ice+)
          (glacier-insert glacier-blue+)
          (glacier-visual glacier-green)

          (glacier-mode-line-fg (if (eq variant 'light) glacier-ice+ glacier-ice))
          (glacier-mode-line-bg (if (eq variant 'light) glacier-ice glacier-shadow+))

          (glacier-built-in (if (eq variant 'light) glacier-ice glacier-blue+)))

     ,@body))

(defun create-glacier-theme (variant)
  (glacier-with-colors variant
    (custom-theme-set-faces
     'glacier

     ;; Builtin
     `(default ((t (:foreground ,glacier-fg :background ,glacier-bg))))
     `(link ((t (:foreground ,glacier-ice+ :underline t))))
     `(link-visited ((t (:foreground ,glacier-ice- :underline t))))
     `(info-menu-star ((t (:foreground ,glacier-red))))

     `(cursor ((t (:background ,glacier-cursor))))
     `(hl-line ((t (:background ,glacier-hl))))
     `(linum ((t (:foreground ,glacier-ice-))))
     `(fringe ((t (:background ,glacier-bg))))
     `(italic ((t (:underline nil :slant italic))))

     `(whitespace-space ((t (:foreground ,glacier-dim))))
     `(whitespace-tab ((t  (:foreground ,glacier-dim))))
     `(whitespace-newline ((t (:foreground ,glacier-dim))))

     `(show-paren-match ((t (:foreground ,glacier-green :weight bold))))
     `(success ((t (:foreground ,glacier-blue+ :weight bold))))
     `(warning ((t (:foreground ,glacier-red+))))
     `(error ((t (:foreground ,glacier-red))))
     `(minibuffer-prompt ((default (:foreground ,glacier-ice+))))
     `(region ((t (:foreground ,glacier-shadow :background ,glacier-ice))))
     `(highlight ((t (:foreground ,glacier-green :background ,glacier-shadow+))))
     `(tooltip ((t (:foreground ,glacier-ice :background ,glacier-shadow
                                :color ,glacier-ice :height 0.9))))
     `(isearch ((t (:foreground ,glacier-shadow+ :background ,glacier-green))))
     `(isearch-fail ((t (:foreground ,glacier-red :background nil))))
     `(lazy-highlight ((t (:foreground ,glacier-ice+ :background ,glacier-ice-))))


     ;; Font-lock
     `(font-lock-builtin-face ((t (:foreground ,glacier-blue+))))
     `(font-lock-comment-face ((t (:foreground ,glacier-ice-))))
     `(font-lock-function-name-face ((t (:foreground ,glacier-ice))))
     `(font-lock-keyword-face ((t (:foreground ,glacier-blue+))))
     `(font-lock-string-face ((t (:foreground ,glacier-ice+))))
     `(font-lock-variable-name-face ((t (:foreground ,glacier-ice))))
     `(font-lock-constant-face ((t (:foreground ,glacier-ice+))))
     `(font-lock-type-face ((t (:foreground ,glacier-blue+))))
     `(font-lock-preprocessor-face ((t (:foreground ,glacier-ice-))))
     `(font-lock-negation-char-face ((t (:foreground ,glacier-blue+))))
     `(font-lock-warning-face ((t (:foreground ,glacier-red))))

     ;; Custom Font-lock
     `(font-lock-operator-face ((t (:foreground ,glacier-ice+))))
     `(font-lock-todo ((t (:foreground ,glacier-red :weight ultra-bold :underline t))))
     `(font-lock-important ((t (:foreground ,glacier-yellow :weight ultra-bold :underline t))))
     `(font-lock-note ((t (:foreground ,glacier-green :weight ultra-bold :underline t))))

     ;; Modeline
     `(mode-line ((t (:box (:line-width 2 :color ,glacier-mode-line-bg :style nil)
                           :height 1.0 :foreground ,glacier-mode-line-fg
                           :background ,glacier-mode-line-bg))))

     `(mode-line-inactive ((t (:box (:line-width 2 :color ,glacier-bg :style nil)
                                    :height 1.0 :foreground ,glacier-fg
                                    :background ,glacier-bg))))

     `(mode-line-path ((t (:foreground ,glacier-ice-))))
     `(mode-line-buffer-name ((t (:foreground ,glacier-ice+))))
     `(mode-line-read-only ((t (:foreground ,glacier-ice-))))
     `(mode-line-highlight ((t (:foreground ,glacier-blue+))))
     `(mode-line-warning ((t (:foreground ,glacier-red))))
     `(mode-line-insert ((t (:foreground ,glacier-insert))))
     `(mode-line-normal ((t (:foreground ,glacier-normal))))
     `(mode-line-visual ((t (:foreground ,glacier-visual))))

     `(comint-highlight-input ((default (:foreground ,glacier-ice-))))
     `(comint-highlight-prompt ((default (:foreground ,glacier-ice+))))

     `(ido-subdir ((t (:foreground ,glacier-ice))))
     `(ido-first-match ((t (:foreground ,glacier-green))))
     `(ido-only-match ((t (:foreground ,glacier-green))))
     `(ido-indicator ((t (:foreground ,glacier-red))))
     `(ido-incomplete-regexp ((t (:foreground ,glacier-red))))

     `(ivy-current-match ((t (:foreground ,glacier-blue+ :background nil))))
     `(ivy-highlight ((t (:foreground ,glacier-blue+ :background nil))))
     `(ivy-action ((t (:foreground ,glacier-ice :background nil))))
     `(ivy-virtual ((t (:foreground ,glacier-ice :background nil))))
     `(ivy-minibuffer-match-face-1 ((t (:foreground ,glacier-ice+ :background nil))))
     `(ivy-minibuffer-match-face-2 ((t (:foreground ,glacier-ice+ :background nil))))
     `(ivy-minibuffer-match-face-3 ((t (:foreground ,glacier-ice+ :background nil))))
     `(ivy-minibuffer-match-face-4 ((t (:foreground ,glacier-ice+ :background nil))))
     `(ivy-subdir ((t (:foreground ,glacier-ice-))))

     `(font-latex-warning-face ((t (:foreground ,glacier-ice+))))
     `(font-latex-bold-face ((t (:foreground ,glacier-ice-))))
     `(font-latex-sectioning-5-face ((t (:foreground ,glacier-blue+ :height 1.0))))
     `(font-latex-sectioning-4-face ((t (:height 1.0))))
     `(font-latex-sectioning-3-face ((t (:height 1.0))))
     `(font-latex-sectioning-2-face ((t (:height 1.0))))
     `(font-latex-sectioning-1-face ((t (:height 1.0))))

     `(js2-external-variable ((t (:foreground ,glacier-ice))))
     `(js2-function-param ((t (:foreground ,glacier-blue+))))
     `(js2-function-call ((t (:foreground ,glacier-ice+))))
     `(js2-warning ((t (:underline ,glacier-red+))))
     `(js2-error ((t (:underline ,glacier-red))))

     `(web-mode-html-tag-face ((t (:foreground ,glacier-ice+))))
     `(web-mode-html-tag-bracket-face ((t (:foreground ,glacier-ice))))
     `(web-mode-html-attr-name-face ((t (:foreground ,glacier-ice))))
     `(web-mode-html-attr-equal-face ((t (:foreground ,glacier-ice))))
     `(web-mode-html-attr-value-face ((t (:foreground ,glacier-ice+))))
     `(web-mode-doctype-face ((t (:foreground ,glacier-ice))))
     `(web-mode-css-selector-face ((t (:foreground ,glacier-ice+))))

     `(outline-1 ((t (:foreground ,glacier-green))))
     `(outline-2 ((t (:foreground ,glacier-ice+))))
     `(outline-3 ((t (:foreground ,glacier-ice))))
     `(outline-4 ((t (:foreground ,glacier-ice-))))
     `(outline-5 ((t (:foreground ,glacier-ice))))
     `(outline-6 ((t (:foreground ,glacier-ice+))))

     `(visible-mark-active ((t (:foreground ,glacier-ice+))))

     `(org-document-info-keyword ((t (:foreground ,glacier-ice-))))
     `(org-document-title ((t (:foreground ,glacier-ice))))


     `(speed-type-correct ((t (:foreground ,glacier-ice-))))
     `(speed-type-mistake ((t (:foreground ,glacier-red :underline t))))

     `(slime-repl-inputed-output-face ((t (:foreground ,glacier-blue+))))
     `(powerline-active1 ((t (:background ,glacier-ice- :foreground ,glacier-shadow-))))))


  (glacier-with-colors variant
    (custom-theme-set-variables
     'glacier

     `(evil-normal-state-cursor   '(box ,glacier-cursor))
     `(evil-insert-state-cursor   '((bar . 3) ,glacier-insert))
     `(evil-visual-state-cursor   '(box ,glacier-visual))
     `(evil-motion-state-cursor   '(box ,glacier-red))
     `(evil-replace-state-cursor  '(box ,glacier-red))
     `(evil-operator-state-cursor '(box ,glacier-red))))

  ;;   0        default            default
  ;;   1        bold               bold
  ;;   2        faint              default
  ;;   3        italic             italic
  ;;   4        underlined         underline
  ;;   5        slowly blinking    success
  ;;   6        rapidly blinking   warning
  ;;   7        negative image     error
  )

(create-glacier-theme 'dark)

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
(file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'glacier)
