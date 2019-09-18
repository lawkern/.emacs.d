(unless (>= emacs-major-version 24)
  (error "Requires Emacs >=24"))

(deftheme glacier "Dark theme based on the Glacier GameBoy Advance")

(defmacro glacier-with-colors (variant &rest body)
  (declare (indent defun))
  `(let* ((class '((class color) (min-colors 89)))
          (light-class (append '((background light)) class))
          (dark-class (append '((background dark)) class))
          (variant ,variant)

          ;; Soft
          ;; (ice "#88b0dd")
          ;; (shadow "#3d5876")
          ;; (shadow- "#2b3f54")
          ;; (blue- "#5cbad6")

          ;; Hard
          (ice "#5980a6")
          (shadow "#192430")
          (shadow- "#101822")
          (blue- "#478fa4")

          (ice++ "#d7e5f4")
          (ice+ "#88b0dd")
          (ice- "#4d6d8f")
          (ice-- "#3e5974")
          (shadow+ "#233343")

          (blue+ "#b3edff")
          (blue "#6ee2ff")

          (yellow "#c9ff94")
          ;; (yellow- "#739464")
          (yellow- "#69875d")

          (green+ "#59a6a6")
          (green "#27f1bf")
          (green- "#218a77")

          (red+ "#fab7fa")
          (red "#f76ed5")
          (red- "#610561")

          (cursor blue)

          (normal ice++)
          (insert blue)
          (visual green))
     ,@body))

(defun create-glacier-theme (variant)
  (glacier-with-colors variant
    (custom-theme-set-faces
     'glacier

     ;; Builtin
     `(default ((t (:foreground ,ice :background ,shadow))))

     `(link           ((t (:foreground ,ice+ :underline t))))
     `(link-visited   ((t (:foreground ,ice-- :underline t))))
     `(info-menu-star ((t (:foreground ,red))))

     `(cursor  ((t (:background ,cursor))))
     `(hl-line ((t (:background ,shadow-))))
     `(linum   ((t (:foreground ,ice--))))
     `(fringe  ((t (:background ,shadow))))
     `(shadow  ((t (:foreground ,ice--))))
     `(italic  ((t (:underline nil :slant italic))))

     `(dired-header  ((t (:foreground ,ice+ :weight bold))))
     `(dired-directory  ((t (:foreground ,ice+))))

     `(line-number ((t (:foreground ,shadow+))))
     `(line-number-current-line ((t (:foreground ,ice--))))

     `(widget-field ((t (:background ,shadow+))))

     `(custom-state ((t (:foreground ,green))))
     `(custom-group-tag ((t (:inherit variable-pitch :foreground ,blue :height 1.2 :weight bold))))
     `(custom-button ((t (:box (:line-width 3 :style released-button)
                               :foreground ,ice+ :background ,shadow+))))

     `(breakpoint-disabled ((t (:foreground ,yellow))))
     `(breakpoint-enabled ((t (:foreground ,red :weight bold))))

     `(whitespace-space   ((t (:foreground ,shadow-))))
     `(whitespace-tab     ((t (:inherit whitespace-space))))
     `(whitespace-newline ((t (:inherit whitespace-space))))

     `(success         ((t (:foreground ,blue :weight bold))))
     `(warning         ((t (:inherit font-lock-warning-face))))
     `(error           ((t (:foreground ,red))))
     `(region          ((t (:foreground nil :background ,shadow- :distant-foreground nil))))
     `(highlight       ((t (:foreground ,green :background ,shadow+))))
     `(tooltip         ((t (:foreground ,ice :background ,shadow))))
     `(border          ((t (:foreground ,red :background ,red :color ,red))))
     `(vertical-border ((t (:foreground ,shadow+))))

     `(match             ((t (:foreground ,green :weight bold))))
     `(isearch           ((t (:foreground ,shadow+ :background ,green :weight bold))))
     `(isearch-fail      ((t (:foreground ,red))))
     `(lazy-highlight    ((t (:foreground ,green :weight bold))))
     `(show-paren-match  ((t (:foreground ,blue :weight bold))))
     `(minibuffer-prompt ((default (:foreground ,ice+))))

     `(visible-mark-active ((t (:foreground ,ice :background ,ice--))))

     ;; Font-lock
     `(font-lock-builtin-face           ((t (:inherit default :foreground))))
     `(font-lock-function-name-face     ((t (:foreground ,blue :weight normal))))
     `(font-lock-keyword-face           ((t (:inherit default))))
     `(font-lock-string-face            ((t (:foreground ,blue-))))
     `(font-lock-variable-name-face     ((t (:inherit default))))
     `(font-lock-constant-face          ((t (:inherit default))))
     `(font-lock-type-face              ((t (:inherit default))))
     `(font-lock-preprocessor-face      ((t (:inherit default))))
     `(font-lock-negation-char-face     ((t (:foreground ,red))))
     `(font-lock-warning-face           ((t (:inherit default))))
     `(font-lock-comment-face           ((t (:inherit font-lock-string-face))))
     `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
     `(font-lock-doc-face               ((t (:inherit font-lock-comment-face))))

     `(highlight-numbers-number ((t (:foreground ,blue))))

     ;; Custom Font-lock
     `(font-lock-operator-face ((t (:foreground ,blue))))
     `(font-lock-todo          ((t (:inherit error :weight bold :underline t))))
     `(font-lock-important     ((t (:foreground ,yellow :weight bold :underline t))))
     `(font-lock-note          ((t (:inherit font-lock-comment-face :weight bold :underline t))))

     ;; Modeline
     `(mode-line ((t (:box (:line-width 2 :color ,shadow :style nil)
                           :height 1.0 :foreground ,ice+
                           :background ,shadow))))

     `(mode-line-inactive ((t (:box (:line-width 2 :color ,shadow :style nil)
                                    :height 1.0 :foreground ,ice--
                                    :background ,shadow))))

     `(mode-line-path   ((t (:foreground ,ice--))))
     `(mode-line-buffer ((t (:foreground ,ice+ :weight bold))))

     `(mode-line-read-only ((t (:foreground ,ice--))))
     `(mode-line-highlight ((t (:foreground ,blue))))
     `(mode-line-warning   ((t (:foreground ,red+))))
     `(mode-line-insert    ((t (:foreground ,insert))))
     `(mode-line-normal    ((t (:foreground ,normal))))
     `(mode-line-visual    ((t (:foreground ,visual))))
     `(mode-line-operator  ((t (:foreground ,red))))

     `(header-line  ((t (:inherit mode-line :weight bold))))

     `(apropos-symbol             ((t (:foreground ,ice+ :weight bold))))
     `(apropos-function-button    ((t (:foreground ,blue :underline t))))
     `(apropos-user-option-button ((t (:foreground ,blue :underline t))))
     `(apropos-variable-button    ((t (:foreground ,blue :underline t))))
     `(apropos-plist-button       ((t (:foreground ,blue :underline t))))

     `(compilation-error          ((t (:foreground ,ice))))
     `(compilation-info           ((t (:foreground ,blue))))
     `(compilation-line-number    ((t (:foreground ,red))))
     `(compilation-column-number  ((t (:foreground ,red))))
     `(compilation-mode-line-exit ((t (:weight bold))))
     `(compilation-mode-line-fail ((t (:weight bold :inherit compilation-error))))

     `(comint-highlight-input  ((default (:foreground ,ice--))))
     `(comint-highlight-prompt ((default (:foreground ,ice+))))

     `(ido-subdir            ((t (:foreground ,ice))))
     `(ido-first-match       ((t (:foreground ,green))))
     `(ido-only-match        ((t (:foreground ,green))))
     `(ido-indicator         ((t (:foreground ,red))))
     `(ido-incomplete-regexp ((t (:foreground ,red))))

     `(ivy-current-match       ((t (:foreground ,blue :weight bold))))
     `(ivy-confirm-face        ((t (:foreground ,blue))))
     `(ivy-highlight           ((t (:foreground ,blue))))
     `(ivy-action              ((t (:foreground ,ice))))
     `(ivy-virtual             ((t (:foreground ,ice))))
     `(ivy-subdir              ((t (:foreground ,blue-))))
     `(ivy-match-required-face ((t (:foreground ,red))))

     `(ivy-minibuffer-match-face-1 ((t (:foreground ,ice+))))
     `(ivy-minibuffer-match-face-2 ((t (:foreground ,ice+))))
     `(ivy-minibuffer-match-face-3 ((t (:foreground ,ice+))))
     `(ivy-minibuffer-match-face-4 ((t (:foreground ,ice+))))

     `(magit-section-heading   ((t (:foreground ,blue :weight bold))))
     `(magit-section-highlight ((t (:background nil))))
     `(magit-hash              ((t (:foreground ,ice--))))
     `(magit-branch-remote     ((t (:foreground ,blue))))
     `(magit-branch-local      ((t (:foreground ,ice+))))

     `(magit-diff-added   ((t (:foreground ,green :background nil))))
     `(magit-diff-removed ((t (:foreground ,red :background nil))))
     `(magit-diff-context ((t (:foreground ,ice))))
     `(magit-diff-hunk-heading ((t (:foreground ,ice+ :background nil :weight bold))))

     `(magit-diff-added-highlight   ((t (:inherit magit-diff-added :background ,green-))))
     `(magit-diff-removed-highlight ((t (:foreground ,red+ :background ,red-))))
     `(magit-diff-context-highlight ((t (:foreground ,ice+ :background ,shadow+))))
     `(magit-diff-hunk-heading-highlight ((t (:inherit magit-diff-hunk-heading :background ,shadow+))))



     ;; magit-diff-hunk-heading-highlight
     ;; `(magit-diff-context-highlight ((t (:foreground ,ice-- :background ,shadow+))))

     `(font-latex-warning-face      ((t (:foreground ,ice+))))
     `(font-latex-bold-face         ((t (:foreground ,ice--))))
     `(font-latex-sectioning-5-face ((t (:foreground ,blue :height 1.0))))
     `(font-latex-sectioning-4-face ((t (:height 1.0))))
     `(font-latex-sectioning-3-face ((t (:height 1.0))))
     `(font-latex-sectioning-2-face ((t (:height 1.0))))
     `(font-latex-sectioning-1-face ((t (:height 1.0))))

     `(js2-external-variable ((t (:foreground ,ice))))
     `(js2-function-param    ((t (:foreground ,blue))))
     `(js2-function-call     ((t (:foreground ,ice+))))
     `(js2-warning           ((t (:underline ,red+))))
     `(js2-error             ((t (:underline ,red))))

     `(tuareg-font-lock-interactive-output-face ((t (:foreground ,blue))))
     `(tuareg-font-lock-governing-face          ((t (:foreground ,blue :weight bold))))
     `(tuareg-font-lock-operator-face           ((t (:foreground ,ice+))))
     `(tuareg-font-double-colon-face            ((t (:foreground ,ice))))
     `(tuareg-font-lock-error-face              ((t (:foreground ,yellow :background ,red))))
     `(tuareg-font-lock-module-face             ((t (:foreground ,ice))))

     `(elixir-attribute-face ((t (:inherit default))))
     `(elixir-atom-face      ((t (:foreground ,ice+))))

     `(css-selector ((t (:foreground ,blue-))))

     `(web-mode-html-tag-face         ((t (:foreground ,ice+))))
     `(web-mode-html-tag-bracket-face ((t (:foreground ,ice))))
     `(web-mode-html-attr-name-face   ((t (:foreground ,ice))))
     `(web-mode-html-attr-equal-face  ((t (:foreground ,ice))))
     `(web-mode-html-attr-value-face  ((t (:foreground ,ice+))))
     `(web-mode-doctype-face          ((t (:foreground ,ice))))
     `(web-mode-css-selector-face     ((t (:foreground ,ice+))))

     `(outline-1 ((t (:foreground ,ice))))
     `(outline-2 ((t (:foreground ,ice+))))
     `(outline-3 ((t (:foreground ,ice))))
     `(outline-4 ((t (:foreground ,ice+))))
     `(outline-5 ((t (:foreground ,ice))))
     `(outline-6 ((t (:foreground ,ice+))))

     `(rainbow-delimiters-base-face ((t (:foreground ,blue))))
     `(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face))))
     `(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face))))
     `(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face))))
     `(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face))))
     `(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face))))
     `(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face))))
     `(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face))))
     `(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face))))
     `(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face))))
     `(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face))))


     `(org-todo                  ((t (:foreground ,red))))
     `(org-done                  ((t (:foreground ,green))))
     `(org-document-info-keyword ((t (:foreground ,ice--))))
     `(org-document-title        ((t (:foreground ,blue))))
     `(org-document-info         ((t (:foreground ,ice+))))
     `(org-latex-and-related     ((t (:foreground ,ice+))))

     `(speed-type-correct ((t (:foreground ,ice--))))
     `(speed-type-mistake ((t (:foreground ,red :underline t))))


     `(lui-time-stamp-face ((t (:foreground ,ice+))))
     `(lui-highlight-face  ((t (:inherit highlight))))
     `(lui-button-face     ((t (:inherit link))))

     `(lui-strong-face   ((t (:inherit default :weight bold))))
     `(lui-emphasis-face ((t (:inherit default :slant italic))))
     `(lui-deleted-face  ((t (:inherit font-lock-comment-face))))

     `(slime-repl-inputed-output-face ((t (:foreground ,blue))))

     `(powerline-active1 ((t (:background ,ice-- :foreground ,shadow-))))))


  (glacier-with-colors variant
    (custom-theme-set-variables
     'glacier

     `(ansi-color-names-vector '[,shadow ,red ,green ,yellow ,blue+ ,red+ ,blue ,ice+])
     `(evil-normal-state-cursor   '(box ,cursor))
     `(evil-insert-state-cursor   '((bar . 5) ,insert))
     `(evil-visual-state-cursor   '(box ,visual))
     `(evil-motion-state-cursor   '(box ,cursor))
     `(evil-replace-state-cursor  '(box ,red))
     `(evil-operator-state-cursor '(box ,red)))))

(create-glacier-theme 'dark)

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
(file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'glacier)
