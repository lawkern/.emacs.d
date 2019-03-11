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

          (ice+ "#88b0dd")
          ;; (ice+ "#7ea4ce")
          (ice "#5980a6")
          (ice- "#3e5974")

          (shadow+ "#233343")
          (shadow "#192430")
          (shadow- "#101822")

          (blue+ "#b3edff")
          (blue "#6ee2ff")
          (blue- "#24748f")
          ;; (blue- "#0086b3")

          (yellow "#c9ff94")
          (green "#27f1bf")
          (red+ "#fab7fa")
          (red "#f76ed5")

          (glacier-cursor blue)

          (glacier-normal ice+)
          (glacier-insert blue)
          (glacier-visual green))
     ,@body))

(defun create-glacier-theme (variant)
  (glacier-with-colors variant
    (custom-theme-set-faces
     'glacier

     ;; Builtin
     `(default ((t (:foreground ,ice :background ,shadow))))

     `(link           ((t (:foreground ,ice+ :underline t))))
     `(link-visited   ((t (:foreground ,ice- :underline t))))
     `(info-menu-star ((t (:foreground ,red))))

     `(cursor  ((t (:background ,glacier-cursor))))
     `(hl-line ((t (:background ,shadow-))))
     `(linum   ((t (:foreground ,ice-))))
     `(fringe  ((t (:background ,shadow))))
     `(shadow  ((t (:foreground ,ice+))))
     `(italic  ((t (:underline nil :slant italic))))

     `(whitespace-space   ((t (:foreground ,shadow-))))
     `(whitespace-tab     ((t (:inherit whitespace-space))))
     `(whitespace-newline ((t (:inherit whitespace-space))))

     `(success         ((t (:foreground ,blue :weight bold))))
     `(warning         ((t (:inherit font-lock-warning-face))))
     `(error           ((t (:foreground ,red))))
     `(region          ((t (:foreground nil :background ,shadow+ :distant-foreground nil))))
     `(highlight       ((t (:foreground ,green :background ,shadow+))))
     `(tooltip         ((t (:foreground ,ice :background ,shadow))))
     `(border          ((t (:foreground ,red :background ,red :color ,red))))
     `(vertical-border ((t (:foreground ,shadow+))))

     `(isearch           ((t (:foreground ,shadow+ :background ,green :weight bold))))
     `(isearch-fail      ((t (:foreground ,red))))
     `(lazy-highlight    ((t (:foreground ,green :weight bold))))
     `(show-paren-match  ((t (:foreground ,blue :weight bold))))
     `(minibuffer-prompt ((default (:foreground ,ice+))))

     `(visible-mark-active ((t (:foreground ,ice :background ,ice-))))

     ;; Font-lock
     `(font-lock-builtin-face       ((t (:inherit default))))
     `(font-lock-function-name-face ((t (:foreground ,blue :weight normal))))
     `(font-lock-keyword-face       ((t (:inherit default))))
     `(font-lock-string-face        ((t (:inherit default :foreground ,ice+))))
     `(font-lock-variable-name-face ((t (:inherit default))))
     `(font-lock-constant-face      ((t (:inherit default))))
     `(font-lock-type-face          ((t (:inherit default))))
     `(font-lock-preprocessor-face  ((t (:inherit default))))
     `(font-lock-negation-char-face ((t (:foreground ,blue))))
     `(font-lock-warning-face       ((t (:inherit default))))
     `(font-lock-comment-face       ((t (:foreground ,ice-))))
     `(font-lock-doc-face           ((t (:inherit font-lock-comment-face))))

     `(highlight-numbers-number ((t (:foreground ,blue))))

     ;; Custom Font-lock
     `(font-lock-operator-face ((t (:foreground ,blue))))
     `(font-lock-todo          ((t (:inherit default :weight bold :underline t))))
     `(font-lock-important     ((t (:foreground ,yellow :weight bold :underline t))))
     `(font-lock-note          ((t (:inherit font-lock-comment-face :weight bold :underline t))))

     ;; Modeline
     `(mode-line ((t (:box (:line-width 2 :color ,shadow :style nil)
                           :height 1.0 :foreground ,ice+
                           :background ,shadow))))

     `(mode-line-inactive ((t (:box (:line-width 2 :color ,shadow :style nil)
                                    :height 1.0 :foreground ,ice-
                                    :background ,shadow))))

     `(mode-line-path   ((t (:foreground ,ice-))))
     `(mode-line-buffer ((t (:foreground ,ice+ :weight bold))))

     `(mode-line-read-only ((t (:foreground ,ice-))))
     `(mode-line-highlight ((t (:foreground ,blue))))
     `(mode-line-warning   ((t (:foreground ,red+))))
     `(mode-line-insert    ((t (:foreground ,glacier-insert))))
     `(mode-line-normal    ((t (:foreground ,glacier-normal))))
     `(mode-line-visual    ((t (:foreground ,glacier-visual))))
     `(mode-line-operator  ((t (:foreground ,red))))

     `(apropos-symbol             ((t (:foreground ,ice+ :weight bold))))
     `(apropos-function-button    ((t (:foreground ,blue :underline t))))
     `(apropos-user-option-button ((t (:foreground ,blue :underline t))))
     `(apropos-variable-button    ((t (:foreground ,blue :underline t))))
     `(apropos-plist-button       ((t (:foreground ,blue :underline t))))

     `(compilation-info           ((t (:foreground ,ice))))
     `(compilation-line-number    ((t (:foreground ,ice))))
     `(compilation-column-number  ((t (:foreground ,ice))))
     `(compilation-mode-line-exit ((t (:weight bold))))
     `(compilation-mode-line-fail ((t (:weight bold :inherit compilation-error))))

     `(comint-highlight-input  ((default (:foreground ,ice-))))
     `(comint-highlight-prompt ((default (:foreground ,ice+))))

     `(ido-subdir            ((t (:foreground ,ice))))
     `(ido-first-match       ((t (:foreground ,green))))
     `(ido-only-match        ((t (:foreground ,green))))
     `(ido-indicator         ((t (:foreground ,red))))
     `(ido-incomplete-regexp ((t (:foreground ,red))))

     `(ivy-current-match       ((t (:foreground ,ice+ :weight bold))))
     `(ivy-confirm-face        ((t (:foreground ,blue))))
     `(ivy-highlight           ((t (:foreground ,blue))))
     `(ivy-action              ((t (:foreground ,ice))))
     `(ivy-virtual             ((t (:foreground ,ice))))
     `(ivy-subdir              ((t (:foreground ,ice-))))
     `(ivy-match-required-face ((t (:foreground ,red))))

     `(ivy-minibuffer-match-face-1 ((t (:foreground ,ice+))))
     `(ivy-minibuffer-match-face-2 ((t (:foreground ,ice+))))
     `(ivy-minibuffer-match-face-3 ((t (:foreground ,ice+))))
     `(ivy-minibuffer-match-face-4 ((t (:foreground ,ice+))))

     `(font-latex-warning-face      ((t (:foreground ,ice+))))
     `(font-latex-bold-face         ((t (:foreground ,ice-))))
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

     `(org-todo                  ((t (:foreground ,red))))
     `(org-done                  ((t (:foreground ,green))))
     `(org-document-info-keyword ((t (:foreground ,ice-))))
     `(org-document-title        ((t (:foreground ,blue))))
     `(org-document-info         ((t (:foreground ,ice+))))
     `(org-latex-and-related     ((t (:foreground ,ice+))))

     `(speed-type-correct ((t (:foreground ,ice-))))
     `(speed-type-mistake ((t (:foreground ,red :underline t))))


     `(lui-time-stamp-face ((t (:foreground ,ice+))))
     `(lui-highlight-face  ((t (:inherit highlight))))
     `(lui-button-face     ((t (:inherit link))))

     `(lui-strong-face   ((t (:inherit default :weight bold))))
     `(lui-emphasis-face ((t (:inherit default :slant italic))))
     `(lui-deleted-face  ((t (:inherit font-lock-comment-face))))

     `(slime-repl-inputed-output-face ((t (:foreground ,blue))))

     `(powerline-active1 ((t (:background ,ice- :foreground ,shadow-))))))


  (glacier-with-colors variant
    (custom-theme-set-variables
     'glacier

     `(ansi-color-names-vector '[,shadow ,red ,green ,yellow ,blue+ ,red+ ,blue ,ice+])
     `(evil-normal-state-cursor   '(box ,glacier-cursor))
     `(evil-insert-state-cursor   '((bar . 3) ,glacier-insert))
     `(evil-visual-state-cursor   '(box ,glacier-visual))
     `(evil-motion-state-cursor   '(box ,glacier-cursor))
     `(evil-replace-state-cursor  '(box ,red))
     `(evil-operator-state-cursor '(box ,red)))))

(create-glacier-theme 'dark)

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
(file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'glacier)
