(deftheme drybones "A dry, sandy theme based on gruvbox.")

(defcustom drybones-org-variable-font nil
  "Determines whether org-mode should use variable pitch fonts."
  :type 'boolean)

(let ((fg++ "#faf1d7")
      ;; (fg+  "#d1c3ab")
      ;; (fg   "#c2a982")
      ;; (fg-  "#ad9266")
      ;; (fg-- "#967b4e")

      (fg+  "#ebdbb2")
      (fg   "#decb9b")
      (fg-  "#b5a48d")
      (fg-- "#928374")


      (bg++ "#524e4c")
      (bg+  "#454240")
      (bg   "#2b2927")
      ;; (bg+  "#3E3F41")
      ;; (bg   "#32302f")
      (bg-  "#282828")
      (bg-- "#21201e")

      (y++ "#ffe7b0")
      (y+  "#ffd77a")
      (y   "#fabd2f")
      ;; (y-  "#d79921")
      (y-  "#e68c51")
      (y-- "#b07809")

      (b+ "#a1c2b5")
      (b  "#83a598")
      (b- "#5c8274")

      (c+ "#a6f5c3")
      (c  "#84e0a6")
      (c- "#68b09e")

      (g++ "#c3f0b4")
      (g+  "#98e084")
      (g   "#6bcf3e")
      (g-  "#649e21")
      (g-- "#377d24")

      ;; (g++ "#f0f725")
      ;; (g+  "#dde32b")
      ;; (g   "#b8bb26")
      ;; (g-  "#98971a")
      ;; (g-- "#78771c")

      (m+  "#f0a5c6")
      (m   "#d3869b")
      (m-  "#b16286")
      (m-- "#a34d74")

      (r++ "#fc8a7b")
      (r+  "#e36959")
      (r   "#fb4934")
      (r-  "#cc241d")
      (r-- "#a63530"))

  (custom-theme-set-faces
   'drybones

   ;; Basic
   `(default                  ((t (:foreground ,fg+ :background ,bg))))
   `(cursor                   ((t (:background ,c-))))
   `(hl-line                  ((t (:background ,bg++))))
   `(linum                    ((t (:foreground ,fg--))))
   `(line-number              ((t (:foreground ,bg+))))
   `(line-number-current-line ((t (:foreground ,fg--))))
   `(fringe                   ((t (:inherit default :foreground ,bg++))))
   `(shadow                   ((t (:foreground ,fg--))))
   `(italic                   ((t (:underline nil :slant italic))))
   `(link                     ((t (:foreground ,y :underline t))))
   `(link-visited             ((t (:foreground ,b- :underline t))))
   `(whitespace-space         ((t (:foreground ,bg-))))
   `(whitespace-tab           ((t (:inherit whitespace-space))))
   `(whitespace-newline       ((t (:inherit whitespace-space))))
   `(success                  ((t (:foreground ,g+ :weight bold))))
   `(warning                  ((t (:inherit font-lock-warning-face))))
   `(error                    ((t (:foreground ,r+))))
   `(region                   ((t (:foreground nil :background ,bg++ :distant-foreground nil))))
   ;; `(region                   ((t (:background nil))))
   `(highlight                ((t (:foreground ,g :background ,bg+))))
   `(tooltip                  ((t (:foreground ,fg :background ,bg))))
   `(border                   ((t (:foreground ,r :background ,r :color ,r))))
   `(vertical-border          ((t (:foreground ,bg++))))
   `(info-menu-star           ((t (:foreground ,r))))
   `(header-line              ((t (:inherit mode-line :weight bold))))
   `(breakpoint-disabled      ((t (:foreground ,y))))
   `(breakpoint-enabled       ((t (:foreground ,r :weight bold))))
   `(widget-field             ((t (:background ,bg+))))
   `(match                    ((t (:foreground ,g+ :weight bold))))
   `(isearch                  ((t (:foreground ,bg+ :background ,g+ :weight bold))))
   `(isearch-fail             ((t (:foreground ,r+))))
   `(lazy-highlight           ((t (:foreground ,g+ :weight bold))))
   `(show-paren-match         ((t (:foreground ,c+ :weight bold))))
   `(show-paren-mismatch      ((t (:foreground ,r+ :weight bold))))
   `(minibuffer-prompt        ((t (:foreground ,fg))))
   `(custom-state             ((t (:foreground ,g))))
   `(custom-group-tag         ((t (:inherit variable-pitch :foreground ,c :height 1.2 :weight bold))))
   `(custom-button            ((t (:foreground ,fg :background ,bg+ :box (:line-width 3 :style released-button)))))
   `(help-key-binding         ((t (:foreground ,b :box (:line-width (1 . -1) :color ,b-)))))

   ;; Evil
   `(evil-ex-substitute-replacement ((t (:foreground ,r :underline t :weight bold))))

   ;; Flyspell
   `(flyspell-incorrect ((t (:underline (:color ,r :style wave)))))
   `(flyspell-duplicate ((t (:underline (:color ,y :style wave)))))

   ;; Ediff
   `(ediff-odd-diff-A  ((t (:background ,bg+ :extend t))))
   `(ediff-even-diff-A ((t (:background ,bg- :extend t))))
   `(ediff-odd-diff-B  ((t (:background ,bg- :extend t))))
   `(ediff-even-diff-B ((t (:background ,bg+ :extend t))))

   `(ediff-current-diff-A ((t (:background ,r-))))
   `(ediff-current-diff-B ((t (:background ,g-))))

   ;; Dir
   `(dired-header         ((t (:foreground ,y :weight bold))))
   `(dired-directory      ((t (:foreground ,b+))))
   `(dired-flagged        ((t (:foreground ,r))))
   `(dired-ignored        ((t (:foreground ,fg--))))
   `(dired-mark           ((t (:foreground ,c))))
   `(dired-marked         ((t (:foreground ,y))))
   `(dired-perm-write     ((t (:foreground ,r))))
   `(dired-set-id         ((t (:foreground ,g))))
   `(dired-special        ((t (:foreground ,g))))
   `(dired-symlink        ((t (:foreground ,g))))
   `(dired-warning        ((t (:foreground ,r+))))
   `(dired-broken-symlink ((t (:foreground ,r :weight bold))))

   ;; Info
   `(info-node ((t (:foreground ,y :weight bold))))

   ;; Eshell
   `(eshell-ls-archive ((t (:foreground ,r))))
   ;; eshell-ls-backup
   ;; eshell-ls-clutter
   ;; eshell-ls-directory
   `(eshell-ls-directory ((t (:foreground ,c-))))
   ;; eshell-ls-
   `(eshell-ls-executable ((t (:foreground ,g))))
   `(eshell-ls-missing ((t (:foreground ,r))))
   ;; eshell-ls-product
   ;; eshell-ls-readonly
   ;; eshell-ls-special
   `(eshell-ls-symlink ((t (:foreground ,g))))
   `(eshell-ls-unreadable ((t (:foreground ,fg-))))
   `(eshell-prompt ((t (:foreground ,y :weight bold))))

   ;; Tabs
   `(tac-bar ((t (:foreground ,fg+ :background ,bg-))))
   `(tac-bar-tab ((t (:foreground ,fg+ :background ,bg :height 1.0 :box (:line-width 8 :color ,bg :style nil)))))
   `(tac-bar-tac-inactive ((t (:inherit tac-bar :height 1.0 :box (:line-width 8 :color ,bg- :style nil)))))

   ;; Font-lock
   `(font-lock-builtin-face           ((t (:inherit default))))
   `(font-lock-function-name-face     ((t (:inherit default :foreground ,y-))))
   `(font-lock-keyword-face           ((t (:inherit default :foreground ,y-))))
   `(font-lock-string-face            ((t (:foreground ,g++))))
   `(font-lock-variable-name-face     ((t (:inherit default))))
   `(font-lock-constant-face          ((t (:inherit default))))
   `(font-lock-type-face              ((t (:inherit default))))
   `(font-lock-preprocessor-face      ((t (:foreground ,fg--))))
   `(font-lock-negation-char-face     ((t (:foreground ,r+))))
   `(font-lock-warning-face           ((t (:foreground ,y))))
   `(font-lock-comment-face           ((t (:foreground ,fg-- :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-doc-face               ((t (:inherit font-lock-string-face))))

   `(font-lock-regexp-grouping-construct ((t (:foreground ,y+ :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,fg-  :weight bold))))

   ;; Custom Font-lock
   `(font-lock-function-decl-face    ((t (:foreground ,y :weight normal))))
   `(font-lock-type-decl-face        ((t (:foreground ,y :weight normal))))
   `(font-lock-number-face           ((t (:inherit font-lock-string-face))))
   `(font-lock-number-extension-face ((t (:inherit default))))
   `(font-lock-operator-face         ((t (:foreground ,c))))
   `(font-lock-assignment-face       ((t (:foreground ,y-))))
   `(font-lock-macro-face            ((t (:foreground ,b-))))
   `(font-lock-todo                  ((t (:foreground ,r++ :weight bold :underline t))))
   `(font-lock-important             ((t (:foreground ,y :weight bold :underline t))))
   `(font-lock-note                  ((t (:foreground ,g+ :weight bold :underline t))))
   `(font-lock-comment-attribution   ((t (:foreground ,fg+ :weight bold))))


   ;; Mode Line
   `(mode-line           ((t (:foreground ,fg+  :background ,bg++ :height 1.0 :box (:line-width 1 :color ,bg+ :style released-button))))) ; :style released-button
   `(mode-line-inactive  ((t (:foreground ,fg-  :background ,bg+  :height 1.0 :box (:line-width 1 :color ,bg++ :style nil)))))
   `(mode-line-path      ((t (:foreground ,fg--))))
   `(mode-line-buffer    ((t (:foreground ,fg+))))
   `(mode-line-read-only ((t (:foreground ,fg--))))
   `(mode-line-highlight ((t (:foreground ,c))))
   `(mode-line-warning   ((t (:foreground ,r+))))
   `(mode-line-insert    ((t (:foreground ,c))))
   `(mode-line-normal    ((t (:foreground ,c))))
   `(mode-line-visual    ((t (:foreground ,g))))
   `(mode-line-operator  ((t (:foreground ,r))))

   ;; Apropos
   `(apropos-symbol             ((t (:foreground ,fg+ :weight bold))))
   `(apropos-function-button    ((t (:foreground ,c :underline t))))
   `(apropos-user-option-button ((t (:foreground ,c :underline t))))
   `(apropos-variable-button    ((t (:foreground ,c :underline t))))
   `(apropos-plist-button       ((t (:foreground ,c :underline t))))

   ;; Compilation
   `(compilation-error          ((t (:inherit error))))
   `(compilation-info           ((t (:foreground ,g+))))
   `(compilation-line-number    ((t (:foreground ,fg))))
   `(compilation-column-number  ((t (:foreground ,fg))))
   `(compilation-mode-line-exit ((t (:weight bold))))
   `(compilation-mode-line-fail ((t (:weight bold :inherit compilation-error))))

   ;; Comint
   `(comint-highlight-input  ((default (:foreground ,fg--))))
   `(comint-highlight-prompt ((default (:inherit nil))))

   ;; Icomplete
   `(icomplete-first-match ((t (:foreground ,y))))

   ;; Ido
   `(ido-subdir            ((t (:foreground ,fg))))
   `(ido-first-match       ((t (:foreground ,y))))
   `(ido-only-match        ((t (:inherit ido-first-match :weight normal))))
   `(ido-indicator         ((t (:foreground ,r))))
   `(ido-incomplete-regexp ((t (:foreground ,r))))

   ;; Ivy
   `(ivy-current-match           ((t (:foreground ,y :weight normal))))
   `(ivy-confirm-face            ((t (:foreground ,c))))
   `(ivy-highlight               ((t (:foreground ,c))))
   `(ivy-action                  ((t (:foreground ,fg))))
   `(ivy-virtual                 ((t (:foreground ,fg))))
   `(ivy-subdir                  ((t (:foreground ,b+))))
   `(ivy-org                     ((t (:foreground ,fg))))
   `(ivy-match-requir-face       ((t (:foreground ,r))))
   `(ivy-minibuffer-match-face-1 ((t (:foreground ,g+ :weight normal))))
   `(ivy-minibuffer-match-face-2 ((t (:foreground ,g+ :weight normal))))
   `(ivy-minibuffer-match-face-3 ((t (:foreground ,g+ :weight normal))))
   `(ivy-minibuffer-match-face-4 ((t (:foreground ,g+ :weight normal))))

   ;; CSS Mode
   `(css-selector ((t (:foreground ,b))))

   ;; Treemacs
   `(treemacs-root-face      ((t (:foreground ,c))))
   `(treemacs-directory-face ((t (:foreground ,c-))))

   ;; Magit
   `(magit-section-heading             ((t (:foreground ,c- :weight bold))))
   `(magit-section-highlight           ((t (:background nil))))
   `(magit-hash                        ((t (:foreground ,g))))
   `(magit-branch-remote               ((t (:foreground ,c))))
   `(magit-branch-local                ((t (:foreground ,c))))
   `(magit-diff-file-heading           ((t (:foreground ,fg  :background ,bg  :weight bold))))
   `(magit-diff-hunk-heading           ((t (:foreground ,fg  :background ,bg+ :weight bold))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,fg+ :background ,fg--))))
   ;; `(magit-diff-context                ((t (:foreground ,fg))))
   ;; `(magit-diff-context-highlight      ((t (:foreground ,fg+ :background ,bg+))))
   `(magit-diff-added                  ((t (:foreground ,g :background ,g--))))
   `(magit-diff-removed                ((t (:foreground ,r :background ,r--))))
   `(magit-diffstat-added              ((t (:foreground ,g))))
   `(magit-diffstat-removed            ((t (:foreground ,r))))
   `(magit-diff-added-highlight        ((t (:inherit magit-diff-added :foreground ,g+ :background ,g-))))
   `(magit-diff-removed-highlight      ((t (:inherit magit-diff-added :foreground ,r+ :background ,r-))))

   ;; Shell-script
   `(sh-quoted-exec ((t (:foreground ,r+))))

   ;; Latex Mode
   `(font-latex-warning-face      ((t (:foreground ,fg+))))
   `(font-latex-bold-face         ((t (:foreground ,fg--))))
   `(font-latex-sectioning-5-face ((t (:foreground ,c :height 1.0))))
   `(font-latex-sectioning-4-face ((t (:height 1.0))))
   `(font-latex-sectioning-3-face ((t (:height 1.0))))
   `(font-latex-sectioning-2-face ((t (:height 1.0))))
   `(font-latex-sectioning-1-face ((t (:height 1.0))))

   ;; JS2 Mode
   `(js2-external-variable ((t (:foreground ,fg))))
   `(js2-function-param    ((t (:foreground ,c))))
   `(js2-function-call     ((t (:foreground ,fg))))
   `(js2-warning           ((t (:underline ,r+))))
   `(js2-error             ((t (:underline ,r))))

   ;; Tuareg Mode
   `(tuareg-font-lock-operator-face           ((t (:inherit default))))
   `(tuareg-font-lock-module-face             ((t (:inherit default))))
   `(tuareg-font-double-colon-face            ((t (:foreground ,c-))))
   `(tuareg-font-lock-interactive-output-face ((t (:foreground ,c))))
   `(tuareg-font-lock-governing-face          ((t (:foreground ,c :weight bold))))
   `(tuareg-font-lock-error-face              ((t (:foreground ,y :background ,r))))

   ;; FSharp Mode
   `(fsharp-ui-operator-face ((t (:foreground ,b+))))

   ;; Elixir Mode
   `(elixir-attribute-face ((t (:inherit default))))
   `(elixir-atom-face      ((t (:foreground ,fg))))

   ;; Web mode
   `(wec-mode-html-tag-face         ((t (:inherit default))))
   `(wec-mode-html-tag-bracket-face ((t (:inherit default))))
   `(wec-mode-html-attr-name-face   ((t (:inherit default))))
   `(wec-mode-html-attr-equal-face  ((t (:inherit default))))
   `(wec-mode-html-attr-value-face  ((t (:inherit default))))
   `(wec-mode-doctype-face          ((t (:inherit default))))
   `(wec-mode-css-selector-face     ((t (:inherit default))))

   ;; Outline
   `(outline-1 ((t (:foreground ,y+))))   ; :inherit fixed-pitch :weight bold
   `(outline-2 ((t (:foreground ,b))))  ; :inherit fixed-pitch :weight bold
   `(outline-3 ((t (:foreground ,fg)))) ; :inherit fixed-pitch :weight bold
   `(outline-4 ((t (:foreground ,fg)))) ; :inherit fixed-pitch :weight bold
   `(outline-5 ((t (:foreground ,fg)))) ; :inherit fixed-pitch :weight bold
   `(outline-6 ((t (:foreground ,fg)))) ; :inherit fixed-pitch :weight bold

   ;; rg Mode
   `(org-level-1 ((t (:inherit outline-1)))) ; :height 1.0
   `(org-level-2 ((t (:inherit outline-2)))) ; :height 1.0
   `(org-level-3 ((t (:inherit outline-3))))
   `(org-level-4 ((t (:inherit outline-4))))
   `(org-level-5 ((t (:inherit outline-5))))
   `(org-level-6 ((t (:inherit outline-6))))

   `(org-todo                  ((t (:inherit font-lock-todo))))
   `(org-done                  ((t (:inherit font-lock-note :foreground ,g))))
   `(org-date                  ((t (:inherit font-lock-string-face))))
   `(org-block                 ((t (:inherit default))))
   `(org-table                 ((t (:foreground ,b))))
   `(org-code                  ((t (:inherit default))))
   `(org-document-info-keyword ((t (:foreground ,fg--))))
   ;; `(org-document-title        ((t (:inherit variable-pitch :foreground ,c :weight bold :height 2.0))))
   `(org-document-title        ((t (:inherit fixed-pitch :foreground ,c :weight bold :height 1.0))))
   `(org-document-info         ((t (:foreground ,fg))))
   `(org-latex-and-related     ((t (:foreground ,fg))))

   ;; Lui
   `(lui-time-stamp-face ((t (:foreground ,fg))))
   `(lui-highlight-face  ((t (:inherit highlight))))
   `(lui-button-face     ((t (:inherit link))))
   `(lui-strong-face     ((t (:inherit default :weight bold))))
   `(lui-emphasis-face   ((t (:inherit default :slant italic))))
   `(lui-deleted-face    ((t (:inherit font-lock-comment-face))))

   ;; Rainbow Mode
   `(rainbow-delimiters-base-face    ((t (:foreground ,fg))))
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,r+))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,fg))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,r+))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,fg))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,r+))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,fg))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,r+))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,fg))))

   ;; Slime
   `(slime-repl-inputed-output-face ((t (:foreground ,c)))))

  (custom-theme-set-variables
   'drybones

   `(evil-normal-state-cursor   '(box ,c-))
   `(evil-insert-state-cursor   '((bar . 3) ,c-))
   `(evil-visual-state-cursor   '(box ,y))
   `(evil-motion-state-cursor   '(box ,c-))
   `(evil-replace-state-cursor  '(box ,r++))
   `(evil-operator-state-cursor '(box ,r++))

   `(ansi-color-names-vector '[,bg ,r ,g ,y ,b ,m ,c ,fg])))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
(file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'drybones)
