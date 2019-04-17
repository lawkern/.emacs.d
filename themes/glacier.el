(unless (>= emacs-major-version 24)
  (error "Requires Emacs >=24"))

(defmacro glacier-with-colors (variant &rest body)
  (declare (indent defun))
  `(let* ((class '((class color) (min-colors 89)))
          (light-class (append '((background light)) class))
          (dark-class (append '((background dark)) class))
          (variant ,variant)

          (ice++ "#88b0dd")
          ;; (ice++ "#7ea4ce")
          (ice+ "#7098c1")

          (ice "#5980a6")
          (ice- "#4d6d8f")
          (ice-- "#3e5974")

          (shadow+ "#233343")
          (shadow "#192430")
          (shadow- "#101822")

          (blue+ "#b3edff")
          ;; (blue+ "#7a8ab8")
          (blue "#6ee2ff")
          ;; (blue- "#54a7be")
          (blue- "#478fa4")
          ;; (blue- "#24748f")
          ;; (blue- "#0086b3")

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

(provide 'glacier)
