(setq law--mode-line-format
      (list
       mode-line-front-space
       mode-line-mule-info
       mode-line-client
       mode-line-modified
       mode-line-remote
       mode-line-frame-identification
       mode-line-buffer-identification
       "   "
       mode-line-position
       "   "
       mode-line-modes
       "   "
       '(vc-mode vc-mode)
       mode-line-misc-info
       ;; evil-mode-line-tag
       mode-line-end-spaces))

;; NOTE(law): Using both setq and setq-default here so changes are visible
;; immediately upon eval-ing this buffer.

(setq mode-line-format law--mode-line-format)
(setq-default mode-line-format law--mode-line-format)


(provide 'law-mode-line)
