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
       mode-line-misc-info
       ;; evil-mode-line-tag
       mode-line-end-spaces))

(setq mode-line-format law--mode-line-format) ;; DEBUG
(setq-default mode-line-format law--mode-line-format)

(provide 'law-mode-line)
