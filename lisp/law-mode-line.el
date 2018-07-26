(defface mode-line-path
  '((t (:foreground "grey")))
  "Path before buffer name on the mode line"
  :group 'basic-faces)

(defface mode-line-buffer-name
  '((t (:foreground "black")))
  "Current buffer name on the mode line"
  :group 'basic-faces)

(defface mode-line-read-only
  '((t (:foreground "grey")))
  "Read-only indicator on mode line"
  :group 'basic-faces)

(defface mode-line-highlight
  '((t (:foreground "green")))
  "Emphasized text on mode line"
  :group 'basic-faces)

(defface mode-line-warning
  '((t (:foreground "red")))
  "Warning text on mode line"
  :group 'basic-faces)

(defface mode-line-insert
  '((t (:foreground "blue")))
  "VI Insert Mode indicator on mode line"
  :group 'basic-faces)

(defface mode-line-normal
  '((t (:foreground "grey")))
  "VI Normal Mode indicator on mode line"
  :group 'basic-faces)

(defface mode-line-visual
  '((t (:foreground "green")))
  "VI Visual Mode indicator on mode line"
  :group 'basic-faces)

(setq law--mode-line-buffer-name
      (list
       ;; (when (display-graphic-p)
       ;;   '(:eval (propertize (shorten-directory default-directory 15)
       ;;                       'face 'mode-line-path)))
       '(:eval (propertize "%b" 'face 'mode-line-buffer-name
                           'help-echo (buffer-file-name)))))
(setq law--mode-line-size-info
      (list "Size:"
            (propertize "%IB" 'face 'mode-line-highlight)))

(setq law--mode-line-major
      (list "Major:"
            '(:eval
              (propertize "%m" 'face 'mode-line-highlight
                          'help-echo buffer-file-coding-system))))

(setq law--mode-line-minor ; the current minor modes for the buffer.
      (list "Minor:" minor-mode-alist))

(setq law--mode-line-evil-mode
      (list '(:eval
              (propertize
               (symbol-name evil-state)
               'face
               (cond ((eq evil-state 'insert) 'mode-line-insert)
                     ((eq evil-state 'normal) 'mode-line-normal)
                     ((eq evil-state 'visual) 'mode-line-visual)
                     (t 'mode-line-warning))))))

(setq law--mode-line-time-save;; was this buffer modified since the last save?
      (list '(:eval
              (when (buffer-modified-p)
                (propertize "*" 'face 'mode-line-highlight)))))


(setq law--mode-line-read-only ; is this buffer read-only?
      (list '(:eval (when buffer-read-only
                      (propertize " RO" 'face 'mode-line-read-only
                                  'help-echo "Buffer is read-only")))))

(setq law--mode-line-datetime
      (list '(:eval (propertize (format-time-string "%l:%M%p ")
                                'help-echo
                                (concat (format-time-string "%c; ")
                                        (emacs-uptime "Uptime:%hh"))))))

(setq law--mode-line-cursor-pos ; position, including warning for 80 columns
      (list '(:eval "%l:")
            '(:eval
              (propertize "%c" 'face
                          (if (> (current-column) fill-column)
                              'mode-line-warning
                            'mode-line-highlight)))))

(setq law--mode-line-height-adjust ; fixes uneven consolas font
      (propertize "\u200b" 'display '((raise -0.2) (height 1.25))))

(setq law--mode-line-format
      (list " "
           ;; law--mode-line-height-adjust
            law--mode-line-buffer-name
            law--mode-line-time-save
            law--mode-line-read-only
            " -- "
            "[" law--mode-line-major "]"
            " "
            "[" law--mode-line-evil-mode "]"
            " -- "
            ;; "[" law--mode-line-size-info "]"
            ;; " -- "
            ;; law--mode-line-datetime
            "[" law--mode-line-cursor-pos "]"))

(setq mode-line-format law--mode-line-format) ;; DEBUG
(setq-default mode-line-format law--mode-line-format)

(provide 'law-mode-line)
