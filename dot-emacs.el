;; NOTE(law): Change file name to .emacs and copy to root directory. On windows,
;; subst path to emacs directory (.emacs.d) with e:/ drive

(setq law-osx   (eq system-type 'darwin)
      law-win32 (eq system-type 'windows-nt)
      law-linux (eq system-type 'gnu/linux))

(cond ((eq law-osx   t) (setq user-emacs-directory "~/Dropbox/.emacs.d"))
      ((eq law-linux t) (setq user-emacs-directory "~/Dropbox/.emacs.d"))
      ((eq law-win32 t) (setq user-emacs-directory "e:/")))

(setq user-init-file (concat user-emacs-directory "init.el"))

(load user-init-file)
