;;; early-init.el --- Emacs early-init.el            -*- lexical-binding: t; -*-

;; 10GB GC threshold during init
(setq gc-cons-threshold (* 10000 1024 1024))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Load time: %s (with %d garbage collections)."
                     (format "%.2f seconds"
                             (float-time (time-subtract after-init-time
                                                        before-init-time)))
                     gcs-done)))

(setq frame-inhibit-implied-resize t)
(setq inhibit-startup-screen t)
(setq custom-safe-themes t)

(require 'package)
(if (fboundp 'native-compile)
    (setq package-user-dir (concat user-emacs-directory
                                   "elpa-"
                                   emacs-version
                                   "-native"))
  (setq package-user-dir (concat user-emacs-directory
                                 "elpa-"
                                 emacs-version)))

(provide 'early-init)
;;; early-init.el ends here
